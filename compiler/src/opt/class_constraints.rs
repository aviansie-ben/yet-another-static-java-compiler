use std::collections::{BTreeMap, HashMap, HashSet, VecDeque};
use std::iter::FromIterator;

use itertools::Itertools;

use crate::log_writeln;
use crate::log::Log;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::{ClassEnvironment, ClassId};

pub struct ClassConstraints {
    vtables: HashMap<MilRegister, MilOperand>,
    global: HashMap<MilRegister, MilClassConstraint>,
    blocks: HashMap<MilBlockId, BTreeMap<MilRegister, MilClassConstraint>>
}

impl ClassConstraints {
    pub fn new() -> ClassConstraints {
        ClassConstraints {
            vtables: HashMap::new(),
            global: HashMap::new(),
            blocks: HashMap::new()
        }
    }

    pub fn at_block(&self, block_id: MilBlockId) -> BlockClassConstraints {
        BlockClassConstraints(self.blocks.get(&block_id), &self.global, &self.vtables)
    }

    pub fn at_block_mut(&mut self, block_id: MilBlockId) -> BlockClassConstraintsMut {
        BlockClassConstraintsMut(self.blocks.entry(block_id).or_insert_with(BTreeMap::new), &self.global, &self.vtables)
    }

    pub fn find(&self, reg: MilRegister) -> Option<MilClassConstraint> {
        self.global.get(&reg).copied()
    }

    pub fn set(&mut self, reg: MilRegister, constraint: MilClassConstraint) -> bool {
        self.global.insert(reg, constraint) != Some(constraint)
    }

    pub fn set_vtable_of(&mut self, reg: MilRegister, obj: MilOperand) {
        self.vtables.insert(reg, obj);
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BlockClassConstraints<'a>(Option<&'a BTreeMap<MilRegister, MilClassConstraint>>, &'a HashMap<MilRegister, MilClassConstraint>, &'a HashMap<MilRegister, MilOperand>);

impl <'a> BlockClassConstraints<'a> {
    pub fn find(&self, reg: MilRegister) -> Option<MilClassConstraint> {
        if let Some(&block_constraint) = self.0.and_then(|c| c.get(&reg)) {
            Some(block_constraint)
        } else {
            self.1.get(&reg).copied()
        }
    }

    pub fn find_operand(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        match *val {
            MilOperand::Register(reg) => self.find(reg),
            MilOperand::KnownObject(_, class_id) => Some(MilClassConstraint::for_class(class_id).not_null().exact()),
            MilOperand::RefNull => Some(MilClassConstraint::null()),
            _ => None
        }
    }

    pub fn find_vtable_of(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        let val = match *val {
            MilOperand::Register(reg) => self.2.get(&reg),
            _ => None
        };

        val.and_then(|val| self.find_operand(&val))
    }

    pub fn with_edge_constraints<'b>(self, edge_constraints: &'b BTreeMap<MilRegister, MilClassConstraint>) -> BlockWithEdgeClassConstraints<'b> where 'a: 'b {
        BlockWithEdgeClassConstraints(edge_constraints, self)
    }

    pub fn remove_unneeded_edge_constraints(&self, edge_constraints: &mut BTreeMap<MilRegister, MilClassConstraint>) {
        edge_constraints.drain_filter(|&reg, &mut edge_constraint| self.find(reg) == Some(edge_constraint));
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BlockWithEdgeClassConstraints<'a>(&'a BTreeMap<MilRegister, MilClassConstraint>, BlockClassConstraints<'a>);

impl <'a> BlockWithEdgeClassConstraints<'a> {
    pub fn find(&self, reg: MilRegister) -> Option<MilClassConstraint> {
        if let Some(&edge_constraint) = self.0.get(&reg) {
            Some(edge_constraint)
        } else {
            self.1.find(reg)
        }
    }

    pub fn find_operand(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        match *val {
            MilOperand::Register(reg) => self.find(reg),
            _ => self.1.find_operand(val)
        }
    }

    pub fn find_vtable_of(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        let val = match *val {
            MilOperand::Register(reg) => (self.1).2.get(&reg),
            _ => None
        };

        val.and_then(|val| self.find_operand(&val))
    }

    pub fn union_into(&self, result: &mut BTreeMap<MilRegister, MilClassConstraint>, env: &ClassEnvironment) {
        for (&reg, &edge_constraint) in self.0.iter() {
            result.entry(reg)
                .and_modify(|old_constraint| {
                    *old_constraint = MilClassConstraint::union(*old_constraint, edge_constraint, env);
                })
                .or_insert(edge_constraint);
        };

        for (&reg, &block_constraint) in (self.1).0.iter().flat_map(|c| c.iter()) {
            if !self.0.contains_key(&reg) {
                result.entry(reg)
                    .and_modify(|old_constraint| {
                        *old_constraint = MilClassConstraint::union(*old_constraint, block_constraint, env);
                    })
                    .or_insert(block_constraint);
            };
        };
    }
}

#[derive(Debug)]
pub struct BlockClassConstraintsMut<'a>(&'a mut BTreeMap<MilRegister, MilClassConstraint>, &'a HashMap<MilRegister, MilClassConstraint>, &'a HashMap<MilRegister, MilOperand>);

impl BlockClassConstraintsMut<'_> {
    pub fn as_ref(&self) -> BlockClassConstraints {
        BlockClassConstraints(Some(self.0), self.1, self.2)
    }

    pub fn find(&self, reg: MilRegister) -> Option<MilClassConstraint> {
        self.as_ref().find(reg)
    }

    pub fn find_operand(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        self.as_ref().find_operand(val)
    }

    pub fn find_vtable_of(&self, val: &MilOperand) -> Option<MilClassConstraint> {
        let val = match *val {
            MilOperand::Register(reg) => self.2.get(&reg),
            _ => None
        };

        val.and_then(|val| self.find_operand(&val))
    }

    pub fn set(&mut self, reg: MilRegister, constraint: MilClassConstraint) -> bool {
        if let Some(existing_constraint) = self.0.get_mut(&reg) {
            std::mem::replace(existing_constraint, constraint) != constraint
        } else {
            self.0.insert(reg, constraint);
            true
        }
    }

    pub fn intersect(&mut self, reg: MilRegister, constraint: MilClassConstraint, env: &ClassEnvironment) -> bool {
        if let Some(existing_constraint) = self.0.get_mut(&reg) {
            std::mem::replace(existing_constraint, MilClassConstraint::intersection(*existing_constraint, constraint, env)) != constraint
        } else if let Some(existing_constraint) = self.1.get(&reg).copied() {
            if existing_constraint != constraint {
                self.0.insert(reg, MilClassConstraint::intersection(existing_constraint, constraint, env));
                true
            } else {
                false
            }
        } else {
            self.0.insert(reg, constraint);
            true
        }
    }
}

fn class_constraint_for_instr(instr: &MilInstructionKind) -> Option<MilClassConstraint> {
    match *instr {
        MilInstructionKind::Nop => None,
        // Copies should already be eliminated by now, so no need to handle them
        MilInstructionKind::Copy(_, _) => None,
        MilInstructionKind::UnOp(_, _, _) => None,
        MilInstructionKind::BinOp(_, _, _, _) => None,
        MilInstructionKind::GetParam(_, constraint, _) => Some(constraint),
        MilInstructionKind::GetLocal(_, _) => None,
        MilInstructionKind::SetLocal(_, _) => None,
        MilInstructionKind::GetField(_, class_id, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilInstructionKind::PutField(_, _, _, _) => None,
        MilInstructionKind::GetArrayLength(_, _) => None,
        MilInstructionKind::GetArrayElement(class_id, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilInstructionKind::PutArrayElement(_, _, _, _) => None,
        MilInstructionKind::GetStatic(_, class_id, _) => Some(MilClassConstraint::for_class(class_id)),
        MilInstructionKind::PutStatic(_, _, _) => None,
        MilInstructionKind::AllocObj(class_id, _) => Some(MilClassConstraint::for_class(class_id).not_null().exact()),
        MilInstructionKind::AllocArray(class_id, _, _) => Some(MilClassConstraint::for_class(class_id).not_null().exact()),
        MilInstructionKind::GetVTable(_, _) => None
    }
}

fn class_constraint_for_end_instr(instr: &MilEndInstructionKind) -> Option<MilClassConstraint> {
    match *instr {
        MilEndInstructionKind::Nop => None,
        MilEndInstructionKind::Unreachable => None,
        MilEndInstructionKind::Call(class_id, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilEndInstructionKind::CallVirtual(class_id, _, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilEndInstructionKind::CallInterface(class_id, _, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilEndInstructionKind::CallNative(class_id, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
        MilEndInstructionKind::Throw(_) => None,
        MilEndInstructionKind::Return(_) => None,
        MilEndInstructionKind::Jump(_) => None,
        MilEndInstructionKind::JumpIfRCmp(_, _, _, _) => None,
        MilEndInstructionKind::JumpIfICmp(_, _, _, _) => None
    }
}

pub fn perform_class_constraint_analysis(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== CLASS CONSTRAINT ANALYSIS =====\n");

    let mut num_changes = 0;
    let mut constraints = ClassConstraints::new();
    let mut edge_constraints = HashMap::new();
    let mut killed = HashMap::new();

    log_writeln!(log, "Collecting initial constraints...");
    for (block_id, next_block_id) in func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = &func.blocks[&block_id];
        let killed = killed.entry(block_id).or_insert_with(HashSet::new);

        for phi in block.phi_nodes.iter() {
            killed.insert(phi.target);
        };

        for instr in block.instrs.iter() {
            if let Some(tgt) = instr.target().copied() {
                killed.insert(tgt);

                if let Some(constraint) = class_constraint_for_instr(&instr.kind) {
                    if tgt != MilRegister::VOID && !constraint.class_id().is_primitive_type() {
                        log_writeln!(log, "  {} <- {}", tgt, constraint.pretty(env));
                        constraints.set(tgt, constraint);
                    };
                };
            };

            match instr.kind {
                MilInstructionKind::GetVTable(tgt, ref obj) => {
                    log_writeln!(log, "  {} <- vtable of {}", tgt, obj.pretty(env));
                    constraints.set_vtable_of(tgt, obj.clone());
                },
                _ => {}
            }
        };

        if let Some(tgt) = block.end_instr.target().copied() {
            killed.insert(tgt);

            if let Some(constraint) = class_constraint_for_end_instr(&block.end_instr.kind) {
                let tgt = block.end_instr.target().cloned().unwrap();

                if tgt != MilRegister::VOID && !constraint.class_id().is_primitive_type() {
                    log_writeln!(log, "  {} <- {}", tgt, constraint.pretty(env));
                    constraints.set(tgt, constraint);
                };
            };
        };

        match block.end_instr.kind {
            MilEndInstructionKind::JumpIfRCmp(MilRefComparison::Eq, target_block_id, MilOperand::Register(lhs), MilOperand::RefNull) => {
                log_writeln!(log, "  [{} -> {}] {} <- {}", block_id, target_block_id, lhs, MilClassConstraint::null().pretty(env));
                log_writeln!(log, "  [{} -> {}] {} <- {}", block_id, next_block_id, lhs, MilClassConstraint::non_null().pretty(env));

                edge_constraints.entry((block_id, target_block_id)).or_insert_with(BTreeMap::new).insert(lhs, MilClassConstraint::null());
                edge_constraints.entry((block_id, next_block_id)).or_insert_with(BTreeMap::new).insert(lhs, MilClassConstraint::non_null());
            },
            MilEndInstructionKind::JumpIfRCmp(MilRefComparison::Ne, target_block_id, MilOperand::Register(lhs), MilOperand::RefNull) => {
                log_writeln!(log, "  [{} -> {}] {} <- {}", block_id, target_block_id, lhs, MilClassConstraint::non_null().pretty(env));
                log_writeln!(log, "  [{} -> {}] {} <- {}", block_id, next_block_id, lhs, MilClassConstraint::null().pretty(env));

                edge_constraints.entry((block_id, target_block_id)).or_insert_with(BTreeMap::new).insert(lhs, MilClassConstraint::non_null());
                edge_constraints.entry((block_id, next_block_id)).or_insert_with(BTreeMap::new).insert(lhs, MilClassConstraint::null());
            },
            _ => {}
        };
    };

    log_writeln!(log, "Updating edge constraints...");
    for (&(from, to), edge_constraints) in edge_constraints.iter_mut() {
        let block_constraints = constraints.at_block(from);
        for (&reg, edge_constraint) in edge_constraints.iter_mut() {
            if let Some(constraint) = block_constraints.find(reg) {
                *edge_constraint = MilClassConstraint::intersection(*edge_constraint, constraint, env);
                log_writeln!(log, "  [{} -> {}] {} <- {}", from, to, reg, edge_constraint.pretty(env));
            };
        };

        block_constraints.remove_unneeded_edge_constraints(edge_constraints);
    };

    let empty_constraints = BTreeMap::new();

    let mut worklist = VecDeque::from_iter(func.block_order.iter().copied());

    log_writeln!(log, "Performing constraint dataflow analysis...");
    while let Some(block_id) = worklist.pop_front() {
        log_writeln!(log, "  {}:", block_id);

        let cfg_node = cfg.get(block_id);
        let block = &func.blocks[&block_id];
        let killed = &killed[&block_id];

        let mut block_constraints_owned = constraints.at_block(block_id).0.unwrap_or(&empty_constraints).clone();
        let mut block_constraints = BlockClassConstraintsMut(&mut block_constraints_owned, &constraints.global, &constraints.vtables);
        let mut changed = false;

        // First, gather constraints from incoming edges and figure out what registers need to be updated based on this
        // information.
        let mut incoming_constraints = BTreeMap::new();
        for &pred in cfg_node.incoming.iter() {
            let this_pred_constraints = constraints.at_block(pred).with_edge_constraints(
                edge_constraints.get(&(pred, block_id)).unwrap_or(&empty_constraints)
            );

            this_pred_constraints.union_into(&mut incoming_constraints, env);
        };

        // Next, intersect the unioned incoming edge constraints with the current block's constraints.
        for (reg, constraint) in incoming_constraints.into_iter() {
            if !killed.contains(&reg) && block_constraints.intersect(reg, constraint, env) {
                log_writeln!(log, "    {} <- {}", reg, block_constraints.find(reg).unwrap().pretty(env));
                changed = true;
            };
        };

        // Finally, compute the class constraints on all phi nodes by using only the values from the particular
        // incoming edge of each source.
        for phi in block.phi_nodes.iter() {
            let mut constraint = None;

            for &(ref src, pred) in phi.sources.iter() {
                let this_pred_constraints = constraints.at_block(pred).with_edge_constraints(
                    edge_constraints.get(&(pred, block_id)).unwrap_or(&empty_constraints)
                );

                if let Some(src_constraint) = this_pred_constraints.find_operand(src) {
                    constraint = Some(if let Some(constraint) = constraint {
                        MilClassConstraint::union(constraint, src_constraint, env)
                    } else {
                        src_constraint
                    });
                } else {
                    constraint = None;
                    break;
                };
            };

            if let Some(constraint) = constraint {
                if block_constraints.set(phi.target, constraint) {
                    log_writeln!(log, "    {} <- {}", phi.target, block_constraints.find(phi.target).unwrap().pretty(env));
                    changed = true;
                };
            };
        };

        if changed {
            log_writeln!(log, "    Constraint changed detected. Updating edge constraints...");

            for &succ in cfg_node.outgoing.iter() {
                if succ != MilBlockId::EXIT {
                    if let Some(this_edge_constraints) = edge_constraints.get_mut(&(block_id, succ)) {
                        for (&reg, constraint) in this_edge_constraints.iter_mut() {
                            if let Some(block_constraint) = block_constraints.find(reg) {
                                *constraint = MilClassConstraint::intersection(*constraint, block_constraint, env);
                                log_writeln!(log, "      [{} -> {}] {} <- {}", block_id, succ, reg, constraint.pretty(env));
                            };
                        };

                        block_constraints.as_ref().remove_unneeded_edge_constraints(this_edge_constraints);
                    };

                    if !worklist.contains(&succ) {
                        worklist.push_back(succ);
                    };
                };
            };

            constraints.blocks.insert(block_id, block_constraints_owned);
        };
    };

    log_writeln!(log, "Performing optimizations...");
    for block_id in func.block_order.iter().cloned() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        let block_constraints = constraints.at_block(block_id);

        for instr in block.instrs.iter_mut() {
            instr.for_operands_mut(|op| if let Some(constraint) = block_constraints.find_operand(op) {
                if op != &MilOperand::RefNull && constraint.nullable() && constraint.class_id() == ClassId::UNRESOLVED {
                    log_writeln!(log, "  Replacing {} in {} with ref:null", op.pretty(env), block_id);
                    *op = MilOperand::RefNull;
                };
            });
        };

        block.end_instr.for_operands_mut(|op| if let Some(constraint) = block_constraints.find_operand(op) {
            if op != &MilOperand::RefNull && constraint.nullable() && constraint.class_id() == ClassId::UNRESOLVED {
                log_writeln!(log, "  Replacing {} in {} with ref:null", op.pretty(env), block_id);
                *op = MilOperand::RefNull;
            };
        });

        match block.end_instr.kind {
            MilEndInstructionKind::JumpIfRCmp(cmp, target_block, ref val, MilOperand::RefNull)
            | MilEndInstructionKind::JumpIfRCmp(cmp, target_block, MilOperand::RefNull, ref val) => {
                if let Some(constraint) = block_constraints.find_operand(val) {
                    if !constraint.nullable() {
                        log_writeln!(log, "  Folding conditional at end of {} since {} is never null", block_id, val.pretty(env));
                        block.end_instr.kind = MilEndInstructionKind::JumpIfRCmp(cmp.reverse(), target_block, MilOperand::RefNull, MilOperand::RefNull);
                        num_changes += 1;
                    };
                };
            },
            MilEndInstructionKind::CallInterface(return_class_id, method_id, tgt, ref vtable, ref args) => {
                if let Some(constraint) = block_constraints.find_vtable_of(vtable) {
                    if constraint.class_id() != ClassId::UNRESOLVED {
                        let method = env.get_method(method_id).1;

                        for class_id in env.get_class_chain(constraint.class_id()) {
                            if let Some(overrider_id) = method.overrides.overridden_by.iter().find(|&overrider_id| overrider_id.0 == class_id).copied() {
                                log_writeln!(log, "  Replacing interface call to {} in {} with virtual call to {}", MethodName(method_id, env), block_id, MethodName(overrider_id, env));
                                block.end_instr.kind = MilEndInstructionKind::CallVirtual(return_class_id, overrider_id, tgt, vtable.clone(), args.clone());
                                break;
                            };
                        };
                    };
                };
            },
            MilEndInstructionKind::CallVirtual(return_class_id, method_id, tgt, ref vtable, ref args) => {
                if let Some(constraint) = block_constraints.find_vtable_of(vtable) {
                    if constraint.class_id() != ClassId::UNRESOLVED {
                        let method = env.get_method(method_id).1;

                        for class_id in env.get_class_chain(constraint.class_id()) {
                            if let Some(overrider_id) = method.overrides.overridden_by.iter().find(|&overrider_id| overrider_id.0 == class_id).copied() {
                                log_writeln!(log, "  Replacing virtual call to {} in {} with virtual call to {}", MethodName(method_id, env), block_id, MethodName(overrider_id, env));
                                block.end_instr.kind = MilEndInstructionKind::CallVirtual(return_class_id, overrider_id, tgt, vtable.clone(), args.clone());
                                break;
                            };
                        };
                    };
                };
            },
            _ => {}
        };

        match block.end_instr.kind {
            MilEndInstructionKind::CallVirtual(return_class_id, method_id, tgt, ref vtable, ref args) => {
                if let Some(constraint) = block_constraints.find_vtable_of(vtable) {
                    if constraint.is_exact() {
                        log_writeln!(log, "  Devirtualizing call to {} in {} since receiver type is known to be exact", MethodName(method_id, env), block_id);
                        block.end_instr.kind = MilEndInstructionKind::Call(return_class_id, method_id, tgt, args.clone());
                    };
                };
            },
            _ => {}
        };
    };

    if num_changes != 0 {
        log_writeln!(log, "\n===== AFTER CLASS CONSTRAINT ANALYSIS =====\n\n{}", func.pretty(env));
    };

    num_changes
}
