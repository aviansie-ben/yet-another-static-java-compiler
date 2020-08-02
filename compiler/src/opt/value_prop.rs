use std::cmp::Ordering;
use std::collections::{HashMap, VecDeque};
use std::iter::FromIterator;

use itertools::Itertools;
use smallvec::SmallVec;

use crate::classfile::FieldFlags;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::mil::transform;
use crate::resolve::{ClassEnvironment, ClassId, ResolvedClass};

fn const_compare<T: Ord>(cmp: MilIntComparison, lhs: T, rhs: T) -> bool {
    match cmp {
        MilIntComparison::Eq => lhs == rhs,
        MilIntComparison::Ne => lhs != rhs,
        MilIntComparison::Gt => lhs > rhs,
        MilIntComparison::Lt => lhs < rhs,
        MilIntComparison::Ge => lhs >= rhs,
        MilIntComparison::Le => lhs <= rhs
    }
}

fn try_fold_constant_instr(instr: &MilInstructionKind, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    Some(match *instr {
        MilInstructionKind::Copy(_, ref val) => val.clone(),
        MilInstructionKind::UnOp(op, _, MilOperand::Int(val)) => match op {
            MilUnOp::INeg => MilOperand::Int(val.wrapping_neg()),
            MilUnOp::IExtB => MilOperand::Int(val as i8 as i32),
            MilUnOp::IExtS => MilOperand::Int(val as i16 as i32),
            MilUnOp::I2L => MilOperand::Long(val as i64),
            MilUnOp::I2F => MilOperand::Float((val as f32).to_bits()),
            MilUnOp::I2D => MilOperand::Double((val as f64).to_bits()),
            _ => unreachable!()
        },
        MilInstructionKind::UnOp(op, _, MilOperand::Long(val)) => match op {
            MilUnOp::LNeg => MilOperand::Long(val.wrapping_neg()),
            MilUnOp::L2I => MilOperand::Int(val as i32),
            MilUnOp::L2F => MilOperand::Float((val as f32).to_bits()),
            MilUnOp::L2D => MilOperand::Double((val as f64).to_bits()),
            _ => unreachable!()
        },
        MilInstructionKind::UnOp(op, _, MilOperand::Float(val_bits)) => {
            let val = f32::from_bits(val_bits);
            match op {
                MilUnOp::FNeg => MilOperand::Float((-val).to_bits()),
                MilUnOp::F2I => MilOperand::Int(
                    if val.is_nan() {
                        0
                    } else if val >= (i32::MAX as f32) {
                        i32::MAX
                    } else if val <= (i32::MIN as f32) {
                        i32::MIN
                    } else {
                        val as i32
                    }
                ),
                MilUnOp::F2L => MilOperand::Long(
                    if val.is_nan() {
                        0
                    } else if val >= (i64::MAX as f32) {
                        i64::MAX
                    } else if val <= (i64::MIN as f32) {
                        i64::MIN
                    } else {
                        val as i64
                    }
                ),
                MilUnOp::F2D => MilOperand::Double((val as f64).to_bits()),
                _ => unreachable!()
            }
        }
        MilInstructionKind::UnOp(op, _, MilOperand::Double(val_bits)) => {
            let val = f64::from_bits(val_bits);
            match op {
                MilUnOp::DNeg => MilOperand::Double((-val).to_bits()),
                MilUnOp::D2I => MilOperand::Int(
                    if val.is_nan() {
                        0
                    } else if val >= (i32::MAX as f64) {
                        i32::MAX
                    } else if val <= (i32::MIN as f64) {
                        i32::MIN
                    } else {
                        val as i32
                    }
                ),
                MilUnOp::D2L => MilOperand::Long(
                    if val.is_nan() {
                        0
                    } else if val >= (i64::MAX as f64) {
                        i64::MAX
                    } else if val <= (i64::MIN as f64) {
                        i64::MIN
                    } else {
                        val as i64
                    }
                ),
                MilUnOp::D2F => MilOperand::Float((val as f32).to_bits()),
                _ => unreachable!()
            }
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Int(lhs), MilOperand::Int(rhs)) => match op {
            MilBinOp::IAdd => MilOperand::Int(lhs.wrapping_add(rhs)),
            MilBinOp::ISub => MilOperand::Int(lhs.wrapping_sub(rhs)),
            MilBinOp::IMul => MilOperand::Int(lhs.wrapping_mul(rhs)),
            MilBinOp::IDivS | MilBinOp::IRemS if rhs == 0 => {
                return None;
            },
            MilBinOp::IDivS => MilOperand::Int(lhs.wrapping_div(rhs)),
            MilBinOp::IRemS => MilOperand::Int(lhs.wrapping_rem(rhs)),
            MilBinOp::IAnd => MilOperand::Int(lhs & rhs),
            MilBinOp::IOr => MilOperand::Int(lhs | rhs),
            MilBinOp::IXor => MilOperand::Int(lhs ^ rhs),
            MilBinOp::IShrS => MilOperand::Int(lhs >> (rhs & 0x1f)),
            MilBinOp::IShrU => MilOperand::Int(((lhs as u32) >> (rhs & 0x1f)) as i32),
            MilBinOp::IShl => MilOperand::Int(lhs << (rhs & 0x1f)),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Long(lhs), MilOperand::Int(rhs)) => match op {
            MilBinOp::LShrS => MilOperand::Long(lhs >> (rhs & 0x3f)),
            MilBinOp::LShrU => MilOperand::Long(((lhs as u64) >> (rhs & 0x3f)) as i64),
            MilBinOp::LShl => MilOperand::Long(lhs << (rhs & 0x3f)),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Long(lhs), MilOperand::Long(rhs)) => match op {
            MilBinOp::LAdd => MilOperand::Long(lhs.wrapping_add(rhs)),
            MilBinOp::LSub => MilOperand::Long(lhs.wrapping_sub(rhs)),
            MilBinOp::LMul => MilOperand::Long(lhs.wrapping_mul(rhs)),
            MilBinOp::LDivS | MilBinOp::LRemS if rhs == 0 => {
                return None;
            },
            MilBinOp::LDivS => MilOperand::Long(lhs.wrapping_div(rhs)),
            MilBinOp::LRemS => MilOperand::Long(lhs.wrapping_rem(rhs)),
            MilBinOp::LAnd => MilOperand::Long(lhs & rhs),
            MilBinOp::LOr => MilOperand::Long(lhs | rhs),
            MilBinOp::LXor => MilOperand::Long(lhs ^ rhs),
            MilBinOp::LCmp => MilOperand::Int(match lhs.cmp(&rhs) {
                Ordering::Less => -1,
                Ordering::Equal => 0,
                Ordering::Greater => 1
            }),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Float(lhs_bits), MilOperand::Float(rhs_bits)) => {
            let lhs = f32::from_bits(lhs_bits);
            let rhs = f32::from_bits(rhs_bits);

            match op {
                MilBinOp::FAdd => MilOperand::Float((lhs + rhs).to_bits()),
                MilBinOp::FSub => MilOperand::Float((lhs - rhs).to_bits()),
                MilBinOp::FMul => MilOperand::Float((lhs * rhs).to_bits()),
                MilBinOp::FDiv => MilOperand::Float((lhs / rhs).to_bits()),
                _ => unreachable!()
            }
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Double(lhs_bits), MilOperand::Double(rhs_bits)) => {
            let lhs = f64::from_bits(lhs_bits);
            let rhs = f64::from_bits(rhs_bits);

            match op {
                MilBinOp::DAdd => MilOperand::Double((lhs + rhs).to_bits()),
                MilBinOp::DSub => MilOperand::Double((lhs - rhs).to_bits()),
                MilBinOp::DMul => MilOperand::Double((lhs * rhs).to_bits()),
                MilBinOp::DDiv => MilOperand::Double((lhs / rhs).to_bits()),
                _ => unreachable!()
            }
        },
        MilInstructionKind::GetField(field_id, _, _, MilOperand::KnownObject(val, _)) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                MilOperand::from_const(known_objects.get(val).read_field(field_id), known_objects)
            } else {
                return None;
            }
        },
        MilInstructionKind::GetArrayLength(_, MilOperand::KnownObject(val, _)) => {
            let val = known_objects.get(val);

            if matches!(val.class(), ResolvedClass::Array(_)) {
                MilOperand::Int(val.read_array_length())
            } else {
                return None;
            }
        },
        MilInstructionKind::GetStatic(field_id, _, _) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                MilOperand::from_const(
                    known_objects.get(known_objects.refs.classes[&field_id.0]).read_field(field_id),
                    known_objects
                )
            } else {
                return None;
            }
        },
        _ => {
            return None;
        }
    })
}

pub fn fold_constant_exprs(func: &mut MilFunction, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> usize {
    eprintln!("\n===== CONSTANT/COPY FOLDING =====\n");

    let mut num_folded = 0;
    let mut constants = HashMap::new();

    loop {
        for block_id in func.block_order.iter().copied() {
            let block = func.blocks.get_mut(&block_id).unwrap();

            block.instrs.drain_filter(|instr| {
                if let Some(val) = try_fold_constant_instr(&instr.kind, env, known_objects) {
                    let val = if let MilOperand::Register(reg) = val {
                        constants.get(&reg).cloned().unwrap_or(MilOperand::Register(reg))
                    } else {
                        val
                    };

                    eprintln!("Replacing {} with {}", instr.target().unwrap(), val.pretty(env));
                    constants.insert(*instr.target().unwrap(), val);
                    true
                } else {
                    false
                }
            });
        };

        if !constants.is_empty() {
            num_folded += constants.len();
            for block_id in func.block_order.iter().copied() {
                transform::replace_register(func.blocks.get_mut(&block_id).unwrap(), &constants);
            };
            constants.clear();
        } else {
            break;
        };
    };

    if num_folded != 0 {
        eprintln!("\n===== AFTER CONSTANT/COPY FOLDING =====\n\n{}", func.pretty(env));
    };

    num_folded
}

pub fn transform_locals_into_phis(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment) {
    eprintln!("\n===== LOCAL TO PHI TRANSFORMATION =====\n");

    let locals_len = func.reg_map.local_info.iter().map(|(id, _)| id.0).max().map_or(0, |i| i + 1);
    let local_types = (0..locals_len).map(|i| func.reg_map.local_info.get(&MilLocalId(i)).map_or(MilType::Void, |info| info.ty)).collect_vec();
    let mut local_phis = HashMap::new();
    let mut locals_out = HashMap::new();

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        eprintln!("{}:", block_id);

        let reg_alloc = &mut func.reg_alloc;
        let reg_map = &mut func.reg_map;
        let mut locals = if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            local_types.iter().copied().map(|ty| {
                if ty != MilType::Void {
                    let reg = reg_alloc.allocate_one();
                    reg_map.add_reg_info(reg, MilRegisterInfo { ty });

                    let i = block.phi_nodes.len();
                    block.phi_nodes.push(MilPhiNode {
                        target: reg,
                        sources: SmallVec::new()
                    });

                    (i, MilOperand::Register(reg))
                } else {
                    (!0, MilOperand::Register(MilRegister::VOID))
                }
            }).collect_vec()
        } else {
            (0..locals_len).map(|_| (!0, MilOperand::Register(MilRegister::VOID))).collect_vec()
        };

        eprint!("  Created local phis: [");
        for (_, val) in locals.iter() {
            eprint!(" {}", val.pretty(env));
        };
        eprintln!(" ]");

        for instr in block.instrs.iter_mut() {
            match instr.kind {
                MilInstructionKind::GetLocal(local_id, tgt) => {
                    let val = locals[local_id.0 as usize].1.clone();

                    eprintln!("  Replacing get_local <{}> {} with {}", local_id, tgt, val.pretty(env));
                    instr.kind = MilInstructionKind::Copy(tgt, val);
                },
                MilInstructionKind::SetLocal(local_id, ref val) => {
                    eprintln!("  Found set_local <{}> {}", local_id, val.pretty(env));
                    locals[local_id.0 as usize].1 = val.clone();
                },
                _ => {}
            };
        };

        eprint!("  Final local values: [");
        for (_, val) in locals.iter() {
            eprint!(" {}", val.pretty(env));
        };
        eprintln!(" ]");

        local_phis.insert(block_id, locals.iter().map(|&(i, _)| i).collect_vec());
        locals_out.insert(block_id, locals.into_iter().map(|(_, val)| val).collect_vec());
    };

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            let phis = &local_phis[&block_id];
            let pred_locals = cfg.get(block_id).incoming.iter().copied()
                .map(|pred_id| (pred_id, &locals_out[&pred_id]))
                .collect_vec();

            for (local_idx, phi) in phis.iter().copied().enumerate() {
                if phi != !0 {
                    block.phi_nodes[phi].sources = pred_locals.iter().copied()
                        .map(|(pred_id, pred_locals)| (pred_locals[local_idx].clone(), pred_id))
                        .collect();
                };
            };
        };
    };

    eprintln!("\n===== AFTER LOCAL TO PHI TRANSFORMATION =====\n\n{}", func.pretty(env));
}

fn find_constraint(constraints: &[(MilRegister, MilClassConstraint)], reg: MilRegister) -> Option<MilClassConstraint> {
    constraints.iter().copied().find(|&(creg, _)| creg == reg).map(|(_, constraint)| constraint)
}

fn overwrite_constraint(constraints: &mut Vec<(MilRegister, MilClassConstraint)>, reg: MilRegister, constraint: MilClassConstraint, env: &ClassEnvironment) {
    if let Some(&mut (_, ref mut existing_constraint)) = constraints.iter_mut().find(|&&mut (creg, _)| creg == reg) {
        *existing_constraint = constraint;
    } else {
        constraints.push((reg, constraint));
    }
}

fn intersect_constraint(constraints: &mut Vec<(MilRegister, MilClassConstraint)>, reg: MilRegister, constraint: MilClassConstraint, env: &ClassEnvironment) {
    if let Some(&mut (_, ref mut existing_constraint)) = constraints.iter_mut().find(|&&mut (creg, _)| creg == reg) {
        *existing_constraint = MilClassConstraint::intersection(*existing_constraint, constraint, env);
    } else {
        constraints.push((reg, constraint));
    }
}

fn register_constraint(block_constraints: &[(MilRegister, MilClassConstraint)], constraints: &HashMap<MilRegister, MilClassConstraint>, reg: MilRegister) -> Option<MilClassConstraint> {
    find_constraint(block_constraints, reg).or_else(|| constraints.get(&reg).copied())
}

fn operand_constraint(block_constraints: &[(MilRegister, MilClassConstraint)], constraints: &HashMap<MilRegister, MilClassConstraint>, val: &MilOperand) -> Option<MilClassConstraint> {
    match *val {
        MilOperand::Register(reg) => register_constraint(block_constraints, constraints, reg),
        MilOperand::KnownObject(_, class_id) => Some(MilClassConstraint::for_class(class_id).not_null()),
        MilOperand::Null => Some(MilClassConstraint::null()),
        _ => None
    }
}

pub fn perform_class_constraint_analysis(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment) -> usize {
    eprintln!("\n===== CLASS CONSTRAINT ANALYSIS =====\n");

    let mut num_changes = 0;
    let mut constraints = HashMap::new();
    let mut edge_constraints = HashMap::new();

    eprintln!("Collecting initial constraints...");
    for (block_id, next_block_id) in func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = &func.blocks[&block_id];

        for instr in block.instrs.iter() {
            let constraint = match instr.kind {
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
                MilInstructionKind::AllocObj(class_id, _) => Some(MilClassConstraint::for_class(class_id).not_null()),
                MilInstructionKind::AllocArray(class_id, _, _) => Some(MilClassConstraint::for_class(class_id).not_null())
            };

            if let Some(constraint) = constraint {
                let tgt = instr.target().cloned().unwrap();

                if tgt != MilRegister::VOID && !constraint.class_id().is_primitive_type() {
                    eprintln!("  {} <- {}", tgt, constraint.pretty(env));
                    constraints.insert(tgt, constraint);
                };
            };
        };

        let constraint = match block.end_instr.kind {
            MilEndInstructionKind::Nop => None,
            MilEndInstructionKind::Unreachable => None,
            MilEndInstructionKind::Call(class_id, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
            MilEndInstructionKind::CallVirtual(class_id, _, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
            MilEndInstructionKind::CallInterface(class_id, _, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
            MilEndInstructionKind::CallNative(class_id, _, _, _) => Some(MilClassConstraint::for_class(class_id)),
            MilEndInstructionKind::Throw(_) => None,
            MilEndInstructionKind::Return(_) => None,
            MilEndInstructionKind::Jump(_) => None,
            MilEndInstructionKind::JumpIfRCmp(MilRefComparison::Eq, target_block_id, MilOperand::Register(lhs), MilOperand::Null) => {
                eprintln!("  [{} -> {}] {} <- {}", block_id, target_block_id, lhs, MilClassConstraint::null().pretty(env));
                eprintln!("  [{} -> {}] {} <- {}", block_id, next_block_id, lhs, MilClassConstraint::non_null().pretty(env));

                edge_constraints.entry((block_id, target_block_id)).or_insert_with(Vec::new).push((lhs, MilClassConstraint::null()));
                edge_constraints.entry((block_id, next_block_id)).or_insert_with(Vec::new).push((lhs, MilClassConstraint::non_null()));

                None
            },
            MilEndInstructionKind::JumpIfRCmp(MilRefComparison::Ne, target_block_id, MilOperand::Register(lhs), MilOperand::Null) => {
                eprintln!("  [{} -> {}] {} <- {}", block_id, target_block_id, lhs, MilClassConstraint::non_null().pretty(env));
                eprintln!("  [{} -> {}] {} <- {}", block_id, next_block_id, lhs, MilClassConstraint::null().pretty(env));

                edge_constraints.entry((block_id, target_block_id)).or_insert_with(Vec::new).push((lhs, MilClassConstraint::non_null()));
                edge_constraints.entry((block_id, next_block_id)).or_insert_with(Vec::new).push((lhs, MilClassConstraint::null()));

                None
            },
            MilEndInstructionKind::JumpIfRCmp(_, _, _, _) => None,
            MilEndInstructionKind::JumpIfICmp(_, _, _, _) => None
        };

        if let Some(constraint) = constraint {
            let tgt = block.end_instr.target().cloned().unwrap();

            if tgt != MilRegister::VOID && !constraint.class_id().is_primitive_type() {
                eprintln!("  {} <- {}", tgt, constraint.pretty(env));
                constraints.insert(tgt, constraint);
            };
        };
    };

    eprintln!("Updating edge constraints...");
    for (&(from, to), edge_constraints) in edge_constraints.iter_mut() {
        for &mut (reg, ref mut edge_constraint) in edge_constraints.iter_mut() {
            if let Some(constraint) = constraints.get(&reg).copied() {
                *edge_constraint = MilClassConstraint::intersection(*edge_constraint, constraint, env);
                eprintln!("  [{} -> {}] {} <- {}", from, to, reg, edge_constraint.pretty(env));
            };
        };
    };

    let mut block_constraints: HashMap<MilBlockId, _> = HashMap::new();
    let mut worklist = VecDeque::from_iter(func.block_order.iter().copied());

    eprintln!("Performing constraint dataflow analysis...");
    while let Some(block_id) = worklist.pop_front() {
        eprintln!("  {}:", block_id);

        let cfg_node = cfg.get(block_id);
        let block = &func.blocks[&block_id];

        let mut changed = false;
        let mut this_block_constraints = vec![];
        let mut pred_constraints: SmallVec<[_; 2]> = SmallVec::new();

        for pred in cfg_node.incoming.iter().copied() {
            let this_pred_constraints = if let Some(mut this_pred_constraints) = block_constraints.get(&pred).cloned() {
                if let Some(edge_constraints) = edge_constraints.get(&(pred, block_id)) {
                    for (reg, constraint) in edge_constraints.iter().cloned() {
                        intersect_constraint(&mut this_pred_constraints, reg, constraint, env);
                    };
                };
                this_pred_constraints
            } else {
                edge_constraints.get(&(pred, block_id)).cloned().unwrap_or_else(Vec::new)
            };

            pred_constraints.push((pred, this_pred_constraints));
        };

        for phi in block.phi_nodes.iter() {
            let mut constraint = None;

            for &(ref src, _) in phi.sources.iter() {
                let src_constraint = operand_constraint(&vec![], &constraints, src).unwrap_or(MilClassConstraint::for_class(ClassId::JAVA_LANG_OBJECT));

                constraint = Some(if let Some(constraint) = constraint {
                    MilClassConstraint::union(constraint, src_constraint, env)
                } else {
                    src_constraint
                });
            };

            let constraint = constraint.unwrap();
            if constraint.class_id() != ClassId::JAVA_LANG_OBJECT {
                eprintln!("    {} <- {}", phi.target, constraint.pretty(env));
                overwrite_constraint(&mut this_block_constraints, phi.target, constraint, env);
            };
        };

        // TODO Make this more efficient by keeping lists sorted
        'constraint_merge_loop: for (reg, mut constraint) in pred_constraints[0].1.iter().copied() {
            for &(_, ref pred_constraints) in pred_constraints[1..].iter() {
                if let Some(pred_constraint) = find_constraint(pred_constraints, reg) {
                    constraint = MilClassConstraint::union(constraint, pred_constraint, env);
                } else {
                    continue 'constraint_merge_loop;
                };
            };

            eprintln!("    {} <- {}", reg, constraint.pretty(env));
            overwrite_constraint(&mut this_block_constraints, reg, constraint, env);
        };

        this_block_constraints.sort_by_key(|&(r, _)| r.0);

        let changed = if this_block_constraints.is_empty() {
            false
        } else if let Some(old_block_constraints) = block_constraints.get(&block_id) {
            &this_block_constraints != old_block_constraints
        } else {
            true
        };

        if changed {
            eprintln!("    Constraint changes detected. Updating edge constraints...");
            block_constraints.insert(block_id, this_block_constraints);

            for succ in cfg_node.outgoing.iter().copied() {
                if succ != MilBlockId::EXIT {
                    if let Some(edge_constraints) = edge_constraints.get_mut(&(block_id, succ)) {
                        for &mut (reg, ref mut edge_constraint) in edge_constraints.iter_mut() {
                            if let Some(constraint) = constraints.get(&reg).copied() {
                                *edge_constraint = MilClassConstraint::intersection(*edge_constraint, constraint, env);
                                eprintln!("      [{} -> {}] {} <- {}", block_id, succ, reg, edge_constraint.pretty(env));
                            };
                        };
                    };

                    if !worklist.contains(&succ) {
                        worklist.push_back(succ);
                    };
                };
            };
        };
    };

    let empty_constraints = vec![];

    eprintln!("Performing optimizations...");
    for block_id in func.block_order.iter().cloned() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        let block_constraints = block_constraints.get(&block_id).unwrap_or(&empty_constraints);

        for instr in block.instrs.iter_mut() {
            instr.for_operands_mut(|op| if let MilOperand::Register(reg) = *op {
                if let Some(constraint) = register_constraint(&block_constraints, &constraints, reg) {
                    if constraint.nullable() && constraint.class_id() == ClassId::UNRESOLVED {
                        eprintln!("  Replacing {} in {} with ref:null", reg, block_id);
                        *op = MilOperand::Null;
                    };
                };
            });
        };

        block.end_instr.for_operands_mut(|op| if let MilOperand::Register(reg) = *op {
            if let Some(constraint) = register_constraint(&block_constraints, &constraints, reg) {
                if constraint.nullable() && constraint.class_id() == ClassId::UNRESOLVED {
                    eprintln!("  Replacing {} in {} with ref:null", reg, block_id);
                    *op = MilOperand::Null;
                };
            };
        });

        match block.end_instr.kind {
            MilEndInstructionKind::JumpIfRCmp(cmp, target_block, MilOperand::Register(reg), MilOperand::Null)
                | MilEndInstructionKind::JumpIfRCmp(cmp, target_block, MilOperand::Null, MilOperand::Register(reg)) => {
                if let Some(constraint) = register_constraint(&block_constraints, &constraints, reg) {
                    if !constraint.nullable() {
                        eprintln!("  Folding conditional at end of {} since {} is never null", block_id, reg);
                        block.end_instr.kind = MilEndInstructionKind::JumpIfRCmp(cmp.reverse(), target_block, MilOperand::Null, MilOperand::Null);
                        num_changes += 1;
                    };
                };
            },
            MilEndInstructionKind::CallInterface(return_class_id, method_id, tgt, ref recv, ref args) => {
                if let Some(constraint) = operand_constraint(&block_constraints, &constraints, recv) {
                    if constraint.class_id() != ClassId::UNRESOLVED {
                        let method = env.get_method(method_id).1;

                        for class_id in env.get_class_chain(constraint.class_id()) {
                            if let Some(overrider_id) = method.overrides.overridden_by.iter().find(|&overrider_id| overrider_id.0 == class_id).copied() {
                                eprintln!("  Replacing interface call to {} in {} with virtual call to {}", MethodName(method_id, env), block_id, MethodName(overrider_id, env));
                                block.end_instr.kind = MilEndInstructionKind::CallVirtual(return_class_id, overrider_id, tgt, recv.clone(), args.clone());
                                break;
                            };
                        };
                    };
                };
            },
            MilEndInstructionKind::CallVirtual(return_class_id, method_id, tgt, ref recv, ref args) => {
                if let Some(constraint) = operand_constraint(&block_constraints, &constraints, recv) {
                    if constraint.class_id() != ClassId::UNRESOLVED {
                        let method = env.get_method(method_id).1;

                        for class_id in env.get_class_chain(constraint.class_id()) {
                            if let Some(overrider_id) = method.overrides.overridden_by.iter().find(|&overrider_id| overrider_id.0 == class_id).copied() {
                                eprintln!("  Replacing virtual call to {} in {} with virtual call to {}", MethodName(method_id, env), block_id, MethodName(overrider_id, env));
                                block.end_instr.kind = MilEndInstructionKind::CallVirtual(return_class_id, overrider_id, tgt, recv.clone(), args.clone());
                                break;
                            };
                        };
                    };
                };
            },
            _ => {}
        };
    };

    if num_changes != 0 {
        eprintln!("\n===== AFTER CLASS CONSTRAINT ANALYSIS =====\n\n{}", func.pretty(env));
    };

    num_changes
}
