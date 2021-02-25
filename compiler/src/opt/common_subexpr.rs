use std::collections::HashMap;
use std::collections::hash_map::Entry;

use crate::log_writeln;
use crate::classfile::FieldFlags;
use crate::log::Log;
use crate::mil::dom::DominatorTree;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::{ClassEnvironment, FieldId};
use crate::util::BitVec;

#[derive(Debug, Clone)]
struct AvailableExpressions {
    un_ops: HashMap<(MilUnOp, MilOperand), MilOperand>,
    bin_ops: HashMap<(MilBinOp, MilOperand, MilOperand), MilOperand>,
    field_loads: Vec<(FieldId, Option<MilOperand>, MilOperand)>
}

impl AvailableExpressions {
    pub fn new() -> AvailableExpressions {
        AvailableExpressions {
            bin_ops: HashMap::new(),
            un_ops: HashMap::new(),
            field_loads: vec![]
        }
    }

    pub fn handle_un_op(&mut self, tgt: MilRegister, op: MilUnOp, val: &MilOperand) -> Option<MilOperand> {
        match self.un_ops.entry((op, val.clone())) {
            Entry::Occupied(entry) => Some(entry.get().clone()),
            Entry::Vacant(entry) => {
                entry.insert(MilOperand::Register(op.type_sig().0, tgt));
                None
            }
        }
    }

    pub fn handle_bin_op(&mut self, tgt: MilRegister, op: MilBinOp, lhs: &MilOperand, rhs: &MilOperand) -> Option<MilOperand> {
        match self.bin_ops.entry((op, lhs.clone(), rhs.clone())) {
            Entry::Occupied(entry) => Some(entry.get().clone()),
            Entry::Vacant(entry) => {
                entry.insert(MilOperand::Register(op.type_sig().0, tgt));
                None
            }
        }
    }

    pub fn handle_field_load(&mut self, tgt: MilRegister, ty: MilType, field_id: FieldId, obj: Option<&MilOperand>) -> Option<MilOperand> {
        if let Some(&(_, _, ref val)) = self.field_loads.iter().find(|&&(id, ref o, _)| id == field_id && o.as_ref() == obj) {
            Some(val.clone())
        } else {
            self.field_loads.push((field_id, obj.cloned(), MilOperand::Register(ty, tgt)));
            None
        }
    }

    pub fn set_field(&mut self, field_id: FieldId, obj: Option<&MilOperand>, val: &MilOperand) {
        self.flush_field_loads(&[field_id]);
        self.field_loads.push((field_id, obj.cloned(), val.clone()));
    }

    pub fn flush_loads(&mut self, env: &ClassEnvironment) {
        self.field_loads.drain_filter(|&mut (id, _, _)| {
            !env.get_field(id).1.flags.contains(FieldFlags::FINAL)
        });
    }

    pub fn flush_field_loads(&mut self, field_ids: &[FieldId]) {
        self.field_loads.drain_filter(|&mut (id, _, _)| {
            field_ids.contains(&id)
        });
    }
}

fn find_intervening_blocks(b1: MilBlockId, b2: MilBlockId, cfg: &FlowGraph<MilBlockId>) -> Vec<MilBlockId> {
    fn find_intervening_blocks_impl(block: MilBlockId, cfg: &FlowGraph<MilBlockId>, seen: &mut BitVec<MilBlockId>, reaches_target: &mut BitVec<MilBlockId>, path: &mut Vec<MilBlockId>, result: &mut Vec<MilBlockId>) {
        if block == MilBlockId::EXIT {
            return;
        };

        if reaches_target.get(block) {
            path.pop();

            for block in path.iter().copied() {
                reaches_target.set(block, true);
            };

            result.extend(path.drain(..));
            return;
        };

        if block != MilBlockId::ENTRY && seen.set(block, true) {
            return;
        };

        for succ_id in cfg.get(block).outgoing.iter().copied() {
            path.push(succ_id);
            find_intervening_blocks_impl(succ_id, cfg, seen, reaches_target, path, result);
            path.pop();
        };
    }

    let mut seen = BitVec::new();
    let mut reaches_target = BitVec::new();
    let mut path = vec![];
    let mut result = vec![];

    reaches_target.set(b2, true);
    find_intervening_blocks_impl(b1, cfg, &mut seen, &mut reaches_target, &mut path, &mut result);

    // If the target block is the start of a loop, then control might reach it from its immediate dominator via itself. Thus, any successors
    // of the target block might also be intervening blocks.
    path.push(b2);
    for succ_id in cfg.get(b2).outgoing.iter().copied() {
        path.push(succ_id);
        find_intervening_blocks_impl(succ_id, cfg, &mut seen, &mut reaches_target, &mut path, &mut result);
        path.pop();
    };

    result
}

pub fn eliminate_common_subexpressions_globally(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== COMMON SUBEXPRESSION ELIMINATION =====\n");

    let mut num_commoned = 0;
    let rpo_blocks = cfg.compute_reverse_postorder();
    let doms = DominatorTree::calculate_dominator_tree(cfg, &rpo_blocks);

    let block_kills: HashMap<_, _> = rpo_blocks.iter().copied().map(|block_id| {
        let block = func.blocks.get_mut(&block_id).unwrap();

        if block.end_instr.is_call() {
            (block_id, None)
        } else {
            let mut kill = vec![];

            for instr in block.instrs.iter() {
                match instr.kind {
                    MilInstructionKind::PutField(field_id, _, _, _) => {
                        if !kill.contains(&field_id) {
                            kill.push(field_id);
                        };
                    },
                    MilInstructionKind::PutStatic(field_id, _, _) => {
                        if !kill.contains(&field_id) {
                            kill.push(field_id);
                        };
                    },
                    _ => {}
                };
            };

            (block_id, Some(kill))
        }
    }).collect();
    let mut kill = vec![];

    let mut available_exprs: HashMap<MilBlockId, AvailableExpressions> = HashMap::new();
    let mut mappings: HashMap<MilRegister, MilOperand> = HashMap::new();

    for block_id in rpo_blocks.iter().copied() {
        let idom_id = doms.get(block_id);

        log_writeln!(log, "{}:", block_id);
        log_writeln!(log, "  (Taking exprs from {})", idom_id);

        let mut this_available_exprs = available_exprs.get(&idom_id).map(|ae| ae.clone()).unwrap_or_else(AvailableExpressions::new);
        let block = func.blocks.get_mut(&block_id).unwrap();

        if cfg.get(block_id).incoming.len() > 1 {
            let intervening_blocks = find_intervening_blocks(idom_id, block_id, cfg);
            log_writeln!(log, "  (Flushing loads killed by intervening blocks {:?})", find_intervening_blocks(idom_id, block_id, cfg));

            let mut kill_all = false;
            kill.clear();
            for intervening_block_id in intervening_blocks {
                if let Some(intervening_kill) = block_kills[&intervening_block_id].as_ref() {
                    kill.extend(intervening_kill);
                } else {
                    kill_all = true;
                    break;
                };
            };

            if kill_all {
                this_available_exprs.flush_loads(env);
            } else {
                this_available_exprs.flush_field_loads(&kill);
            };
        };

        for instr in block.instrs.iter_mut() {
            instr.for_operands_mut(|o| if let Some(val) = o.as_reg().and_then(|reg| mappings.get(&reg).cloned()) {
                *o = val;
            });

            let replace_val = match instr.kind {
                MilInstructionKind::Nop => None,
                MilInstructionKind::Copy(_, _) => None,
                MilInstructionKind::Select(_, _, _, _) => None,
                MilInstructionKind::UnOp(op, tgt, ref val) => this_available_exprs.handle_un_op(tgt, op, val),
                MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => this_available_exprs.handle_bin_op(tgt, op, lhs, rhs),
                MilInstructionKind::GetParam(_, _, _) => None,
                MilInstructionKind::GetLocal(_, _) => None,
                MilInstructionKind::SetLocal(_, _) => None,
                MilInstructionKind::GetField(field_id, cls, tgt, ref obj) => this_available_exprs.handle_field_load(tgt, MilType::for_class(cls), field_id, Some(obj)),
                MilInstructionKind::PutField(field_id, _, ref obj, ref val) => {
                    this_available_exprs.set_field(field_id, Some(obj), val);
                    None
                },
                MilInstructionKind::GetArrayElement(_, _, _, _) => None,
                MilInstructionKind::PutArrayElement(_, _, _, _) => None,
                MilInstructionKind::GetStatic(field_id, cls, tgt) => this_available_exprs.handle_field_load(tgt, MilType::for_class(cls), field_id, None),
                MilInstructionKind::PutStatic(field_id, _, ref val) => {
                    this_available_exprs.set_field(field_id, None, val);
                    None
                },
                MilInstructionKind::AllocObj(_, _) => None,
                MilInstructionKind::AllocArray(_, _, _) => None
            };

            if let Some(replace_val) = replace_val {
                let tgt = *instr.target().unwrap();
                log_writeln!(log, "  Replacing {} with {}", instr.pretty(env), replace_val.pretty(env));

                instr.kind = MilInstructionKind::Copy(tgt, replace_val.clone());
                mappings.insert(tgt, replace_val);
                num_commoned += 1;
            };
        };

        if block.end_instr.is_call() {
            log_writeln!(log, "  (Flushing non-final loads due to call)");
            this_available_exprs.flush_loads(env);
        };

        available_exprs.insert(block_id, this_available_exprs);
    };

    if num_commoned != 0 {
        log_writeln!(log, "\n===== AFTER COMMON SUBEXPRESSION ELIMINATION =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_commoned
}
