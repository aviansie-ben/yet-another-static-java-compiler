use std::collections::{HashMap, VecDeque};

use itertools::Itertools;
use smallvec::SmallVec;

use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::mil::transform;
use crate::resolve::ClassEnvironment;
use crate::util::BitVec;

pub fn eliminate_dead_blocks(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment) -> usize {
    eprintln!("\n===== DEAD BLOCK ELIMINATION =====\n");

    let mut reachable = BitVec::new();
    let mut worklist = VecDeque::new();
    worklist.push_back(MilBlockId::ENTRY);

    while let Some(block_id) = worklist.pop_front() {
        for succ_id in cfg.get(block_id).outgoing.iter().copied() {
            if succ_id != MilBlockId::EXIT && !reachable.set(succ_id, true) {
                worklist.push_back(succ_id);
            };
        };
    };

    let blocks = &mut func.blocks;
    let num_eliminated = func.block_order.drain_filter(|&mut block_id| {
        if !reachable.get(block_id) {
            eprintln!("Eliminating dead block {}", block_id);

            for succ in cfg.get(block_id).outgoing.iter().copied() {
                if succ != MilBlockId::EXIT {
                    transform::remove_incoming_phis(blocks.get_mut(&succ).unwrap(), block_id);
                };
            };

            blocks.remove(&block_id);
            cfg.remove_node(block_id);

            true
        } else {
            false
        }
    }).count();

    if num_eliminated != 0 {
        eprintln!("\n===== AFTER DEAD BLOCK ELIMINATION =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_eliminated
}

pub fn simplify_phis(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment) -> usize {
    eprintln!("\n===== PHI NODE SIMPLIFICATION =====\n");

    let mut num_simplified = 0;
    let mut to_simplify = HashMap::new();

    loop {
        for block_id in func.block_order.iter().copied() {
            let block = func.blocks.get_mut(&block_id).unwrap();

            block.phi_nodes.drain_filter(|phi| {
                if phi.sources.iter().any(|(op, _)| op == &MilOperand::Register(MilRegister::VOID)) {
                    eprintln!("Replacing poisoned phi node {} with $void", phi.target);
                    to_simplify.insert(phi.target, MilOperand::Register(MilRegister::VOID));
                    true
                } else {
                    let first_real_source = phi.sources.iter().filter_map(|(op, _)| {
                        if op != &MilOperand::Register(phi.target) {
                            Some(op)
                        } else {
                            None
                        }
                    }).cloned().next();

                    if let Some(first_real_source) = first_real_source {
                        if phi.sources.iter().skip(1).all(|(op, _)| op == &first_real_source || op == &MilOperand::Register(phi.target)) {
                            let replacement = if let MilOperand::Register(first_real_source) = first_real_source {
                                to_simplify.get(&first_real_source).cloned().unwrap_or(MilOperand::Register(first_real_source))
                            } else {
                                first_real_source
                            };
                            eprintln!("Replacing redundant phi node {} with {}", phi.target, replacement.pretty(env));
                            to_simplify.insert(phi.target, replacement);
                            true
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
            });
        };

        if !to_simplify.is_empty() {
            for block_id in func.block_order.iter().copied() {
                transform::replace_register(func.blocks.get_mut(&block_id).unwrap(), &to_simplify);
            };

            num_simplified += to_simplify.len();
            to_simplify.clear();
        } else {
            break;
        };
    };

    if num_simplified != 0 {
        eprintln!("\n===== AFTER PHI NODE SIMPLIFICATION =====\n\n{}", func.pretty(env));
    };

    num_simplified
}

pub fn merge_blocks(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment) -> usize {
    eprintln!("\n===== BLOCK MERGING =====\n");

    let mut num_merged = 0;
    let mut i = 1;

    while i < func.block_order.len() {
        let prev_id = func.block_order[i - 1];
        let next_id = func.block_order[i];

        let prev = &func.blocks[&prev_id];
        let merged = if matches!(prev.end_instr.kind, MilEndInstructionKind::Nop) {
            let merge = if cfg.get(next_id).incoming.len() == 1 {
                eprintln!("Merging {} and {} since {} has one predecessor", prev_id, next_id, next_id);
                true
            } else {
                false
            };

            if merge {
                transform::rewrite_phis(&mut func.blocks, cfg, next_id, prev_id);

                let next = func.blocks.remove(&next_id).unwrap();
                let prev = func.blocks.get_mut(&prev_id).unwrap();

                // Must run phi simplification before block merging
                assert!(next.phi_nodes.is_empty());

                func.block_order.remove(i);
                cfg.merge_nodes_back(prev_id, next_id);

                prev.instrs.extend(next.instrs);
                prev.end_instr = next.end_instr;

                true
            } else {
                false
            }
        } else {
            false
        };

        if merged {
            num_merged += 1;
        } else {
            i += 1;
        };
    };

    if num_merged != 0 {
        eprintln!("\n===== AFTER BLOCK MERGING =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_merged
}

fn try_fold_constant_jump(instr: &mut MilEndInstructionKind, fallthrough_block: MilBlockId) -> Option<MilBlockId> {
    Some(match *instr {
        MilEndInstructionKind::JumpIf(cmp, target_block, MilOperand::Int(lhs), MilOperand::Int(rhs)) => {
            let is_taken = match cmp {
                MilComparison::Eq => lhs == rhs,
                MilComparison::Ne => lhs != rhs,
                MilComparison::Lt => lhs < rhs,
                MilComparison::Gt => lhs > rhs,
                MilComparison::Le => lhs <= rhs,
                MilComparison::Ge => lhs >= rhs
            };

            if is_taken {
                target_block
            } else {
                fallthrough_block
            }
        },
        MilEndInstructionKind::JumpIf(cmp, target_block, MilOperand::Null, MilOperand::Null) => match cmp {
            MilComparison::Eq => target_block,
            MilComparison::Ne => fallthrough_block,
            _ => unreachable!()
        },
        MilEndInstructionKind::JumpIf(cmp, target_block, MilOperand::Null, MilOperand::KnownObject(_, _))
            | MilEndInstructionKind::JumpIf(cmp, target_block, MilOperand::KnownObject(_, _), MilOperand::Null) => match cmp {
            MilComparison::Eq => fallthrough_block,
            MilComparison::Ne => target_block,
            _ => unreachable!()
        },
        MilEndInstructionKind::JumpIf(cmp, target_block, MilOperand::KnownObject(lhs, _), MilOperand::KnownObject(rhs, _)) => {
            let is_taken = match cmp {
                MilComparison::Eq => lhs == rhs,
                MilComparison::Ne => lhs != rhs,
                _ => unreachable!()
            };

            if is_taken {
                target_block
            } else {
                fallthrough_block
            }
        },
        _ => {
            return None;
        }
    })
}

pub fn fold_constant_jumps(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment) -> usize {
    eprintln!("\n===== CONSTANT JUMP FOLDING =====\n");

    let mut num_folded = 0;

    for (block_id, next_block_id) in func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        if let Some(target) = try_fold_constant_jump(&mut block.end_instr.kind, next_block_id) {
            eprintln!("Folding conditional jump at end of {} to unconditionally go to {}", block_id, target);

            block.end_instr.kind = if target != next_block_id {
                MilEndInstructionKind::Jump(target)
            } else {
                MilEndInstructionKind::Nop
            };

            let mut seen_target = false;
            let to_remove: SmallVec<[MilBlockId; 2]> = cfg.get(block_id).outgoing.iter().copied()
                .filter(|&succ| {
                    if succ == target && !seen_target {
                        seen_target = true;
                        false
                    } else {
                        true
                    }
                })
                .collect();
            assert!(seen_target);

            for succ in to_remove {
                cfg.remove_edge(block_id, succ);
                transform::remove_incoming_phis(func.blocks.get_mut(&succ).unwrap(), block_id);
            };

            num_folded += 1;
        };
    };

    if num_folded != 0 {
        eprintln!("\n===== AFTER CONSTANT JUMP FOLDING =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_folded
}
