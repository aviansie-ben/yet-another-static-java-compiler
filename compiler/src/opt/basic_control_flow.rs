use std::collections::HashMap;
use std::mem;

use itertools::Itertools;
use smallvec::SmallVec;

use crate::{log_write, log_writeln};
use crate::log::Log;
use crate::mil::dom::Dominators;
use crate::mil::flow_graph::{FlowGraph, FlowGraphNode};
use crate::mil::il::*;
use crate::mil::transform;
use crate::resolve::{ClassEnvironment, MethodName};

pub fn eliminate_dead_blocks(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== DEAD BLOCK ELIMINATION =====\n");

    let reachable = cfg.compute_reachability();
    let blocks = &mut func.blocks;
    let num_eliminated = func.block_order.drain_filter(|&mut block_id| {
        if !reachable.get(block_id) {
            log_writeln!(log, "Eliminating dead block {}", block_id);

            for succ in cfg.get(block_id).outgoing.iter().copied() {
                if succ != MilBlockId::EXIT && succ != block_id {
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
        log_writeln!(log, "\n===== AFTER DEAD BLOCK ELIMINATION =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_eliminated
}

pub fn simplify_phis(func: &mut MilFunction, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== PHI NODE SIMPLIFICATION =====\n");

    let mut num_simplified = 0;
    let mut to_simplify = HashMap::new();

    loop {
        for block_id in func.block_order.iter().copied() {
            let block = func.blocks.get_mut(&block_id).unwrap();

            block.phi_nodes.drain_filter(|phi| {
                if phi.sources.iter().any(|(op, _)| matches!(*op, MilOperand::Poison(_))) {
                    log_writeln!(log, "Replacing poisoned phi node {} with poison", phi.target);
                    to_simplify.insert(phi.target, MilOperand::Poison(phi.ty));
                    true
                } else {
                    let first_real_source = phi.sources.iter().filter_map(|(op, _)| {
                        if op.as_reg() != Some(phi.target) {
                            Some(op)
                        } else {
                            None
                        }
                    }).cloned().next();

                    if let Some(first_real_source) = first_real_source {
                        if phi.sources.iter().skip(1).all(|(op, _)| op == &first_real_source || op.as_reg() == Some(phi.target)) {
                            let replacement = if let MilOperand::Register(ty, first_real_source) = first_real_source {
                                to_simplify.get(&first_real_source).cloned().unwrap_or(MilOperand::Register(ty, first_real_source))
                            } else {
                                first_real_source
                            };
                            log_writeln!(log, "Replacing redundant phi node {} with {}", phi.target, replacement.pretty(env));
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
        log_writeln!(log, "\n===== AFTER PHI NODE SIMPLIFICATION =====\n\n{}", func.pretty(env));
    };

    num_simplified
}

fn find_incoming_overlap(prev_cfg_node: &FlowGraphNode<MilBlockId>, next_cfg_node: &FlowGraphNode<MilBlockId>) -> Vec<MilBlockId> {
    prev_cfg_node.incoming.iter().copied().filter(|pred| next_cfg_node.incoming.contains(pred)).collect()
}

fn is_phi_consistent(next: &MilBlock, prev_id: MilBlockId, incoming_overlap: &[MilBlockId]) -> bool {
    if !incoming_overlap.is_empty() {
        for phi in next.phi_nodes.iter() {
            let mut overlap_result = None;

            for &(ref val, pred_id) in phi.sources.iter() {
                if pred_id == prev_id || incoming_overlap.contains(&pred_id) {
                    if let Some(ref overlap_result) = overlap_result {
                        if overlap_result != val {
                            return false;
                        };
                    } else {
                        overlap_result = Some(val.clone());
                    };
                };
            };
        };
    };

    true
}

fn remove_phi_predecessor(phi: &mut MilPhiNode, pred_id: MilBlockId) -> MilOperand {
    let i = phi.sources.iter().enumerate()
        .filter(|&(_, &(_, p))| p == pred_id)
        .map(|(i, _)| i)
        .next().unwrap();
    phi.sources.remove(i).0
}

pub fn merge_blocks(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== BLOCK MERGING =====\n");

    let doms = Dominators::calculate_dominators(func, cfg);
    let mut num_merged = 0;
    let mut i = 1;

    while i < func.block_order.len() {
        let prev_id = func.block_order[i - 1];
        let next_id = func.block_order[i];

        let prev = &func.blocks[&prev_id];
        let merged = if matches!(prev.end_instr.kind, MilEndInstructionKind::Nop) {
            let next = &func.blocks[&next_id];
            let prev_cfg_node = cfg.get(prev_id);
            let next_cfg_node = cfg.get(next_id);
            let incoming_overlap = find_incoming_overlap(prev_cfg_node, next_cfg_node);

            if next_cfg_node.incoming.len() == 1 {
                log_writeln!(log, "Merging {} and {} since {} has one predecessor", prev_id, next_id, next_id);

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
            } else if i != 1 && prev.instrs.is_empty() && is_phi_consistent(next, prev_id, &incoming_overlap) {
                log_writeln!(log, "Merging {} and {} since {} is empty", prev_id, next_id, prev_id);

                let nonoverlap_new_incoming = prev_cfg_node.incoming.iter().copied()
                    .filter(|&pred_id| !incoming_overlap.contains(&pred_id))
                    .sorted_by_key(|i| i.0)
                    .dedup()
                    .collect_vec();

                transform::replace_block_target(&mut func.blocks, cfg, prev_id, next_id);

                let prev = func.blocks.remove(&prev_id).unwrap();
                let next = func.blocks.get_mut(&next_id).unwrap();

                for phi in next.phi_nodes.iter_mut() {
                    match remove_phi_predecessor(phi, prev_id) {
                        MilOperand::Register(ty, reg) => {
                            if let Some(prev_phi) = prev.phi_nodes.iter().filter(|&phi| phi.target == reg).next() {
                                phi.sources.reserve(nonoverlap_new_incoming.len());
                                for &(ref val, pred_id) in prev_phi.sources.iter() {
                                    if !incoming_overlap.contains(&pred_id) {
                                        phi.sources.push((val.clone(), pred_id));
                                    };
                                };
                            } else {
                                phi.sources.extend(nonoverlap_new_incoming.iter().copied().map(|pred_id| (MilOperand::Register(ty, reg), pred_id)));
                            };
                        },
                        val => {
                            phi.sources.extend(nonoverlap_new_incoming.iter().copied().map(|pred_id| (val.clone(), pred_id)));
                        }
                    }
                };

                if doms.get(next_id).get(prev_id) {
                    let loop_incoming = next_cfg_node.incoming.iter().copied()
                        .filter(|&pred_id| pred_id != prev_id && !prev_cfg_node.incoming.contains(&pred_id))
                        .sorted_by_key(|i| i.0)
                        .dedup()
                        .collect_vec();

                    for mut phi in prev.phi_nodes.into_iter() {
                        let phi_ty = phi.ty;
                        let phi_target = phi.target;

                        phi.sources.extend(loop_incoming.iter().copied().map(|pred_id| (MilOperand::Register(phi_ty, phi_target), pred_id)));
                        next.phi_nodes.push(phi);
                    };
                };

                func.block_order.remove(i - 1);
                cfg.merge_nodes_forward(prev_id, next_id);

                i = i - 1;
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
        log_writeln!(log, "\n===== AFTER BLOCK MERGING =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_merged
}

fn try_fold_constant_jump(instr: &mut MilEndInstructionKind) -> Option<MilBlockId> {
    Some(match *instr {
        MilEndInstructionKind::JumpIf(true_block, _, MilOperand::Bool(true)) => {
            true_block
        },
        MilEndInstructionKind::JumpIf(_, false_block, MilOperand::Bool(false)) => {
            false_block
        },
        _ => {
            return None;
        }
    })
}

pub fn fold_constant_jumps(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== CONSTANT JUMP FOLDING =====\n");

    let mut num_folded = 0;

    for (block_id, next_block_id) in func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        if let Some(target) = try_fold_constant_jump(&mut block.end_instr.kind) {
            log_writeln!(log, "Folding conditional jump at end of {} to unconditionally go to {}", block_id, target);

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
        log_writeln!(log, "\n===== AFTER CONSTANT JUMP FOLDING =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_folded
}

pub fn remove_redundant_jumps(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== REDUNDANT JUMP REMOVAL =====\n");

    let mut num_removed = 0;

    for (block_id, next_block_id) in func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        match block.end_instr.kind {
            MilEndInstructionKind::Jump(target_block_id) if target_block_id == next_block_id => {
                log_writeln!(log, "Removed redundant jump from {} to next block {}", block_id, next_block_id);
                block.end_instr.kind = MilEndInstructionKind::Nop;
                num_removed += 1;
            },
            MilEndInstructionKind::JumpIf(true_block_id, false_block_id, _) if true_block_id == false_block_id => {
                if true_block_id != next_block_id {
                    log_writeln!(log, "Turned redundant conditional jump from {} into an unconditional jump to {}", block_id, true_block_id);
                    block.end_instr.kind = MilEndInstructionKind::Jump(true_block_id)
                } else {
                    log_writeln!(log, "Removed redundant conditional jump from {} to next block {}", block_id, next_block_id);
                    block.end_instr.kind = MilEndInstructionKind::Nop;
                };
                cfg.remove_edge(block_id, next_block_id);
                num_removed += 1;
            },
            _ => {}
        };
    };

    if num_removed != 0 {
        log_writeln!(log, "\n===== AFTER REDUNDANT JUMP REMOVAL =====\n\n{}", func.pretty(env));
    };

    num_removed
}

pub fn devirtualize_nonoverriden_calls(func: &mut MilFunction, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== NON-OVERRIDDEN CALL DEVIRTUALIZATION =====\n");

    let mut num_devirtualized = 0;

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        match block.end_instr.kind {
            MilEndInstructionKind::CallVirtual(return_class_id, method_id, tgt, _, ref args) => {
                if env.get_method(method_id).1.overrides.overridden_by.is_empty() {
                    log_writeln!(log, "Devirtualizing call to non-overridden function {}", MethodName(method_id, env));
                    block.end_instr.kind = MilEndInstructionKind::Call(return_class_id, method_id, tgt, args.clone());
                    num_devirtualized += 1;
                };
            },
            _ => {}
        };
    };

    if num_devirtualized != 0 {
        log_writeln!(log, "\n===== AFTER NON-OVERRIDDEN CALL DEVIRTUALIZATION =====\n\n{}", func.pretty(env));
    };

    num_devirtualized
}

pub fn is_pass_through_block(func: &MilFunction, from_node: &FlowGraphNode<MilBlockId>, to_block_id: MilBlockId) -> bool {
    if &from_node.outgoing != &[to_block_id] || from_node.incoming.len() != 1 {
        return false;
    };

    let from_block = &func.blocks[&from_node.id];

    if !from_block.phi_nodes.is_empty() {
        return false;
    } else if !from_block.instrs.is_empty() {
        return false;
    };

    match from_block.end_instr.kind {
        MilEndInstructionKind::Nop => {},
        MilEndInstructionKind::Jump(_) => {},
        _ => {
            return false;
        }
    };

    true
}

pub fn recognize_select_pattern(func: &mut MilFunction, cfg: &mut FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== SELECT PATTERN RECOGNITION =====\n");

    let mut num_recognized = 0;

    for cond_block_id in func.block_order.iter().copied() {
        let cond_block = &func.blocks[&cond_block_id];

        if let MilEndInstructionKind::JumpIf(true_block_id, false_block_id, ref cond) = cond_block.end_instr.kind {
            let true_node = cfg.get(true_block_id);
            let false_node = cfg.get(false_block_id);

            let (true_pred_id, false_pred_id, merge_id) = if true_node.incoming.len() == 2 {
                // L0:
                //   jc L2/L1, <cond>
                // L1:
                //   j L2
                // L2:
                //   phi <tgt>, L0:<true_val>, L1:<false_val>
                if !is_pass_through_block(func, false_node, true_block_id) {
                    continue;
                };

                (cond_block_id, false_block_id, true_block_id)
            } else if false_node.incoming.len() == 2 {
                // L0:
                //   jc L1/L2, <cond>
                // L1:
                //   j L2
                // L2:
                //   phi <tgt>, L0:<false_val>, L1:<true_val>
                if !is_pass_through_block(func, true_node, false_block_id) {
                    continue;
                };

                (true_block_id, cond_block_id, false_block_id)
            } else {
                continue;
            };

            log_writeln!(log, "Recognized select pattern {} -> [ {} {} ] -> {}, condition {}", cond_block_id, true_pred_id, false_pred_id, merge_id, cond.pretty(env));

            let cond = cond.clone();
            let phis = mem::replace(&mut func.blocks.get_mut(&merge_id).unwrap().phi_nodes, vec![]);
            let cond_block = func.blocks.get_mut(&cond_block_id).unwrap();

            for mut phi in phis {
                log_write!(log, "  {} -> ", phi.pretty(env));

                assert_eq!(2, phi.sources.len());

                let src_0 = mem::replace(&mut phi.sources[0].0, MilOperand::RefNull);
                let src_1 = mem::replace(&mut phi.sources[1].0, MilOperand::RefNull);

                let (true_val, false_val) = if phi.sources[0].1 == true_pred_id {
                    assert_eq!(false_pred_id, phi.sources[1].1);

                    (src_0, src_1)
                } else {
                    assert_eq!(false_pred_id, phi.sources[0].1);
                    assert_eq!(true_pred_id, phi.sources[1].1);

                    (src_1, src_0)
                };

                let select_instr = MilInstruction {
                    kind: MilInstructionKind::Select(phi.target, cond.clone(), true_val, false_val),
                    bytecode: phi.bytecode
                };

                log_writeln!(log, "{}", select_instr.pretty(env));
                cond_block.instrs.push(select_instr);
            };

            cfg.remove_edge(cond_block_id, true_block_id);
            cfg.remove_edge(cond_block_id, false_block_id);
            cfg.add_edge(cond_block_id, merge_id);

            cond_block.end_instr.kind = MilEndInstructionKind::Jump(merge_id);

            num_recognized += 1;
        };
    };

    if num_recognized != 0 {
        log_writeln!(log, "\n===== AFTER SELECT PATTERN RECONGITION =====\n\n{}\n{:#?}", func.pretty(env), cfg);
    };

    num_recognized
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::log::Log;
    use crate::resolve::MethodId;
    use crate::test_util::*;

    use smallvec::smallvec;

    static NO_BYTECODE: (u32, u32) = (!0, !0);

    fn create_test_block(id: MilBlockId, phis: &[MilPhiNode], instrs: &[MilInstructionKind], end_instr: MilEndInstructionKind) -> MilBlock {
        let mut block = MilBlock::new();

        block.id = id;
        block.phi_nodes = phis.iter().cloned().collect();
        block.instrs = instrs.iter().cloned().map(|instr| MilInstruction { kind: instr, bytecode: NO_BYTECODE }).collect();
        block.end_instr.kind = end_instr;

        block
    }

    #[test]
    fn test_eliminate_trivial_dead_block() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.blocks.insert(MilBlockId(1), create_test_block(MilBlockId(1), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 1);

        assert_eq!(func.block_order, vec![MilBlockId(0)]);
        assert!(func.blocks.get(&MilBlockId(0)).is_some());
        assert!(func.blocks.get(&MilBlockId(1)).is_none());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_none());
    }

    #[test]
    fn test_eliminate_dead_block_with_edge_to_live_block() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.blocks.insert(MilBlockId(1), create_test_block(MilBlockId(1), &[], &[], MilEndInstructionKind::Jump(MilBlockId(0))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 1);

        assert_eq!(func.block_order, vec![MilBlockId(0)]);
        assert!(func.blocks.get(&MilBlockId(0)).is_some());
        assert!(func.blocks.get(&MilBlockId(1)).is_none());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_none());
        assert_eq!(cfg.get(MilBlockId(0)).incoming, vec![MilBlockId::ENTRY]);
    }

    #[test]
    fn test_eliminate_dead_block_with_phi_to_live_block() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::Nop));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(2), MilBlockId(2))], bytecode: NO_BYTECODE }],
            &[],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(MilBlockId(2), &[], &[], MilEndInstructionKind::Jump(MilBlockId(1))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 1);

        assert_eq!(func.block_order, vec![MilBlockId(0), MilBlockId(1)]);
        assert!(func.blocks.get(&MilBlockId(0)).is_some());
        assert!(func.blocks.get(&MilBlockId(1)).is_some());
        assert!(func.blocks.get(&MilBlockId(2)).is_none());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_some());
        assert!(cfg.try_get(MilBlockId(2)).is_none());
        assert_eq!(func.blocks[&MilBlockId(1)].phi_nodes[0].sources.clone().into_vec(), vec![(MilOperand::Int(0), MilBlockId(0))]);
    }

    #[test]
    fn test_eliminate_dead_block_single_loop() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.blocks.insert(MilBlockId(1), create_test_block(MilBlockId(1), &[], &[], MilEndInstructionKind::Jump(MilBlockId(1))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 1);

        assert_eq!(func.block_order, vec![MilBlockId(0)]);
        assert!(func.blocks.get(&MilBlockId(1)).is_none());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_none());
    }

    #[test]
    fn test_eliminate_dead_block_multi_loop() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.blocks.insert(MilBlockId(1), create_test_block(MilBlockId(1), &[], &[], MilEndInstructionKind::Nop));
        func.blocks.insert(MilBlockId(2), create_test_block(MilBlockId(2), &[], &[], MilEndInstructionKind::Jump(MilBlockId(1))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 2);

        assert_eq!(func.block_order, vec![MilBlockId(0)]);
        assert!(func.blocks.get(&MilBlockId(1)).is_none());
        assert!(func.blocks.get(&MilBlockId(2)).is_none());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_none());
        assert!(cfg.try_get(MilBlockId(2)).is_none());
    }

    #[test]
    fn test_no_eliminate_live_blocks() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(MilBlockId(0), &[], &[], MilEndInstructionKind::JumpIf(MilBlockId(3), MilBlockId(1), MilOperand::Bool(true))));
        func.blocks.insert(MilBlockId(1), create_test_block(MilBlockId(1), &[], &[], MilEndInstructionKind::Jump(MilBlockId(4))));
        func.blocks.insert(MilBlockId(2), create_test_block(MilBlockId(2), &[], &[], MilEndInstructionKind::Nop));
        func.blocks.insert(MilBlockId(3), create_test_block(MilBlockId(3), &[], &[], MilEndInstructionKind::Jump(MilBlockId(2))));
        func.blocks.insert(MilBlockId(4), create_test_block(MilBlockId(4), &[], &[], MilEndInstructionKind::Return(Some(MilOperand::RefNull))));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3), MilBlockId(4)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(eliminate_dead_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()), 0);

        assert_eq!(func.block_order, vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3), MilBlockId(4)]);
        assert!(func.blocks.get(&MilBlockId(0)).is_some());
        assert!(func.blocks.get(&MilBlockId(1)).is_some());
        assert!(func.blocks.get(&MilBlockId(2)).is_some());
        assert!(func.blocks.get(&MilBlockId(3)).is_some());
        assert!(func.blocks.get(&MilBlockId(4)).is_some());
        assert!(cfg.try_get(MilBlockId(0)).is_some());
        assert!(cfg.try_get(MilBlockId(1)).is_some());
        assert!(cfg.try_get(MilBlockId(2)).is_some());
        assert!(cfg.try_get(MilBlockId(3)).is_some());
        assert!(cfg.try_get(MilBlockId(4)).is_some());
    }

    #[test]
    fn test_simplify_phi_same_const() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Int(0)));
    }

    #[test]
    fn test_simplify_phi_same_reg() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(1)), MilBlockId(0)), (MilOperand::Register(MilType::Int, MilRegister(1)), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(1))));
    }

    #[test]
    fn test_simplify_phi_const_and_self() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Int(0)));
    }

    #[test]
    fn test_simplify_phi_reg_and_self() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(0)), (MilOperand::Register(MilType::Int, MilRegister(1)), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(1))));
    }

    #[test]
    fn test_simplify_poisoned_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Poison(MilType::Int), MilBlockId(0)), (MilOperand::Register(MilType::Int, MilRegister(1)), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister(1), MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister(1), MilOperand::Poison(MilType::Int)));
    }

    #[test]
    fn test_unsimplifiable_phis() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[
                MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))], bytecode: NO_BYTECODE },
                MilPhiNode { target: MilRegister(1), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(2)), MilBlockId(0)), (MilOperand::Register(MilType::Int, MilRegister(3)), MilBlockId(1))], bytecode: NO_BYTECODE }
            ],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0))),
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(1)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 0);
        assert_eq!(func.blocks[&MilBlockId(0)].phi_nodes.len(), 2);
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0))));
        assert_eq!(func.blocks[&MilBlockId(0)].instrs[1].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(1))));
    }

    #[test]
    fn test_simplify_phi_with_nonlocal_refs() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[MilPhiNode { target: MilRegister(1), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(0)))
            ],
            MilEndInstructionKind::Return(Some(MilOperand::Register(MilType::Int, MilRegister(0))))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 1);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(1)].phi_nodes[0].sources.clone().into_vec(), vec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))]);
        assert_eq!(func.blocks[&MilBlockId(1)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Int(0)));
        assert_eq!(func.blocks[&MilBlockId(1)].end_instr.kind, MilEndInstructionKind::Return(Some(MilOperand::Int(0))));
    }

    #[test]
    fn test_simplify_phi_nested() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[MilPhiNode { target: MilRegister(1), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: NO_BYTECODE }],
            &[
                MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Register(MilType::Int, MilRegister(1)))
            ],
            MilEndInstructionKind::Nop
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        assert_eq!(simplify_phis(&mut func, &TEST_ENV, &Log::none()), 2);
        assert!(func.blocks[&MilBlockId(0)].phi_nodes.is_empty());
        assert!(func.blocks[&MilBlockId(1)].phi_nodes.is_empty());
        assert_eq!(func.blocks[&MilBlockId(1)].instrs[0].kind, MilInstructionKind::Copy(MilRegister::VOID, MilOperand::Int(0)));
    }

    #[test]
    fn test_merge_blocks_back_trivial() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[MilInstructionKind::Copy(MilRegister(0), MilOperand::Int(0))],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[MilInstructionKind::Copy(MilRegister(1), MilOperand::Int(1))],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0)], func.block_order);

        let block_0 = &func.blocks[&MilBlockId(0)];
        assert!(block_0.phi_nodes.is_empty());
        assert_eq!(2, block_0.instrs.len());
        assert_eq!(MilInstructionKind::Copy(MilRegister(0), MilOperand::Int(0)), block_0.instrs[0].kind);
        assert_eq!(MilInstructionKind::Copy(MilRegister(1), MilOperand::Int(1)), block_0.instrs[1].kind);
        assert_eq!(MilEndInstructionKind::Return(Some(MilOperand::RefNull)), block_0.end_instr.kind);

        let cfg_0 = cfg.get(MilBlockId(0));
        assert_eq!(vec![MilBlockId::ENTRY], cfg_0.incoming);
        assert_eq!(vec![MilBlockId::EXIT], cfg_0.outgoing);
    }

    #[test]
    fn test_merge_blocks_back_with_outgoing_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(3), MilBlockId(2), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(1)), (MilOperand::Int(1), MilBlockId(2))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Jump(MilBlockId(2))
        ));
        func.blocks.insert(MilBlockId(3), create_test_block(
            MilBlockId(3),
            &[MilPhiNode { target: MilRegister(1), ty: MilType::Int, sources: smallvec![(MilOperand::Int(1), MilBlockId(3)), (MilOperand::Int(0), MilBlockId(1))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Jump(MilBlockId(3))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(2), MilBlockId(3)], func.block_order);

        assert_eq!(
            vec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(2))],
            func.blocks[&MilBlockId(2)].phi_nodes[0].sources.clone().into_vec()
        );
        assert_eq!(
            vec![MilBlockId(0), MilBlockId(2)],
            cfg.get(MilBlockId(2)).incoming
        );

        assert_eq!(
            vec![(MilOperand::Int(1), MilBlockId(3)), (MilOperand::Int(0), MilBlockId(0))],
            func.blocks[&MilBlockId(3)].phi_nodes[0].sources.clone().into_vec()
        );
        assert_eq!(
            vec![MilBlockId(0), MilBlockId(3)],
            cfg.get(MilBlockId(3)).incoming
        );
    }

    #[test]
    fn test_merge_blocks_back_triple() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[MilInstructionKind::Copy(MilRegister(0), MilOperand::Int(0))],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[MilInstructionKind::Copy(MilRegister(1), MilOperand::Int(1))],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[MilInstructionKind::Copy(MilRegister(2), MilOperand::Int(2))],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(2, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0)], func.block_order);

        let block_0 = &func.blocks[&MilBlockId(0)];
        assert!(block_0.phi_nodes.is_empty());
        assert_eq!(3, block_0.instrs.len());
        assert_eq!(MilInstructionKind::Copy(MilRegister(0), MilOperand::Int(0)), block_0.instrs[0].kind);
        assert_eq!(MilInstructionKind::Copy(MilRegister(1), MilOperand::Int(1)), block_0.instrs[1].kind);
        assert_eq!(MilInstructionKind::Copy(MilRegister(2), MilOperand::Int(2)), block_0.instrs[2].kind);
        assert_eq!(MilEndInstructionKind::Return(Some(MilOperand::RefNull)), block_0.end_instr.kind);

        let cfg_0 = cfg.get(MilBlockId(0));
        assert_eq!(vec![MilBlockId::ENTRY], cfg_0.incoming);
        assert_eq!(vec![MilBlockId::EXIT], cfg_0.outgoing);
    }

    #[test]
    fn test_no_merge_blocks_back_multi_predecessor() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(0, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
    }

    #[test]
    fn test_merge_blocks_forward_trivial() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(2)], func.block_order);

        let block_2 = &func.blocks[&MilBlockId(2)];
        assert!(block_2.phi_nodes.is_empty());
        assert_eq!(1, block_2.instrs.len());
        assert_eq!(MilInstructionKind::Nop, block_2.instrs[0].kind);
        assert_eq!(MilEndInstructionKind::Return(Some(MilOperand::RefNull)), block_2.end_instr.kind);

        assert_eq!(vec![MilBlockId(2), MilBlockId(2)], cfg.get(MilBlockId(0)).outgoing);
        assert_eq!(vec![MilBlockId(0), MilBlockId(0)], cfg.get(MilBlockId(2)).incoming);
    }

    #[test]
    fn test_no_merge_blocks_forward_inconsistent_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(0, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
    }

    #[test]
    fn test_no_merge_blocks_forward_entry_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::void());

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Jump(MilBlockId(1))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(0, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
    }

    #[test]
    fn test_merge_blocks_forward_consistent_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(0), MilBlockId(1))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(2)], func.block_order);
        assert_eq!(vec![(MilOperand::Int(0), MilBlockId(0))], func.blocks[&MilBlockId(2)].phi_nodes[0].sources.clone().into_vec());
    }

    #[test]
    fn test_merge_blocks_forward_complex_phis() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(3), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(4), MilBlockId(2), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(3), create_test_block(
            MilBlockId(3),
            &[MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(2))], bytecode: (!0, 0) }],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(4), create_test_block(
            MilBlockId(4),
            &[
                MilPhiNode { target: MilRegister(1), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(1)), (MilOperand::Int(1), MilBlockId(3))], bytecode: (!0, 0) },
                MilPhiNode { target: MilRegister(2), ty: MilType::Int, sources: smallvec![(MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(3)), (MilOperand::Int(0), MilBlockId(1))], bytecode: (!0, 0) }
            ],
            &[],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3), MilBlockId(4)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(4)], func.block_order);

        let block_4 = &func.blocks[&MilBlockId(4)];
        assert_eq!(
            vec![(MilOperand::Int(0), MilBlockId(1)), (MilOperand::Int(1), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(2))],
            block_4.phi_nodes[0].sources.clone().into_vec()
        );
        assert_eq!(
            vec![(MilOperand::Int(0), MilBlockId(1)), (MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(2))],
            block_4.phi_nodes[1].sources.clone().into_vec()
        );
    }

    #[test]
    fn test_merge_blocks_forward_duplicate_predecessor() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(3), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(2), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(3), create_test_block(
            MilBlockId(3),
            &[
                MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(2))], bytecode: (!0, 0) }
            ],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::Return(Some(MilOperand::RefNull))
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(1), MilBlockId(3)], func.block_order);

        assert_eq!(
            vec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(1))],
            func.blocks[&MilBlockId(3)].phi_nodes[0].sources.clone().into_vec()
        );
    }

    #[test]
    fn test_merge_blocks_forward_loop_phi() {
        let mut func = MilFunction::new(MethodId::UNRESOLVED, MilFunctionSignature::new_bare(Some(MilType::Ref), vec![]));

        func.blocks.insert(MilBlockId(0), create_test_block(
            MilBlockId(0),
            &[],
            &[],
            MilEndInstructionKind::JumpIf(MilBlockId(4), MilBlockId(1), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(1), create_test_block(
            MilBlockId(1),
            &[
                MilPhiNode { target: MilRegister(0), ty: MilType::Int, sources: smallvec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(3))], bytecode: (!0, 0) }
            ],
            &[],
            MilEndInstructionKind::Nop
        ));
        func.blocks.insert(MilBlockId(2), create_test_block(
            MilBlockId(2),
            &[],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::JumpIf(MilBlockId(2), MilBlockId(3), MilOperand::Bool(true))
        ));
        func.blocks.insert(MilBlockId(3), create_test_block(
            MilBlockId(3),
            &[],
            &[
                MilInstructionKind::Nop
            ],
            MilEndInstructionKind::Jump(MilBlockId(1))
        ));
        func.blocks.insert(MilBlockId(4), create_test_block(
            MilBlockId(4),
            &[],
            &[],
            MilEndInstructionKind::Return(None)
        ));
        func.block_order = vec![MilBlockId(0), MilBlockId(1), MilBlockId(2), MilBlockId(3), MilBlockId(4)];

        let mut cfg = FlowGraph::for_function(&func);

        assert_eq!(1, merge_blocks(&mut func, &mut cfg, &TEST_ENV, &Log::none()));
        assert_eq!(vec![MilBlockId(0), MilBlockId(2), MilBlockId(3), MilBlockId(4)], func.block_order);

        let block_2 = &func.blocks[&MilBlockId(2)];
        assert_eq!(1, block_2.phi_nodes.len());
        assert_eq!(MilRegister(0), block_2.phi_nodes[0].target);
        assert_eq!(
            vec![(MilOperand::Int(0), MilBlockId(0)), (MilOperand::Int(1), MilBlockId(3)), (MilOperand::Register(MilType::Int, MilRegister(0)), MilBlockId(2))],
            block_2.phi_nodes[0].sources.clone().into_vec()
        );
    }
}
