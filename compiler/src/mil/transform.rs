use std::collections::HashMap;

use super::flow_graph::*;
use super::il::*;

pub fn replace_register(block: &mut MilBlock, map: &HashMap<MilRegister, MilOperand>) {
    for phi in block.phi_nodes.iter_mut() {
        for &mut (ref mut src, _) in phi.sources.iter_mut() {
            if let MilOperand::Register(ref mut src_reg) = *src {
                if let Some(new_src) = map.get(src_reg).cloned() {
                    *src = new_src;
                };
            };
        };
    };

    for instr in block.instrs.iter_mut() {
        instr.for_operands_mut(|src| {
            if let MilOperand::Register(ref mut src_reg) = *src {
                if let Some(new_src) = map.get(src_reg).cloned() {
                    *src = new_src;
                };
            };
        });
    };

    block.end_instr.for_operands_mut(|src| {
        if let MilOperand::Register(ref mut src_reg) = *src {
            if let Some(new_src) = map.get(src_reg).cloned() {
                *src = new_src;
            };
        };
    });
}

pub fn rewrite_phis(blocks: &mut HashMap<MilBlockId, MilBlock>, cfg: &FlowGraph<MilBlockId>, old: MilBlockId, new: MilBlockId) {
    for succ in cfg.get(old).outgoing.iter().copied() {
        if succ != MilBlockId::EXIT {
            for phi in blocks.get_mut(&succ).unwrap().phi_nodes.iter_mut() {
                for &mut (_, ref mut pred) in phi.sources.iter_mut() {
                    if *pred == old {
                        *pred = new;
                    };
                };
            };
        };
    };
}

pub fn remove_incoming_phis(block: &mut MilBlock, pred: MilBlockId) {
    for phi in block.phi_nodes.iter_mut() {
        phi.sources.remove(
            phi.sources.iter().enumerate()
                .find(|(_, &(_, phi_pred))| phi_pred == pred)
                .map(|(i, _)| i)
                .unwrap()
        );
    };
}

pub fn remove_nops(block: &mut MilBlock) {
    block.instrs.drain_filter(|instr| {
        if let MilInstructionKind::Nop = instr.kind {
            true
        } else {
            false
        }
    });
}
