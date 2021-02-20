use std::collections::{HashMap, VecDeque};
use std::fmt;

use super::flow_graph::FlowGraph;
use super::il::*;
use crate::util::BitVec;

#[derive(Clone)]
pub struct BlockDominators {
    blocks: BitVec<MilBlockId>
}

impl BlockDominators {
    pub fn new() -> BlockDominators {
        BlockDominators {
            blocks: BitVec::new()
        }
    }

    pub fn from_iter(iter: impl IntoIterator<Item=MilBlockId>) -> BlockDominators {
        let mut doms = BlockDominators::new();

        for block in iter {
            doms.set(block, true);
        };

        doms
    }

    pub fn set(&mut self, block: MilBlockId, val: bool) {
        if block != MilBlockId::ENTRY && block != MilBlockId::EXIT {
            self.blocks.set(block, val);
        };
    }

    pub fn get(&self, block: MilBlockId) -> bool {
        if block == MilBlockId::ENTRY {
            true
        } else if block == MilBlockId::EXIT {
            false
        } else {
            self.blocks.get(block)
        }
    }

    pub fn clear(&mut self) {
        self.blocks.clear();
    }

    pub fn intersect(&mut self, other: &BlockDominators) -> bool {
        self.blocks.intersect(&other.blocks)
    }

    pub fn union(&mut self, other: &BlockDominators) -> bool {
        self.blocks.union(&other.blocks)
    }
}

pub struct Dominators {
    doms: HashMap<MilBlockId, BlockDominators>
}

impl Dominators {
    pub fn calculate_dominators(func: &MilFunction, cfg: &FlowGraph<MilBlockId>) -> Dominators {
        let all_blocks = BlockDominators::from_iter(func.block_order.iter().copied());
        let mut doms = HashMap::new();

        for id in func.block_order.iter().copied() {
            doms.insert(id, all_blocks.clone());
        };

        doms.insert(MilBlockId::ENTRY, BlockDominators::new());
        doms.insert(MilBlockId::EXIT, all_blocks);

        let mut worklist: VecDeque<_> = func.block_order.iter().copied().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).collect();
        let mut pred_doms = BlockDominators::new();

        while let Some(next_id) = worklist.pop_front() {
            let mut first = true;

            pred_doms.clear();

            for pred_id in cfg.get(next_id).incoming.iter().copied().filter(|&pred_id| pred_id != next_id) {
                if first {
                    pred_doms.union(&doms[&pred_id]);
                    first = false;
                } else {
                    pred_doms.intersect(&doms[&pred_id]);
                };
            };

            pred_doms.set(next_id, true);
            if doms.get_mut(&next_id).unwrap().intersect(&pred_doms) {
                for succ_id in cfg.get(next_id).outgoing.iter().copied().filter(|&pred_id| pred_id != next_id) {
                    if !worklist.contains(&succ_id) {
                        worklist.push_back(succ_id);
                    };
                };
            };
        };

        Dominators { doms }
    }

    pub fn get(&self, block: MilBlockId) -> &BlockDominators {
        &self.doms[&block]
    }
}

impl fmt::Debug for Dominators {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Dominators {{")?;

        for (&block, doms) in self.doms.iter() {
            write!(f, "  {} <- [ ", block)?;

            for dom in doms.blocks.iter() {
                write!(f, "{} ", dom)?;
            };

            writeln!(f, "]")?;
        };

        write!(f, "}}")?;
        Ok(())
    }
}
