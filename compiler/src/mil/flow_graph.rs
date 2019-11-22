use std::collections::HashMap;
use std::hash::Hash;

use itertools::Itertools;
use smallvec::SmallVec;

use super::il::*;

#[derive(Debug, Clone)]
pub struct FlowGraphNode<T: Copy + PartialEq + Eq + Hash> {
    pub id: T,
    pub incoming: Vec<T>,
    pub outgoing: Vec<T>
}

impl <T: Copy + PartialEq + Eq + Hash> FlowGraphNode<T> {
    pub fn new(id: T) -> FlowGraphNode<T> {
        FlowGraphNode {
            id,
            incoming: vec![],
            outgoing: vec![]
        }
    }
}

#[derive(Debug, Clone)]
pub struct FlowGraph<T: Copy + PartialEq + Eq + Hash> {
    nodes: HashMap<T, FlowGraphNode<T>>
}

impl <T: Copy + PartialEq + Eq + Hash> FlowGraph<T> {
    pub fn new() -> FlowGraph<T> {
        FlowGraph { nodes: HashMap::new() }
    }

    pub fn get(&self, id: T) -> &FlowGraphNode<T> {
        self.nodes.get(&id).unwrap()
    }

    pub fn get_mut(&mut self, id: T) -> &mut FlowGraphNode<T> {
        self.nodes.get_mut(&id).unwrap()
    }

    pub fn add_node(&mut self, id: T) {
        self.nodes.insert(id, FlowGraphNode::new(id));
    }

    pub fn remove_node(&mut self, id: T) {
        let node = self.nodes.remove(&id).unwrap();

        for from in node.incoming {
            self.get_mut(from).outgoing.remove_item(&id);
        };

        for to in node.outgoing {
            self.get_mut(to).outgoing.remove_item(&id);
        };
    }

    pub fn add_edge(&mut self, from: T, to: T) {
        self.get_mut(from).outgoing.push(to);
        self.get_mut(to).incoming.push(from);
    }

    pub fn remove_edge(&mut self, from: T, to: T) {
        self.get_mut(from).outgoing.remove_item(&to);
        self.get_mut(to).incoming.remove_item(&from);
    }

    pub fn clear(&mut self) {
        self.nodes.clear();
    }
}

impl FlowGraph<MilBlockId> {
    pub fn for_function(func: &MilFunction) -> FlowGraph<MilBlockId> {
        let mut cfg = FlowGraph::new();

        cfg.add_node(MilBlockId::ENTRY);
        cfg.add_node(MilBlockId::EXIT);

        for block_id in func.block_order.iter().cloned() {
            cfg.add_node(block_id);
        };

        cfg.add_edge(MilBlockId::ENTRY, func.block_order[0]);

        for (prev_block_id, next_block_id) in func.block_order.iter().cloned().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
            let prev_block = &func.blocks[&prev_block_id];

            if prev_block.end_instr.can_fall_through() {
                cfg.add_edge(prev_block_id, next_block_id);
            };

            match prev_block.end_instr.kind {
                MilEndInstructionKind::Return(_) => {
                    cfg.add_edge(prev_block_id, MilBlockId::EXIT);
                },
                MilEndInstructionKind::Jump(target) => {
                    cfg.add_edge(prev_block_id, target);
                },
                MilEndInstructionKind::JumpIf(_, target, _, _) => {
                    cfg.add_edge(prev_block_id, target);
                },
                _ => {}
            };

            for catch_block in prev_block.exception_successors.iter().cloned() {
                cfg.add_edge(prev_block_id, catch_block);
            };
        };

        cfg
    }

    pub fn get_entry(&self) -> &FlowGraphNode<MilBlockId> {
        self.get(MilBlockId::ENTRY)
    }

    pub fn get_entry_mut(&mut self) -> &mut FlowGraphNode<MilBlockId> {
        self.get_mut(MilBlockId::ENTRY)
    }

    pub fn get_exit(&self) -> &FlowGraphNode<MilBlockId> {
        self.get(MilBlockId::EXIT)
    }

    pub fn get_exit_mut(&mut self) -> &mut FlowGraphNode<MilBlockId> {
        self.get_mut(MilBlockId::EXIT)
    }
}
