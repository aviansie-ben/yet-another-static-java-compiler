use std::collections::HashMap;
use std::fmt;
use std::hash::Hash;

use itertools::Itertools;

use super::il::*;

#[derive(Clone)]
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

#[derive(Clone)]
pub struct FlowGraph<T: Copy + PartialEq + Eq + Hash> {
    nodes: HashMap<T, FlowGraphNode<T>>
}

impl <T: Copy + PartialEq + Eq + Hash> FlowGraph<T> {
    pub fn new() -> FlowGraph<T> {
        FlowGraph { nodes: HashMap::new() }
    }

    pub fn try_get(&self, id: T) -> Option<&FlowGraphNode<T>> {
        self.nodes.get(&id)
    }

    pub fn get(&self, id: T) -> &FlowGraphNode<T> {
        self.try_get(id).unwrap()
    }

    pub fn try_get_mut(&mut self, id: T) -> Option<&mut FlowGraphNode<T>> {
        self.nodes.get_mut(&id)
    }

    pub fn get_mut(&mut self, id: T) -> &mut FlowGraphNode<T> {
        self.try_get_mut(id).unwrap()
    }

    pub fn add_node(&mut self, id: T) {
        self.nodes.insert(id, FlowGraphNode::new(id));
    }

    pub fn remove_node(&mut self, id: T) {
        let node = self.nodes.remove(&id).unwrap();

        for from in node.incoming {
            if from != id {
                let from_node = self.get_mut(from);
                from_node.outgoing.remove(from_node.outgoing.iter().position(|&succ| succ == id).unwrap());
            };
        };

        for to in node.outgoing {
            if to != id {
                let to_node = self.get_mut(to);
                to_node.incoming.remove(to_node.incoming.iter().position(|&pred| pred == id).unwrap());
            };
        };
    }

    pub fn add_edge(&mut self, from: T, to: T) {
        self.get_mut(from).outgoing.push(to);
        self.get_mut(to).incoming.push(from);
    }

    pub fn remove_edge(&mut self, from: T, to: T) {
        let from_node = self.get_mut(from);
        from_node.outgoing.remove(from_node.outgoing.iter().position(|&succ| succ == to).unwrap());

        let to_node = self.get_mut(to);
        to_node.incoming.remove(to_node.incoming.iter().position(|&pred| pred == from).unwrap());
    }

    pub fn merge_nodes_back(&mut self, from: T, to: T) {
        let to_node = self.nodes.remove(&to).unwrap();

        for succ in to_node.outgoing.iter().copied() {
            let succ = self.get_mut(succ);
            let i = succ.incoming.iter().enumerate().filter(|(_, &id)| id == to).map(|(i, _)| i).next().unwrap();
            succ.incoming[i] = from;
        };

        self.get_mut(from).outgoing = to_node.outgoing;
    }

    pub fn merge_nodes_forward(&mut self, from: T, to: T) {
        self.remove_edge(from, to);

        let from_node = self.nodes.remove(&from).unwrap();

        for pred in from_node.incoming.iter().copied() {
            let pred = self.get_mut(pred);
            let i = pred.outgoing.iter().enumerate().filter(|(_, &id)| id == from).map(|(i, _)| i).next().unwrap();
            pred.outgoing[i] = to;
        };

        self.get_mut(to).incoming.extend(from_node.incoming);
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
                MilEndInstructionKind::JumpIfICmp(_, target, _, _) | MilEndInstructionKind::JumpIfRCmp(_, target, _, _) => {
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

impl <T: Copy + PartialEq + Eq + Hash + fmt::Debug> fmt::Debug for FlowGraphNode<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        if let Some(first_incoming) = self.incoming.first() {
            write!(f, "{:?}", first_incoming)?;
            for incoming in self.incoming[1..].iter() {
                write!(f, ", {:?}", incoming)?;
            };
        };
        write!(f, "}} -> {:?} -> {{", self.id)?;
        if let Some(first_outgoing) = self.outgoing.first() {
            write!(f, "{:?}", first_outgoing)?;
            for outgoing in self.outgoing[1..].iter() {
                write!(f, ", {:?}", outgoing)?;
            };
        };
        write!(f, "}}")?;
        Ok(())
    }
}

impl <T: Copy + PartialEq + Eq + Hash + fmt::Debug> fmt::Debug for FlowGraph<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "FlowGraph ")?;
        f.debug_set().entries(self.nodes.values()).finish()
    }
}
