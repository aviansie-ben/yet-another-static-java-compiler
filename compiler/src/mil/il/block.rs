use std::fmt;

use crate::resolve::ClassEnvironment;
use crate::util::BitVecIndex;

use super::*;

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilBlockId(pub u32);

impl MilBlockId {
    pub const ENTRY: MilBlockId = MilBlockId(!0);
    pub const EXIT: MilBlockId = MilBlockId(!0 - 1);
}

impl fmt::Debug for MilBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for MilBlockId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &MilBlockId::ENTRY {
            write!(f, "(entry)")
        } else if self == &MilBlockId::EXIT {
            write!(f, "(exit)")
        } else {
            write!(f, "L{}", self.0)
        }
    }
}

impl BitVecIndex for MilBlockId {
    fn into_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(i: usize) -> Self {
        MilBlockId(i as u32)
    }
}

#[derive(Debug, Clone)]
pub struct MilBlock {
    pub id: MilBlockId,
    pub phi_nodes: Vec<MilPhiNode>,
    pub instrs: Vec<MilInstruction>,
    pub end_instr: MilEndInstruction,
    pub exception_successors: Vec<MilBlockId>
}

impl MilBlock {
    pub fn new() -> MilBlock {
        MilBlock {
            id: MilBlockId::ENTRY,
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Nop,
                bytecode: (!0, !0)
            },
            exception_successors: vec![]
        }
    }

    pub fn initial_bytecode(&self) -> (u32, u32) {
        self.instrs.first().map_or(self.end_instr.bytecode, |instr| instr.bytecode)
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilBlock(self, env)
    }
}

struct PrettyMilBlock<'a>(&'a MilBlock, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0.id)?;

        for phi in self.0.phi_nodes.iter() {
            write!(f, "\n  {}", phi.pretty(self.1))?;
        };

        for instr in self.0.instrs.iter() {
            write!(f, "\n  {}", instr.pretty(self.1))?;
        };

        write!(f, "\n  {}", self.0.end_instr.pretty(self.1))?;

        Ok(())
    }
}
