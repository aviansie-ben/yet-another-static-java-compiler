use std::collections::HashMap;
use std::fmt;

use crate::resolve::{ClassEnvironment, MethodId, MethodName};
use crate::util::BitVecIndex;

use super::*;

#[derive(Debug, Clone)]
pub struct MilRegisterAllocator {
    next: MilRegister
}

impl MilRegisterAllocator {
    pub fn new() -> MilRegisterAllocator {
        MilRegisterAllocator::new_from(MilRegister(0))
    }

    pub fn new_from(start: MilRegister) -> MilRegisterAllocator {
        MilRegisterAllocator { next: start }
    }

    pub fn allocate_one(&mut self) -> MilRegister {
        self.allocate_many(1)
    }

    pub fn allocate_many(&mut self, n: u32) -> MilRegister {
        let r = self.next;
        self.next.0 += n;
        r
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilLocalId(pub u32);

impl fmt::Display for MilLocalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

impl BitVecIndex for MilLocalId {
    fn into_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(i: usize) -> Self {
        MilLocalId(i as u32)
    }
}

#[derive(Debug, Clone)]
pub struct MilLocalInfo {
    pub java_local: u16,
    pub ty: MilType
}

#[derive(Debug, Clone)]
pub struct MilBlockIdAllocator {
    next: MilBlockId
}

impl MilBlockIdAllocator {
    pub fn new() -> MilBlockIdAllocator {
        MilBlockIdAllocator::new_from(MilBlockId(0))
    }

    pub fn new_from(start: MilBlockId) -> MilBlockIdAllocator {
        MilBlockIdAllocator { next: start }
    }

    pub fn allocate_one(&mut self) -> MilBlockId {
        self.allocate_many(1)
    }

    pub fn allocate_many(&mut self, n: u32) -> MilBlockId {
        let id = self.next;
        self.next.0 += n;
        id
    }

    pub fn next(&self) -> MilBlockId {
        self.next
    }
}

#[derive(Debug, Clone)]
pub struct MilFunction {
    pub id: MethodId,
    pub sig: MilFunctionSignature,
    pub param_regs: Vec<MilRegister>,
    pub reg_alloc: MilRegisterAllocator,
    pub local_info: HashMap<MilLocalId, MilLocalInfo>,
    pub block_alloc: MilBlockIdAllocator,
    pub blocks: HashMap<MilBlockId, MilBlock>,
    pub block_order: Vec<MilBlockId>,
    pub source_file: (String, String),
    pub line_map: MilLineMap,
    pub local_map: Option<MilLocalDebugMap>,
    pub inline_sites: Vec<MilInlineSiteInfo>
}

impl MilFunction {
    pub fn new(id: MethodId, sig: MilFunctionSignature) -> MilFunction {
        let mut reg_alloc = MilRegisterAllocator::new();
        let param_regs = sig.param_types.iter().map(|_| reg_alloc.allocate_one()).collect();

        MilFunction {
            id,
            sig,
            param_regs,
            reg_alloc,
            local_info: HashMap::new(),
            block_alloc: MilBlockIdAllocator::new(),
            blocks: HashMap::new(),
            block_order: vec![],
            source_file: (String::new(), String::new()),
            line_map: MilLineMap::empty(),
            local_map: None,
            inline_sites: vec![]
        }
    }

    pub fn pretty_decl<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilFunctionDecl(self, env)
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilFunction(self, env)
    }
}

struct PrettyMilFunctionDecl<'a>(&'a MilFunction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilFunctionDecl<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyMilFunctionDecl(func, env) = *self;

        write!(f, "func ")?;

        if let Some(return_type) = func.sig.return_type {
            write!(f, "{} ", return_type.pretty(env))?;
        } else {
            write!(f, "void ")?;
        };

        write!(f, "%\"{}\"(", MethodName(func.id, self.1))?;

        let mut first = true;
        for (ty, reg) in func.sig.param_types.iter().zip(func.param_regs.iter().copied()) {
            if !first {
                write!(f, ", ")?;
            } else {
                first = false;
            };

            write!(f, "{} {}", ty.pretty(env), reg)?;
        };

        write!(f, ")")
    }
}

struct PrettyMilFunction<'a>(&'a MilFunction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyMilFunction(func, env) = *self;
        writeln!(f, "{} {{", func.pretty_decl(env))?;

        for block_id in func.block_order.iter().cloned() {
            writeln!(f, "{}", func.blocks[&block_id].pretty(env))?;
        };

        writeln!(f, "}}")?;

        Ok(())
    }
}
