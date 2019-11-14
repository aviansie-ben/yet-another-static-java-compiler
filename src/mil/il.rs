use std::collections::HashMap;
use std::fmt;

use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId};
use crate::static_heap::JavaStaticRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilRegister(pub u32);

impl MilRegister {
    pub const VOID: MilRegister = MilRegister(!0);
}

impl fmt::Display for MilRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &MilRegister::VOID {
            write!(f, "$void")
        } else {
            write!(f, "${}", self.0)
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilRegisterAllocator {
    next: MilRegister
}

impl MilRegisterAllocator {
    pub fn new() -> MilRegisterAllocator {
        MilRegisterAllocator {
            next: MilRegister(0)
        }
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

#[derive(Debug, Clone)]
pub struct MilRegisterInfo {
    pub ty: MilType
}

#[derive(Debug, Clone)]
pub struct MilRegisterMap {
    pub info: HashMap<MilRegister, MilRegisterInfo>
}

impl MilRegisterMap {
    pub fn new() -> MilRegisterMap {
        MilRegisterMap {
            info: HashMap::new()
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilBlockIdAllocator {
    next: MilBlockId
}

impl MilBlockIdAllocator {
    pub fn new() -> MilBlockIdAllocator {
        MilBlockIdAllocator {
            next: MilBlockId(0)
        }
    }

    pub fn allocate_one(&mut self) -> MilBlockId {
        self.allocate_many(1)
    }

    pub fn allocate_many(&mut self, n: u32) -> MilBlockId {
        let id = self.next;
        self.next.0 += n;
        id
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilBlockId(pub u32);

impl MilBlockId {
    pub const ENTRY: MilBlockId = MilBlockId(!0);
    pub const EXIT: MilBlockId = MilBlockId(!0 - 1);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MilType {
    Void,
    Ref,
    Bool,
    Int,
    Long,
    Float,
    Double
}

impl fmt::Display for MilType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MilType::Void => write!(f, "void"),
            MilType::Ref => write!(f, "ref"),
            MilType::Bool => write!(f, "bool"),
            MilType::Int => write!(f, "int"),
            MilType::Long => write!(f, "long"),
            MilType::Float => write!(f, "float"),
            MilType::Double => write!(f, "double")
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilKnownObjectMap<'a> {
    objs: HashMap<MilKnownObjectId, JavaStaticRef<'a>>,
    next: MilKnownObjectId
}

impl <'a> MilKnownObjectMap<'a> {
    pub fn new() -> MilKnownObjectMap<'a> {
        MilKnownObjectMap {
            objs: HashMap::new(),
            next: MilKnownObjectId(0)
        }
    }

    pub fn add(&mut self, obj: JavaStaticRef<'a>) -> MilKnownObjectId {
        let id = self.next;
        self.objs.insert(id, obj);
        self.next.0 += 1;
        id
    }

    pub fn get(&self, id: MilKnownObjectId) -> &JavaStaticRef<'a> {
        &self.objs[&id]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilKnownObjectId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MilOperand {
    Register(MilRegister),
    Null,
    KnownObject(MilKnownObjectId, ClassId),
    Bool(bool),
    Int(i32),
    Long(i64),
    Float(u32),
    Double(u64)
}

impl MilOperand {
    pub fn get_const_type(&self) -> Option<MilType> {
        match *self {
            MilOperand::Register(_) => None,
            MilOperand::Null => Some(MilType::Ref),
            MilOperand::KnownObject(_, _) => Some(MilType::Ref),
            MilOperand::Bool(_) => Some(MilType::Bool),
            MilOperand::Int(_) => Some(MilType::Int),
            MilOperand::Long(_) => Some(MilType::Long),
            MilOperand::Float(_) => Some(MilType::Float),
            MilOperand::Double(_) => Some(MilType::Double)
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilOperand(self, env)
    }
}

struct PrettyMilOperand<'a>(&'a MilOperand, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilOperand<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self.0 {
            MilOperand::Register(reg) => write!(f, "{}", reg),
            MilOperand::Null => write!(f, "ref:null"),
            MilOperand::KnownObject(id, cls) => write!(f, "ref:<obj_{} {}>", id.0, self.1.get(cls).name(self.1)),
            MilOperand::Bool(val) => write!(f, "bool:{}", val),
            MilOperand::Int(val) => write!(f, "int:{}", val),
            MilOperand::Long(val) => write!(f, "long:{}", val),
            MilOperand::Float(val) => write!(f, "float:0x{:x}/{}", val, f32::from_bits(val)),
            MilOperand::Double(val) => write!(f, "double:0x{:x}/{}", val, f64::from_bits(val))
        }
    }
}

#[derive(Debug, Clone)]
pub enum MilInstructionKind {
    Nop,
    Copy(MilRegister, MilOperand),
    GetParam(u16, ClassId, MilRegister),
    GetField(FieldId, ClassId, MilRegister, MilOperand),
    PutField(FieldId, ClassId, MilOperand, MilOperand),
    GetStatic(FieldId, ClassId, MilRegister),
    PutStatic(FieldId, ClassId, MilOperand),
    AllocObj(ClassId, MilRegister),
    AllocArray(ClassId, MilRegister, MilOperand)
}

#[derive(Debug, Clone)]
pub enum MilEndInstructionKind {
    Nop,
    Call(MilType, MethodId, MilRegister, Vec<MilOperand>),
    CallVirtual(MilType, MethodId, MilRegister, MilOperand, Vec<MilOperand>),
    Return(MilOperand),
    Jump(MilBlockId)
}

#[derive(Debug, Clone)]
pub struct MilInstructionWrapper<T> {
    pub kind: T,
    pub bytecode: (u32, u32)
}

pub type MilInstruction = MilInstructionWrapper<MilInstructionKind>;
pub type MilEndInstruction = MilInstructionWrapper<MilEndInstructionKind>;

struct FieldName<'a>(FieldId, &'a ClassEnvironment);

impl <'a> fmt::Display for FieldName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FieldName(field_id, env) = self;
        let class = env.get(field_id.0).as_user_class();
        let field = &class.fields[field_id.1 as usize];

        write!(f, "{}.{} {}", class.meta.name, field.name, field.descriptor)
    }
}

struct MethodName<'a>(MethodId, &'a ClassEnvironment);

impl <'a> fmt::Display for MethodName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MethodName(method_id, env) = self;
        let class = env.get(method_id.0).as_user_class();
        let method = &class.methods[method_id.1 as usize];

        write!(f, "{}.{}{}", class.meta.name, method.name, method.descriptor)
    }
}

pub struct PrettyMilInstruction<'a>(&'a MilInstruction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.kind {
            MilInstructionKind::Nop => {
                write!(f, "nop")?;
            },
            MilInstructionKind::Copy(tgt, ref src) => {
                write!(f, "copy {}, {}", tgt, src.pretty(self.1))?;
            },
            MilInstructionKind::GetParam(n, class_id, tgt) => {
                write!(f, "get_param <{} {}> {}", n, self.1.get(class_id).name(self.1), tgt)?;
            },
            MilInstructionKind::GetField(field_id, _, tgt, ref obj) => {
                write!(f, "get_field <{}> {}, {}", FieldName(field_id, self.1), tgt, obj.pretty(self.1))?;
            },
            MilInstructionKind::PutField(field_id, _, ref obj, ref val) => {
                write!(f, "put_field <{}> {}, {}", FieldName(field_id, self.1), obj.pretty(self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::GetStatic(field_id, _, ref tgt) => {
                write!(f, "get_static <{}> {}", FieldName(field_id, self.1), tgt)?;
            },
            MilInstructionKind::PutStatic(field_id, _, ref val) => {
                write!(f, "put_static <{}> {}", FieldName(field_id, self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::AllocObj(class_id, tgt) => {
                write!(f, "alloc_obj <{}> {}", self.1.get(class_id).name(self.1), tgt)?;
            },
            MilInstructionKind::AllocArray(class_id, tgt, ref len) => {
                write!(f, "alloc_array <{}> {}, {}", self.1.get(class_id).name(self.1), tgt, len.pretty(self.1))?;
            }
        }

        Ok(())
    }
}

impl MilInstruction {
    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilInstruction(self, env)
    }
}

pub struct PrettyMilEndInstruction<'a>(&'a MilEndInstruction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilEndInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.kind {
            MilEndInstructionKind::Nop => {
                write!(f, "nop")?;
            },
            MilEndInstructionKind::Call(ty, method_id, tgt, ref args) => {
                write!(f, "call_{} <{}> {}", ty, MethodName(method_id, self.1), tgt)?;

                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallVirtual(ty, method_id, tgt, ref obj, ref args) => {
                write!(f, "call_virtual_{} <{}> {}, {}", ty, MethodName(method_id, self.1), tgt, obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::Return(ref val) => {
                write!(f, "ret {}", val.pretty(self.1))?;
            },
            MilEndInstructionKind::Jump(block) => {
                write!(f, "jump {}", block)?;
            }
        }

        Ok(())
    }
}

impl MilEndInstruction {
    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilEndInstruction(self, env)
    }

    pub fn is_nop(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Nop => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilBlock {
    pub id: MilBlockId,
    pub instrs: Vec<MilInstruction>,
    pub end_instr: MilEndInstruction,
    pub exception_successors: Vec<MilBlockId>
}

impl MilBlock {
    pub fn new() -> MilBlock {
        MilBlock {
            id: MilBlockId::ENTRY,
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Nop,
                bytecode: (!0, !0)
            },
            exception_successors: vec![]
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilBlock(self, env)
    }
}

struct PrettyMilBlock<'a>(&'a MilBlock, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0.id)?;

        for instr in self.0.instrs.iter() {
            write!(f, "\n  {}", instr.pretty(self.1))?;
        };

        if !self.0.end_instr.is_nop() {
            write!(f, "\n  {}", self.0.end_instr.pretty(self.1))?;
        };

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MilFunction {
    pub reg_alloc: MilRegisterAllocator,
    pub reg_map: MilRegisterMap,
    pub block_alloc: MilBlockIdAllocator,
    pub blocks: HashMap<MilBlockId, MilBlock>,
    pub block_order: Vec<MilBlockId>
}

impl MilFunction {
    pub fn new() -> MilFunction {
        MilFunction {
            reg_alloc: MilRegisterAllocator::new(),
            reg_map: MilRegisterMap::new(),
            block_alloc: MilBlockIdAllocator::new(),
            blocks: HashMap::new(),
            block_order: vec![]
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilFunction(self, env)
    }
}

struct PrettyMilFunction<'a>(&'a MilFunction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for block_id in self.0.block_order.iter().cloned() {
            writeln!(f, "{}", self.0.blocks[&block_id].pretty(self.1))?;
        };

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MilProgram {
    pub funcs: HashMap<MethodId, MilFunction>
}

impl MilProgram {
    pub fn new() -> MilProgram {
        MilProgram {
            funcs: HashMap::new()
        }
    }
}
