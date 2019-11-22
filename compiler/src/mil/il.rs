use std::collections::HashMap;
use std::fmt;

use smallvec::SmallVec;

use crate::bytecode::BytecodeCondition;
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilLocalId(pub u32);

impl fmt::Display for MilLocalId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct MilLocalInfo {
    pub java_local: u16,
    pub ty: MilType
}

#[derive(Debug, Clone)]
pub struct MilRegisterInfo {
    pub ty: MilType
}

#[derive(Debug, Clone)]
pub struct MilRegisterMap {
    pub info: HashMap<MilRegister, MilRegisterInfo>,
    pub local_info: HashMap<MilLocalId, MilLocalInfo>
}

impl MilRegisterMap {
    pub fn new() -> MilRegisterMap {
        MilRegisterMap {
            info: HashMap::new(),
            local_info: HashMap::new()
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

impl MilType {
    pub fn for_class(class_id: ClassId) -> MilType {
        match class_id {
            ClassId::PRIMITIVE_VOID => MilType::Void,
            ClassId::PRIMITIVE_BYTE => MilType::Int,
            ClassId::PRIMITIVE_CHAR => MilType::Int,
            ClassId::PRIMITIVE_DOUBLE => MilType::Double,
            ClassId::PRIMITIVE_FLOAT => MilType::Float,
            ClassId::PRIMITIVE_INT => MilType::Int,
            ClassId::PRIMITIVE_LONG => MilType::Long,
            ClassId::PRIMITIVE_SHORT => MilType::Int,
            ClassId::PRIMITIVE_BOOLEAN => MilType::Int,
            _ => MilType::Ref
        }
    }
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
pub struct MilKnownObjectRefs {
    pub classes: HashMap<ClassId, MilKnownObjectId>,
    pub strings: Vec<MilKnownObjectId>
}

impl MilKnownObjectRefs {
    pub fn new() -> MilKnownObjectRefs {
        MilKnownObjectRefs {
            classes: HashMap::new(),
            strings: vec![]
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilKnownObjectMap<'a> {
    pub refs: MilKnownObjectRefs,
    objs: HashMap<MilKnownObjectId, JavaStaticRef<'a>>,
    next: MilKnownObjectId
}

impl <'a> MilKnownObjectMap<'a> {
    pub fn new() -> MilKnownObjectMap<'a> {
        MilKnownObjectMap {
            refs: MilKnownObjectRefs::new(),
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

    pub fn get_type(&self, reg_map: &MilRegisterMap) -> MilType {
        match *self {
            MilOperand::Register(reg) => reg_map.info[&reg].ty,
            _ => self.get_const_type().unwrap()
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilComparison {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le
}

impl MilComparison {
    pub fn from_bytecode(cond: BytecodeCondition) -> MilComparison {
        match cond {
            BytecodeCondition::Eq => MilComparison::Eq,
            BytecodeCondition::Ne => MilComparison::Ne,
            BytecodeCondition::Gt => MilComparison::Gt,
            BytecodeCondition::Lt => MilComparison::Lt,
            BytecodeCondition::Ge => MilComparison::Ge,
            BytecodeCondition::Le => MilComparison::Le
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilBinOp {
    IAdd
}

impl fmt::Display for MilBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MilBinOp::IAdd => write!(f, "iadd")
        }
    }
}

#[derive(Debug, Clone)]
pub enum MilInstructionKind {
    Nop,
    Copy(MilRegister, MilOperand),
    BinOp(MilBinOp, MilRegister, MilOperand, MilOperand),
    GetParam(u16, ClassId, MilRegister),
    GetLocal(MilLocalId, MilRegister),
    SetLocal(MilLocalId, MilOperand),
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
    Call(ClassId, MethodId, MilRegister, Vec<MilOperand>),
    CallVirtual(ClassId, MethodId, MilRegister, MilOperand, Vec<MilOperand>),
    CallNative(ClassId, String, MilRegister, Vec<MilOperand>),
    Return(MilOperand),
    Jump(MilBlockId),
    JumpIf(MilComparison, MilBlockId, MilOperand, MilOperand)
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
        let FieldName(field_id, env) = *self;
        let (class, field) = env.get_field(field_id);

        write!(f, "{}.{} {}", class.meta.name, field.name, field.descriptor)
    }
}

struct MethodName<'a>(MethodId, &'a ClassEnvironment);

impl <'a> fmt::Display for MethodName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let MethodName(method_id, env) = *self;
        let (class, method) = env.get_method(method_id);

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
            MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                write!(f, "{} {}, {}, {}", op, tgt, lhs.pretty(self.1), rhs.pretty(self.1))?;
            },
            MilInstructionKind::GetParam(n, class_id, tgt) => {
                write!(f, "get_param <{} {}> {}", n, self.1.get(class_id).name(self.1), tgt)?;
            },
            MilInstructionKind::GetLocal(local_id, tgt) => {
                write!(f, "get_local <{}> {}", local_id, tgt)?;
            },
            MilInstructionKind::SetLocal(local_id, ref src) => {
                write!(f, "set_local <{}> {}", local_id, src.pretty(self.1))?;
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

    pub fn target(&self) -> Option<&MilRegister> {
        match self.kind {
            MilInstructionKind::Nop => None,
            MilInstructionKind::Copy(ref tgt, _) => Some(tgt),
            MilInstructionKind::BinOp(_, ref tgt, _, _) => Some(tgt),
            MilInstructionKind::GetParam(_, _, ref tgt) => Some(tgt),
            MilInstructionKind::GetLocal(_, ref tgt) => Some(tgt),
            MilInstructionKind::SetLocal(_, _) => None,
            MilInstructionKind::GetField(_, _, ref tgt, _) => Some(tgt),
            MilInstructionKind::PutField(_, _, _, _) => None,
            MilInstructionKind::GetStatic(_, _, ref tgt) => Some(tgt),
            MilInstructionKind::PutStatic(_, _, _) => None,
            MilInstructionKind::AllocObj(_, ref tgt) => Some(tgt),
            MilInstructionKind::AllocArray(_, ref tgt, _) => Some(tgt)
        }
    }

    pub fn for_operands(&self, mut f: impl FnMut (&MilOperand) -> ()) {
        match self.kind {
            MilInstructionKind::Nop => {},
            MilInstructionKind::Copy(_, ref val) => {
                f(val);
            },
            MilInstructionKind::BinOp(_, _, ref lhs, ref rhs) => {
                f(lhs);
                f(rhs);
            },
            MilInstructionKind::GetParam(_, _, _) => {}
            MilInstructionKind::GetLocal(_, _) => {},
            MilInstructionKind::SetLocal(_, ref val) => {
                f(val);
            },
            MilInstructionKind::GetField(_, _, _, ref obj) => {
                f(obj);
            },
            MilInstructionKind::PutField(_, _, ref obj, ref val) => {
                f(val);
            },
            MilInstructionKind::GetStatic(_, _, _) => {},
            MilInstructionKind::PutStatic(_, _, ref val) => {
                f(val);
            },
            MilInstructionKind::AllocObj(_, _) => {},
            MilInstructionKind::AllocArray(_, _, ref len) => {
                f(len);
            }
        };
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
                write!(f, "call_{} <{}> {}", MilType::for_class(ty), MethodName(method_id, self.1), tgt)?;

                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallVirtual(ty, method_id, tgt, ref obj, ref args) => {
                write!(f, "call_virtual_{} <{}> {}, {}", MilType::for_class(ty), MethodName(method_id, self.1), tgt, obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallNative(ty, ref name, tgt, ref args) => {
                write!(f, "call_native_{} <{}> {}", MilType::for_class(ty), name, tgt)?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::Return(ref val) => {
                write!(f, "ret {}", val.pretty(self.1))?;
            },
            MilEndInstructionKind::Jump(block) => {
                write!(f, "j {}", block)?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Eq, block, ref src1, ref src2) => {
                write!(f, "jeq {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Ne, block, ref src1, ref src2) => {
                write!(f, "jne {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Gt, block, ref src1, ref src2) => {
                write!(f, "jgt {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Lt, block, ref src1, ref src2) => {
                write!(f, "jlt {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Ge, block, ref src1, ref src2) => {
                write!(f, "jge {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            },
            MilEndInstructionKind::JumpIf(MilComparison::Le, block, ref src1, ref src2) => {
                write!(f, "jle {}, {}, {}", block, src1.pretty(self.1), src2.pretty(self.1))?;
            }
        }

        Ok(())
    }
}

impl MilEndInstruction {
    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilEndInstruction(self, env)
    }

    pub fn target(&self) -> Option<&MilRegister> {
        match self.kind {
            MilEndInstructionKind::Nop => None,
            MilEndInstructionKind::Call(_, _, ref tgt, _) => Some(tgt),
            MilEndInstructionKind::CallVirtual(_, _, ref tgt, _, _) => Some(tgt),
            MilEndInstructionKind::CallNative(_, _, ref tgt, _) => Some(tgt),
            MilEndInstructionKind::Return(_) => None,
            MilEndInstructionKind::Jump(_) => None,
            MilEndInstructionKind::JumpIf(_, _, _, _) => None
        }
    }

    pub fn for_operands(&self, mut f: impl FnMut (&MilOperand) -> ()) {
        match self.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Call(_, _, _, ref args) => {
                for a in args.iter() {
                    f(a);
                };
            },
            MilEndInstructionKind::CallVirtual(_, _, _, ref obj, ref args) => {
                f(obj);
                for a in args.iter() {
                    f(a);
                };
            },
            MilEndInstructionKind::CallNative(_, _, _, ref args) => {
                for a in args.iter() {
                    f(a);
                };
            },
            MilEndInstructionKind::Return(ref val) => {
                f(val);
            },
            MilEndInstructionKind::Jump(_) => {},
            MilEndInstructionKind::JumpIf(_, _, ref lhs, ref rhs) => {
                f(lhs);
                f(rhs);
            }
        };
    }

    pub fn can_fall_through(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Nop => true,
            MilEndInstructionKind::Call(_, _, _, _) => true,
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => true,
            MilEndInstructionKind::CallNative(_, _, _, _) => true,
            MilEndInstructionKind::Return(_) => false,
            MilEndInstructionKind::Jump(_) => false,
            MilEndInstructionKind::JumpIf(_, _, _, _) => true
        }
    }

    pub fn is_nop(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Nop => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilPhiNode {
    pub target: MilRegister,
    pub sources: SmallVec<[(MilRegister, MilBlockId); 2]>
}

impl fmt::Display for MilPhiNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "phi {}", self.target)?;

        for (src, pred) in self.sources.iter().cloned() {
            write!(f, ", {}:{}", pred, src)?;
        };

        Result::Ok(())
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

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilBlock(self, env)
    }
}

struct PrettyMilBlock<'a>(&'a MilBlock, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilBlock<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:", self.0.id)?;

        for phi in self.0.phi_nodes.iter() {
            write!(f, "\n  {}", phi)?;
        };

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
    pub id: MethodId,
    pub reg_alloc: MilRegisterAllocator,
    pub reg_map: MilRegisterMap,
    pub block_alloc: MilBlockIdAllocator,
    pub blocks: HashMap<MilBlockId, MilBlock>,
    pub block_order: Vec<MilBlockId>
}

impl MilFunction {
    pub fn new(id: MethodId) -> MilFunction {
        MilFunction {
            id,
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
pub struct MilProgram<'a> {
    pub funcs: HashMap<MethodId, MilFunction>,
    pub known_objects: MilKnownObjectMap<'a>,
    pub main_method: MethodId
}

impl <'a> MilProgram<'a> {
    pub fn new(known_objects: MilKnownObjectMap, main_method: MethodId) -> MilProgram {
        MilProgram {
            funcs: HashMap::new(),
            known_objects,
            main_method
        }
    }
}
