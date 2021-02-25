use std::fmt;

use smallvec::{smallvec, SmallVec};

use crate::bytecode::BytecodeCondition;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, FieldName, MethodName, MethodId};

use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MilPhiNode {
    pub target: MilRegister,
    pub ty: MilType,
    pub sources: SmallVec<[(MilOperand, MilBlockId); 2]>,
    pub bytecode: (u32, u32)
}

impl MilPhiNode {
    pub fn dummy() -> Self {
        MilPhiNode {
            target: MilRegister::VOID,
            ty: MilType::Int,
            sources: smallvec![],
            bytecode: (!0, !0)
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilPhiNode(self, env)
    }

    pub fn for_registers(&self, mut f: impl FnMut(MilRegister) -> ()) {
        f(self.target);

        for &(ref op, _) in self.sources.iter() {
            if let MilOperand::Register(_, reg) = *op {
                f(reg);
            };
        };
    }
}

struct PrettyMilPhiNode<'a>(&'a MilPhiNode, &'a ClassEnvironment);

impl fmt::Display for PrettyMilPhiNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "phi {} {}", self.0.ty, self.0.target)?;

        for (src, pred) in self.0.sources.iter().cloned() {
            write!(f, ", {}:{}", pred, src.pretty(self.1))?;
        };

        write!(f, " @bc ")?;

        if self.0.bytecode.0 != !0 {
            write!(f, "{}:", self.0.bytecode.0)?;
        };

        if self.0.bytecode.1 == !0 {
            write!(f, "prologue")?;
        } else {
            write!(f, "{}", self.0.bytecode.1)?;
        };

        Result::Ok(())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilRefComparison {
    Eq,
    Ne
}

impl MilRefComparison {
    pub fn from_bytecode(cond: BytecodeCondition) -> MilRefComparison {
        match cond {
            BytecodeCondition::Eq => MilRefComparison::Eq,
            BytecodeCondition::Ne => MilRefComparison::Ne,
            _ => unreachable!()
        }
    }

    pub fn reverse(&self) -> MilRefComparison {
        match *self {
            MilRefComparison::Eq => MilRefComparison::Ne,
            MilRefComparison::Ne => MilRefComparison::Eq
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            MilRefComparison::Eq => "eq",
            MilRefComparison::Ne => "ne"
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilIntComparison {
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le
}

impl MilIntComparison {
    pub fn from_bytecode(cond: BytecodeCondition) -> MilIntComparison {
        match cond {
            BytecodeCondition::Eq => MilIntComparison::Eq,
            BytecodeCondition::Ne => MilIntComparison::Ne,
            BytecodeCondition::Gt => MilIntComparison::Gt,
            BytecodeCondition::Lt => MilIntComparison::Lt,
            BytecodeCondition::Ge => MilIntComparison::Ge,
            BytecodeCondition::Le => MilIntComparison::Le
        }
    }

    pub fn reverse(&self) -> MilIntComparison {
        match *self {
            MilIntComparison::Eq => MilIntComparison::Ne,
            MilIntComparison::Ne => MilIntComparison::Eq,
            MilIntComparison::Gt => MilIntComparison::Le,
            MilIntComparison::Lt => MilIntComparison::Ge,
            MilIntComparison::Ge => MilIntComparison::Lt,
            MilIntComparison::Le => MilIntComparison::Gt
        }
    }

    pub fn flip_order(&self) -> MilIntComparison {
        match *self {
            MilIntComparison::Eq => MilIntComparison::Eq,
            MilIntComparison::Ne => MilIntComparison::Ne,
            MilIntComparison::Gt => MilIntComparison::Lt,
            MilIntComparison::Lt => MilIntComparison::Gt,
            MilIntComparison::Ge => MilIntComparison::Le,
            MilIntComparison::Le => MilIntComparison::Ge
        }
    }

    pub fn name(&self) -> &'static str {
        match *self {
            MilIntComparison::Eq => "eq",
            MilIntComparison::Ne => "ne",
            MilIntComparison::Gt => "gt",
            MilIntComparison::Lt => "lt",
            MilIntComparison::Ge => "ge",
            MilIntComparison::Le => "le"
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilUnOp {
    ZNot,
    INeg,
    IExtB,
    IExtS,
    LNeg,
    FNeg,
    DNeg,
    I2L,
    I2D,
    I2F,
    L2I,
    L2F,
    L2D,
    F2I,
    F2L,
    F2D,
    D2I,
    D2L,
    D2F,
    GetVTable,
    GetArrayLength
}

impl MilUnOp {
    pub fn type_sig(self) -> (MilType, MilType) {
        match self {
            MilUnOp::ZNot => (MilType::Bool, MilType::Bool),
            MilUnOp::INeg => (MilType::Int, MilType::Int),
            MilUnOp::IExtB => (MilType::Int, MilType::Int),
            MilUnOp::IExtS => (MilType::Int, MilType::Int),
            MilUnOp::LNeg => (MilType::Long, MilType::Long),
            MilUnOp::FNeg => (MilType::Float, MilType::Float),
            MilUnOp::DNeg => (MilType::Double, MilType::Double),
            MilUnOp::I2L => (MilType::Long, MilType::Long),
            MilUnOp::I2F => (MilType::Float, MilType::Int),
            MilUnOp::I2D => (MilType::Double, MilType::Int),
            MilUnOp::L2I => (MilType::Int, MilType::Long),
            MilUnOp::L2F => (MilType::Float, MilType::Long),
            MilUnOp::L2D => (MilType::Double, MilType::Long),
            MilUnOp::F2I => (MilType::Int, MilType::Float),
            MilUnOp::F2L => (MilType::Long, MilType::Float),
            MilUnOp::F2D => (MilType::Double, MilType::Float),
            MilUnOp::D2I => (MilType::Int, MilType::Double),
            MilUnOp::D2L => (MilType::Long, MilType::Double),
            MilUnOp::D2F => (MilType::Float, MilType::Double),
            MilUnOp::GetVTable => (MilType::Addr, MilType::Ref),
            MilUnOp::GetArrayLength => (MilType::Int, MilType::Ref)
        }
    }
}

impl fmt::Display for MilUnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MilUnOp::ZNot => write!(f, "znot"),
            MilUnOp::INeg => write!(f, "ineg"),
            MilUnOp::IExtB => write!(f, "iextb"),
            MilUnOp::IExtS => write!(f, "iexts"),
            MilUnOp::LNeg => write!(f, "lneg"),
            MilUnOp::FNeg => write!(f, "fneg"),
            MilUnOp::DNeg => write!(f, "dneg"),
            MilUnOp::I2L => write!(f, "i2l"),
            MilUnOp::I2F => write!(f, "i2f"),
            MilUnOp::I2D => write!(f, "i2d"),
            MilUnOp::L2I => write!(f, "l2i"),
            MilUnOp::L2F => write!(f, "l2f"),
            MilUnOp::L2D => write!(f, "l2d"),
            MilUnOp::F2I => write!(f, "f2i"),
            MilUnOp::F2L => write!(f, "f2l"),
            MilUnOp::F2D => write!(f, "f2d"),
            MilUnOp::D2I => write!(f, "d2i"),
            MilUnOp::D2L => write!(f, "d2l"),
            MilUnOp::D2F => write!(f, "d2f"),
            MilUnOp::GetVTable => write!(f, "get_vtable"),
            MilUnOp::GetArrayLength => write!(f, "get_array_length")
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilFCmpMode {
    L,
    G
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum MilBinOp {
    IAdd,
    ISub,
    IMul,
    IDivS,
    IRemS,
    IAnd,
    IOr,
    IXor,
    IShrS,
    IShrU,
    IShl,
    ICmp(MilIntComparison),
    LAdd,
    LSub,
    LMul,
    LDivS,
    LRemS,
    LAnd,
    LOr,
    LXor,
    LShrS,
    LShrU,
    LShl,
    LCmp,
    FAdd,
    FSub,
    FMul,
    FDiv,
    FRem,
    FCmp(MilFCmpMode),
    DAdd,
    DSub,
    DMul,
    DDiv,
    DRem,
    DCmp(MilFCmpMode),
    RCmp(MilRefComparison),
    IsSubclass
}

impl MilBinOp {
    pub fn type_sig(self) -> (MilType, MilType, MilType) {
        match self {
            MilBinOp::IAdd => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::ISub => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IMul => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IDivS => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IRemS => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IAnd => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IOr => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IXor => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IShrS => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IShrU => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::IShl => (MilType::Int, MilType::Int, MilType::Int),
            MilBinOp::ICmp(_) => (MilType::Bool, MilType::Int, MilType::Int),
            MilBinOp::LAdd => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LSub => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LMul => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LDivS => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LRemS => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LAnd => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LOr => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LXor => (MilType::Long, MilType::Long, MilType::Long),
            MilBinOp::LShrS => (MilType::Long, MilType::Long, MilType::Int),
            MilBinOp::LShrU => (MilType::Long, MilType::Long, MilType::Int),
            MilBinOp::LShl => (MilType::Long, MilType::Long, MilType::Int),
            MilBinOp::LCmp => (MilType::Int, MilType::Long, MilType::Long),
            MilBinOp::FAdd => (MilType::Float, MilType::Float, MilType::Float),
            MilBinOp::FSub => (MilType::Float, MilType::Float, MilType::Float),
            MilBinOp::FMul => (MilType::Float, MilType::Float, MilType::Float),
            MilBinOp::FDiv => (MilType::Float, MilType::Float, MilType::Float),
            MilBinOp::FRem => (MilType::Float, MilType::Float, MilType::Float),
            MilBinOp::FCmp(_) => (MilType::Int, MilType::Float, MilType::Float),
            MilBinOp::DAdd => (MilType::Double, MilType::Double, MilType::Double),
            MilBinOp::DSub => (MilType::Double, MilType::Double, MilType::Double),
            MilBinOp::DMul => (MilType::Double, MilType::Double, MilType::Double),
            MilBinOp::DDiv => (MilType::Double, MilType::Double, MilType::Double),
            MilBinOp::DRem => (MilType::Double, MilType::Double, MilType::Double),
            MilBinOp::DCmp(_) => (MilType::Int, MilType::Double, MilType::Double),
            MilBinOp::RCmp(_) => (MilType::Bool, MilType::Ref, MilType::Ref),
            MilBinOp::IsSubclass => (MilType::Bool, MilType::Addr, MilType::Addr)
        }
    }

    pub fn get_commuted_op(self) -> Option<MilBinOp> {
        match self {
            MilBinOp::IAdd => Some(MilBinOp::IAdd),
            MilBinOp::ISub => None,
            MilBinOp::IMul => Some(MilBinOp::IMul),
            MilBinOp::IDivS => None,
            MilBinOp::IRemS => None,
            MilBinOp::IAnd => Some(MilBinOp::IAnd),
            MilBinOp::IOr => Some(MilBinOp::IOr),
            MilBinOp::IXor => Some(MilBinOp::IXor),
            MilBinOp::IShrS => None,
            MilBinOp::IShrU => None,
            MilBinOp::IShl => None,
            MilBinOp::ICmp(cmp) => Some(MilBinOp::ICmp(cmp.flip_order())),
            MilBinOp::LAdd => Some(MilBinOp::LAdd),
            MilBinOp::LSub => None,
            MilBinOp::LMul => Some(MilBinOp::LMul),
            MilBinOp::LDivS => None,
            MilBinOp::LRemS => None,
            MilBinOp::LAnd => Some(MilBinOp::LAnd),
            MilBinOp::LOr => Some(MilBinOp::LOr),
            MilBinOp::LXor => Some(MilBinOp::LXor),
            MilBinOp::LShrS => None,
            MilBinOp::LShrU => None,
            MilBinOp::LShl => None,
            MilBinOp::LCmp => None,
            MilBinOp::FAdd => Some(MilBinOp::FAdd),
            MilBinOp::FSub => None,
            MilBinOp::FMul => Some(MilBinOp::FMul),
            MilBinOp::FDiv => None,
            MilBinOp::FRem => None,
            MilBinOp::FCmp(_) => None,
            MilBinOp::DAdd => Some(MilBinOp::DAdd),
            MilBinOp::DSub => None,
            MilBinOp::DMul => Some(MilBinOp::DMul),
            MilBinOp::DDiv => None,
            MilBinOp::DCmp(_) => None,
            MilBinOp::DRem => None,
            MilBinOp::RCmp(cmp) => Some(MilBinOp::RCmp(cmp)),
            MilBinOp::IsSubclass => None
        }
    }

    pub fn is_associative(self) -> bool {
        match self {
            MilBinOp::IAdd => true,
            MilBinOp::ISub => false,
            MilBinOp::IMul => true,
            MilBinOp::IDivS => false,
            MilBinOp::IRemS => false,
            MilBinOp::IAnd => true,
            MilBinOp::IOr => true,
            MilBinOp::IXor => true,
            MilBinOp::IShrS => false,
            MilBinOp::IShrU => false,
            MilBinOp::IShl => false,
            MilBinOp::ICmp(_) => false,
            MilBinOp::LAdd => true,
            MilBinOp::LSub => false,
            MilBinOp::LMul => true,
            MilBinOp::LDivS => false,
            MilBinOp::LRemS => false,
            MilBinOp::LAnd => true,
            MilBinOp::LOr => true,
            MilBinOp::LXor => true,
            MilBinOp::LShrS => false,
            MilBinOp::LShrU => false,
            MilBinOp::LShl => false,
            MilBinOp::LCmp => false,
            MilBinOp::FAdd => false,
            MilBinOp::FSub => false,
            MilBinOp::FMul => false,
            MilBinOp::FDiv => false,
            MilBinOp::FRem => false,
            MilBinOp::FCmp(_) => false,
            MilBinOp::DAdd => false,
            MilBinOp::DSub => false,
            MilBinOp::DMul => false,
            MilBinOp::DDiv => false,
            MilBinOp::DRem => false,
            MilBinOp::DCmp(_) => false,
            MilBinOp::RCmp(_) => false,
            MilBinOp::IsSubclass => false
        }
    }
}

impl fmt::Display for MilBinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            MilBinOp::IAdd => write!(f, "iadd"),
            MilBinOp::ISub => write!(f, "isub"),
            MilBinOp::IMul => write!(f, "imul"),
            MilBinOp::IDivS => write!(f, "idivs"),
            MilBinOp::IRemS => write!(f, "irems"),
            MilBinOp::IAnd => write!(f, "iand"),
            MilBinOp::IOr => write!(f, "ior"),
            MilBinOp::IXor => write!(f, "ixor"),
            MilBinOp::IShrS => write!(f, "ishrs"),
            MilBinOp::IShrU => write!(f, "ishru"),
            MilBinOp::IShl => write!(f, "ishl"),
            MilBinOp::ICmp(cond) => write!(f, "icmp.{}", cond.name()),
            MilBinOp::LAdd => write!(f, "ladd"),
            MilBinOp::LSub => write!(f, "lsub"),
            MilBinOp::LMul => write!(f, "lmul"),
            MilBinOp::LDivS => write!(f, "ldivs"),
            MilBinOp::LRemS => write!(f, "lrems"),
            MilBinOp::LAnd => write!(f, "land"),
            MilBinOp::LOr => write!(f, "lor"),
            MilBinOp::LXor => write!(f, "lxor"),
            MilBinOp::LShrS => write!(f, "lshrs"),
            MilBinOp::LShrU => write!(f, "lshru"),
            MilBinOp::LShl => write!(f, "lshl"),
            MilBinOp::LCmp => write!(f, "lcmp"),
            MilBinOp::FAdd => write!(f, "fadd"),
            MilBinOp::FSub => write!(f, "fsub"),
            MilBinOp::FMul => write!(f, "fmul"),
            MilBinOp::FDiv => write!(f, "fdiv"),
            MilBinOp::FRem => write!(f, "frem"),
            MilBinOp::FCmp(MilFCmpMode::L) => write!(f, "fcmpl"),
            MilBinOp::FCmp(MilFCmpMode::G) => write!(f, "fcmpg"),
            MilBinOp::DAdd => write!(f, "dadd"),
            MilBinOp::DSub => write!(f, "dsub"),
            MilBinOp::DMul => write!(f, "dmul"),
            MilBinOp::DDiv => write!(f, "ddiv"),
            MilBinOp::DRem => write!(f, "drem"),
            MilBinOp::DCmp(MilFCmpMode::L) => write!(f, "dcmpl"),
            MilBinOp::DCmp(MilFCmpMode::G) => write!(f, "dcmpg"),
            MilBinOp::RCmp(cond) => write!(f, "rcmp.{}", cond.name()),
            MilBinOp::IsSubclass => write!(f, "is_subclass")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MilInstructionKind {
    Nop,
    Copy(MilRegister, MilOperand),
    Select(MilRegister, MilOperand, MilOperand, MilOperand),
    UnOp(MilUnOp, MilRegister, MilOperand),
    BinOp(MilBinOp, MilRegister, MilOperand, MilOperand),
    GetLocal(MilLocalId, MilRegister),
    SetLocal(MilLocalId, MilOperand),
    GetField(FieldId, ClassId, MilRegister, MilOperand),
    PutField(FieldId, ClassId, MilOperand, MilOperand),
    GetArrayElement(ClassId, MilRegister, MilOperand, MilOperand),
    PutArrayElement(ClassId, MilOperand, MilOperand, MilOperand),
    GetStatic(FieldId, ClassId, MilRegister),
    PutStatic(FieldId, ClassId, MilOperand),
    AllocObj(ClassId, MilRegister),
    AllocArray(ClassId, MilRegister, MilOperand)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MilEndInstructionKind {
    Nop,
    Unreachable,
    Call(ClassId, MethodId, MilRegister, Vec<MilOperand>),
    CallVirtual(ClassId, MethodId, MilRegister, MilOperand, Vec<MilOperand>),
    CallInterface(ClassId, MethodId, MilRegister, MilOperand, Vec<MilOperand>),
    CallNative(ClassId, String, MilRegister, Vec<MilOperand>),
    Throw(MilOperand),
    Return(Option<MilOperand>),
    Jump(MilBlockId),
    JumpIf(MilBlockId, MilBlockId, MilOperand),
    ISwitch(MilOperand, Vec<(i32, MilBlockId)>, MilBlockId)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MilInstructionWrapper<T> {
    pub kind: T,
    pub bytecode: (u32, u32)
}

pub type MilInstruction = MilInstructionWrapper<MilInstructionKind>;
pub type MilEndInstruction = MilInstructionWrapper<MilEndInstructionKind>;

pub struct PrettyMilInstruction<'a>(&'a MilInstruction, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilInstruction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.0.kind {
            MilInstructionKind::Nop => {
                write!(f, "nop")?;
            },
            MilInstructionKind::Copy(tgt, ref src) => {
                write!(f, "{} = copy {}", tgt, src.pretty(self.1))?;
            },
            MilInstructionKind::Select(tgt, ref cond, ref true_val, ref false_val) => {
                write!(f, "{} = select {}, {}, {}", tgt, cond.pretty(self.1), true_val.pretty(self.1), false_val.pretty(self.1))?;
            },
            MilInstructionKind::UnOp(op, tgt, ref val) => {
                write!(f, "{} = {} {}", tgt, op, val.pretty(self.1))?;
            },
            MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                write!(f, "{} = {} {}, {}", tgt, op, lhs.pretty(self.1), rhs.pretty(self.1))?;
            },
            MilInstructionKind::GetLocal(local_id, tgt) => {
                write!(f, "{} = get_local <{}>", tgt, local_id)?;
            },
            MilInstructionKind::SetLocal(local_id, ref src) => {
                write!(f, "set_local <{}> {}", local_id, src.pretty(self.1))?;
            },
            MilInstructionKind::GetField(field_id, _, tgt, ref obj) => {
                write!(f, "{} = get_field <{}> {}", tgt, FieldName(field_id, self.1), obj.pretty(self.1))?;
            },
            MilInstructionKind::PutField(field_id, _, ref obj, ref val) => {
                write!(f, "put_field <{}> {}, {}", FieldName(field_id, self.1), obj.pretty(self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::GetArrayElement(class_id, tgt, ref obj, ref idx) => {
                write!(f, "{} = get_array_elem <{}> {}, {}", tgt, self.1.get(class_id).name(self.1), obj.pretty(self.1), idx.pretty(self.1))?;
            },
            MilInstructionKind::PutArrayElement(class_id, ref obj, ref idx, ref val) => {
                write!(f, "put_array_elem <{}> {}, {}, {}", self.1.get(class_id).name(self.1), obj.pretty(self.1), idx.pretty(self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::GetStatic(field_id, _, tgt) => {
                write!(f, "{} = get_static <{}>", tgt, FieldName(field_id, self.1))?;
            },
            MilInstructionKind::PutStatic(field_id, _, ref val) => {
                write!(f, "put_static <{}> {}", FieldName(field_id, self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::AllocObj(class_id, tgt) => {
                write!(f, "{} = alloc_obj <{}>", tgt, self.1.get(class_id).name(self.1))?;
            },
            MilInstructionKind::AllocArray(class_id, tgt, ref len) => {
                write!(f, "{} = alloc_array <{}> {}", tgt, self.1.get(class_id).name(self.1), len.pretty(self.1))?;
            }
        };

        write!(f, " @bc ")?;

        if self.0.bytecode.0 != !0 {
            write!(f, "{}:", self.0.bytecode.0)?;
        };

        if self.0.bytecode.1 == !0 {
            write!(f, "prologue")?;
        } else {
            write!(f, "{}", self.0.bytecode.1)?;
        };

        Ok(())
    }
}

impl MilInstruction {
    pub fn dummy() -> MilInstruction {
        MilInstruction {
            kind: MilInstructionKind::Nop,
            bytecode: (!0, !0)
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilInstruction(self, env)
    }

    pub fn target(&self) -> Option<&MilRegister> {
        match self.kind {
            MilInstructionKind::Nop => None,
            MilInstructionKind::Copy(ref tgt, _) => Some(tgt),
            MilInstructionKind::Select(ref tgt, _, _, _) => Some(tgt),
            MilInstructionKind::UnOp(_, ref tgt, _) => Some(tgt),
            MilInstructionKind::BinOp(_, ref tgt, _, _) => Some(tgt),
            MilInstructionKind::GetLocal(_, ref tgt) => Some(tgt),
            MilInstructionKind::SetLocal(_, _) => None,
            MilInstructionKind::GetField(_, _, ref tgt, _) => Some(tgt),
            MilInstructionKind::PutField(_, _, _, _) => None,
            MilInstructionKind::GetArrayElement(_, ref tgt, _, _) => Some(tgt),
            MilInstructionKind::PutArrayElement(_, _, _, _) => None,
            MilInstructionKind::GetStatic(_, _, ref tgt) => Some(tgt),
            MilInstructionKind::PutStatic(_, _, _) => None,
            MilInstructionKind::AllocObj(_, ref tgt) => Some(tgt),
            MilInstructionKind::AllocArray(_, ref tgt, _) => Some(tgt)
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut MilRegister> {
        match self.kind {
            MilInstructionKind::Nop => None,
            MilInstructionKind::Copy(ref mut tgt, _) => Some(tgt),
            MilInstructionKind::Select(ref mut tgt, _, _, _) => Some(tgt),
            MilInstructionKind::UnOp(_, ref mut tgt, _) => Some(tgt),
            MilInstructionKind::BinOp(_, ref mut tgt, _, _) => Some(tgt),
            MilInstructionKind::GetLocal(_, ref mut tgt) => Some(tgt),
            MilInstructionKind::SetLocal(_, _) => None,
            MilInstructionKind::GetField(_, _, ref mut tgt, _) => Some(tgt),
            MilInstructionKind::PutField(_, _, _, _) => None,
            MilInstructionKind::GetArrayElement(_, ref mut tgt, _, _) => Some(tgt),
            MilInstructionKind::PutArrayElement(_, _, _, _) => None,
            MilInstructionKind::GetStatic(_, _, ref mut tgt) => Some(tgt),
            MilInstructionKind::PutStatic(_, _, _) => None,
            MilInstructionKind::AllocObj(_, ref mut tgt) => Some(tgt),
            MilInstructionKind::AllocArray(_, ref mut tgt, _) => Some(tgt)
        }
    }

    pub fn for_operands(&self, mut f: impl FnMut (&MilOperand) -> ()) {
        match self.kind {
            MilInstructionKind::Nop => {},
            MilInstructionKind::Copy(_, ref val) => {
                f(val);
            },
            MilInstructionKind::Select(_, ref cond, ref true_val, ref false_val) => {
                f(cond);
                f(true_val);
                f(false_val);
            },
            MilInstructionKind::UnOp(_, _, ref val) => {
                f(val);
            },
            MilInstructionKind::BinOp(_, _, ref lhs, ref rhs) => {
                f(lhs);
                f(rhs);
            },
            MilInstructionKind::GetLocal(_, _) => {},
            MilInstructionKind::SetLocal(_, ref val) => {
                f(val);
            },
            MilInstructionKind::GetField(_, _, _, ref obj) => {
                f(obj);
            },
            MilInstructionKind::PutField(_, _, ref obj, ref val) => {
                f(obj);
                f(val);
            },
            MilInstructionKind::GetArrayElement(_, _, ref obj, ref idx) => {
                f(obj);
                f(idx);
            },
            MilInstructionKind::PutArrayElement(_, ref obj, ref idx, ref val) => {
                f(obj);
                f(idx);
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

    pub fn for_operands_mut(&mut self, mut f: impl FnMut (&mut MilOperand) -> ()) {
        match self.kind {
            MilInstructionKind::Nop => {},
            MilInstructionKind::Copy(_, ref mut val) => {
                f(val);
            },
            MilInstructionKind::Select(_, ref mut cond, ref mut true_val, ref mut false_val) => {
                f(cond);
                f(true_val);
                f(false_val);
            },
            MilInstructionKind::UnOp(_, _, ref mut val) => {
                f(val);
            },
            MilInstructionKind::BinOp(_, _, ref mut lhs, ref mut rhs) => {
                f(lhs);
                f(rhs);
            },
            MilInstructionKind::GetLocal(_, _) => {},
            MilInstructionKind::SetLocal(_, ref mut val) => {
                f(val);
            },
            MilInstructionKind::GetField(_, _, _, ref mut obj) => {
                f(obj);
            },
            MilInstructionKind::PutField(_, _, ref mut obj, ref mut val) => {
                f(obj);
                f(val);
            },
            MilInstructionKind::GetArrayElement(_, _, ref mut obj, ref mut idx) => {
                f(obj);
                f(idx);
            },
            MilInstructionKind::PutArrayElement(_, ref mut obj, ref mut idx, ref mut val) => {
                f(obj);
                f(idx);
                f(val);
            },
            MilInstructionKind::GetStatic(_, _, _) => {},
            MilInstructionKind::PutStatic(_, _, ref mut val) => {
                f(val);
            },
            MilInstructionKind::AllocObj(_, _) => {},
            MilInstructionKind::AllocArray(_, _, ref mut len) => {
                f(len);
            }
        };
    }

    pub fn for_registers(&self, mut f: impl FnMut (MilRegister) -> ()) {
        self.for_operands(|op| if let MilOperand::Register(_, reg) = *op {
            f(reg);
        });

        if let Some(&target) = self.target() {
            f(target);
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
            MilEndInstructionKind::Unreachable => {
                write!(f, "unreachable")?;
            },
            MilEndInstructionKind::Call(_, method_id, tgt, ref args) => {
                if tgt != MilRegister::VOID {
                    write!(f, "{} = ", tgt)?;
                };

                write!(f, "call <{}>", MethodName(method_id, self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallVirtual(_, method_id, tgt, ref obj, ref args) => {
                if tgt != MilRegister::VOID {
                    write!(f, "{} = ", tgt)?;
                };

                write!(f, "call_virtual <{}> {}", MethodName(method_id, self.1), obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallInterface(_, method_id, tgt, ref obj, ref args) => {
                if tgt != MilRegister::VOID {
                    write!(f, "{} = ", tgt)?;
                };

                write!(f, "call_interface <{}> {}", MethodName(method_id, self.1), obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallNative(_, ref name, tgt, ref args) => {
                if tgt != MilRegister::VOID {
                    write!(f, "{} = ", tgt)?;
                };

                write!(f, "call_native <{}>", name)?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::Throw(ref val) => {
                write!(f, "throw {}", val.pretty(self.1))?;
            },
            MilEndInstructionKind::Return(None) => {
                write!(f, "ret void")?;
            }
            MilEndInstructionKind::Return(Some(ref val)) => {
                write!(f, "ret {}", val.pretty(self.1))?;
            },
            MilEndInstructionKind::Jump(block) => {
                write!(f, "j {}", block)?;
            },
            MilEndInstructionKind::JumpIf(true_block, false_block, ref cond) => {
                write!(f, "jc {}/{}, {}", true_block, false_block, cond.pretty(self.1))?;
            },
            MilEndInstructionKind::ISwitch(ref val, ref targets, default_target) => {
                write!(f, "iswitch {}", val.pretty(self.1))?;

                for (val, target) in targets.iter().copied() {
                    write!(f, ", {}:{}", val, target)?;
                };

                write!(f, ", default:{}", default_target)?;
            }
        };

        write!(f, " @bc ")?;

        if self.0.bytecode.0 != !0 {
            write!(f, "{}:", self.0.bytecode.0)?;
        };

        if self.0.bytecode.1 == !0 {
            write!(f, "prologue")?;
        } else {
            write!(f, "{}", self.0.bytecode.1)?;
        };

        Ok(())
    }
}

impl MilEndInstruction {
    pub fn dummy() -> MilEndInstruction {
        MilEndInstruction {
            kind: MilEndInstructionKind::Nop,
            bytecode: (!0, !0)
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilEndInstruction(self, env)
    }

    pub fn target(&self) -> Option<&MilRegister> {
        match self.kind {
            MilEndInstructionKind::Nop => None,
            MilEndInstructionKind::Unreachable => None,
            MilEndInstructionKind::Call(_, _, ref tgt, _) => Some(tgt),
            MilEndInstructionKind::CallVirtual(_, _, ref tgt, _, _) => Some(tgt),
            MilEndInstructionKind::CallInterface(_, _, ref tgt, _, _) => Some(tgt),
            MilEndInstructionKind::CallNative(_, _, ref tgt, _) => Some(tgt),
            MilEndInstructionKind::Throw(_) => None,
            MilEndInstructionKind::Return(_) => None,
            MilEndInstructionKind::Jump(_) => None,
            MilEndInstructionKind::JumpIf(_, _, _) => None,
            MilEndInstructionKind::ISwitch(_, _, _) => None
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut MilRegister> {
        match self.kind {
            MilEndInstructionKind::Nop => None,
            MilEndInstructionKind::Unreachable => None,
            MilEndInstructionKind::Call(_, _, ref mut tgt, _) => Some(tgt),
            MilEndInstructionKind::CallVirtual(_, _, ref mut tgt, _, _) => Some(tgt),
            MilEndInstructionKind::CallInterface(_, _, ref mut tgt, _, _) => Some(tgt),
            MilEndInstructionKind::CallNative(_, _, ref mut tgt, _) => Some(tgt),
            MilEndInstructionKind::Throw(_) => None,
            MilEndInstructionKind::Return(_) => None,
            MilEndInstructionKind::Jump(_) => None,
            MilEndInstructionKind::JumpIf(_, _, _) => None,
            MilEndInstructionKind::ISwitch(_, _, _) => None
        }
    }

    pub fn for_operands(&self, mut f: impl FnMut (&MilOperand) -> ()) {
        match self.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Unreachable => {},
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
            MilEndInstructionKind::CallInterface(_, _, _, ref obj, ref args) => {
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
            MilEndInstructionKind::Throw(ref val) => {
                f(val);
            },
            MilEndInstructionKind::Return(None) => {},
            MilEndInstructionKind::Return(Some(ref val)) => {
                f(val);
            },
            MilEndInstructionKind::Jump(_) => {},
            MilEndInstructionKind::JumpIf(_, _, ref cond) => {
                f(cond);
            },
            MilEndInstructionKind::ISwitch(ref val, _, _) => {
                f(val);
            }
        };
    }

    pub fn for_operands_mut(&mut self, mut f: impl FnMut (&mut MilOperand) -> ()) {
        match self.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Call(_, _, _, ref mut args) => {
                for a in args.iter_mut() {
                    f(a);
                };
            },
            MilEndInstructionKind::CallVirtual(_, _, _, ref mut obj, ref mut args) => {
                f(obj);
                for a in args.iter_mut() {
                    f(a);
                };
            },
            MilEndInstructionKind::CallInterface(_, _, _, ref mut obj, ref mut args) => {
                f(obj);
                for a in args.iter_mut() {
                    f(a);
                };
            },
            MilEndInstructionKind::CallNative(_, _, _, ref mut args) => {
                for a in args.iter_mut() {
                    f(a);
                };
            },
            MilEndInstructionKind::Throw(ref mut val) => {
                f(val);
            },
            MilEndInstructionKind::Return(None) => {},
            MilEndInstructionKind::Return(Some(ref mut val)) => {
                f(val);
            },
            MilEndInstructionKind::Jump(_) => {},
            MilEndInstructionKind::JumpIf(_, _, ref mut cond) => {
                f(cond);
            },
            MilEndInstructionKind::ISwitch(ref mut val, _, _) => {
                f(val);
            }
        };
    }

    pub fn for_registers(&self, mut f: impl FnMut (MilRegister) -> ()) {
        self.for_operands(|op| if let MilOperand::Register(_, reg) = *op {
            f(reg);
        });

        if let Some(&target) = self.target() {
            f(target);
        };
    }

    pub fn can_fall_through(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Nop => true,
            MilEndInstructionKind::Unreachable => false,
            MilEndInstructionKind::Call(_, _, _, _) => true,
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => true,
            MilEndInstructionKind::CallInterface(_, _, _, _, _) => true,
            MilEndInstructionKind::CallNative(_, _, _, _) => true,
            MilEndInstructionKind::Throw(_) => false,
            MilEndInstructionKind::Return(_) => false,
            MilEndInstructionKind::Jump(_) => false,
            MilEndInstructionKind::JumpIf(_, _, _) => false,
            MilEndInstructionKind::ISwitch(_, _, _) => false
        }
    }

    pub fn for_successors(&self, mut f: impl FnMut (&MilBlockId) -> ()) {
        match self.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Call(_, _, _, _) => {},
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => {},
            MilEndInstructionKind::CallInterface(_, _, _, _, _) => {},
            MilEndInstructionKind::CallNative(_, _, _, _) => {},
            MilEndInstructionKind::Throw(_) => {},
            MilEndInstructionKind::Return(_) => {},
            MilEndInstructionKind::Jump(ref tgt) => {
                f(tgt);
            },
            MilEndInstructionKind::JumpIf(ref true_tgt, ref false_tgt, _) => {
                f(true_tgt);
                f(false_tgt);
            },
            MilEndInstructionKind::ISwitch(_, ref tgts, ref default_tgt) => {
                for &(_, ref tgt) in tgts.iter() {
                    f(tgt);
                };
                f(default_tgt);
            }
        }
    }

    pub fn for_successors_mut(&mut self, mut f: impl FnMut (&mut MilBlockId) -> ()) {
        match self.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Call(_, _, _, _) => {},
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => {},
            MilEndInstructionKind::CallInterface(_, _, _, _, _) => {},
            MilEndInstructionKind::CallNative(_, _, _, _) => {},
            MilEndInstructionKind::Throw(_) => {},
            MilEndInstructionKind::Return(_) => {},
            MilEndInstructionKind::Jump(ref mut tgt) => {
                f(tgt);
            },
            MilEndInstructionKind::JumpIf(ref mut true_tgt, ref mut false_tgt, _) => {
                f(true_tgt);
                f(false_tgt);
            },
            MilEndInstructionKind::ISwitch(_, ref mut tgts, ref mut default_tgt) => {
                for &mut (_, ref mut tgt) in tgts.iter_mut() {
                    f(tgt);
                };
                f(default_tgt);
            }
        }
    }

    pub fn is_nop(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Nop => true,
            _ => false
        }
    }

    pub fn is_call(&self) -> bool {
        match self.kind {
            MilEndInstructionKind::Call(_, _, _, _) => true,
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => true,
            MilEndInstructionKind::CallInterface(_, _, _, _, _) => true,
            MilEndInstructionKind::CallNative(_, _, _, _) => true,
            _ => false
        }
    }
}
