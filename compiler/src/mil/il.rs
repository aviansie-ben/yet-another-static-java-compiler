use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

use smallvec::{smallvec, SmallVec};

use crate::bytecode::BytecodeCondition;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId};
use crate::static_heap::JavaStaticRef;
use crate::static_interp::Value;
use crate::util::BitVecIndex;

#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct MilRegister(pub u32);

impl MilRegister {
    pub const VOID: MilRegister = MilRegister(!0);
}

impl fmt::Debug for MilRegister {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self == &MilRegister::VOID {
            write!(f, "MilRegister::VOID")
        } else {
            write!(f, "MilRegister({})", self.0)
        }
    }
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

impl BitVecIndex for MilRegister {
    fn into_index(self) -> usize {
        self.0 as usize
    }

    fn from_index(i: usize) -> Self {
        MilRegister(i as u32)
    }
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MilType {
    Addr,
    Ref,
    Bool,
    Int,
    Long,
    Float,
    Double
}

impl MilType {
    pub fn for_class_return(class_id: ClassId) -> Option<MilType> {
        match class_id {
            ClassId::PRIMITIVE_VOID => None,
            _ => Some(MilType::for_class(class_id))
        }
    }

    pub fn for_class(class_id: ClassId) -> MilType {
        match class_id {
            ClassId::PRIMITIVE_VOID => unreachable!(),
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
            MilType::Addr => write!(f, "addr"),
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
    rev_objs: HashMap<JavaStaticRef<'a>, MilKnownObjectId>,
    next: MilKnownObjectId
}

impl <'a> MilKnownObjectMap<'a> {
    pub fn new() -> MilKnownObjectMap<'a> {
        MilKnownObjectMap {
            refs: MilKnownObjectRefs::new(),
            objs: HashMap::new(),
            rev_objs: HashMap::new(),
            next: MilKnownObjectId(0)
        }
    }

    pub fn add(&mut self, obj: JavaStaticRef<'a>) -> MilKnownObjectId {
        let id = self.next;
        self.objs.insert(id, obj.clone());
        self.rev_objs.insert(obj, id);
        self.next.0 += 1;
        id
    }

    pub fn get(&self, id: MilKnownObjectId) -> &JavaStaticRef<'a> {
        &self.objs[&id]
    }

    pub fn id_of(&self, obj: &JavaStaticRef<'a>) -> MilKnownObjectId {
        self.rev_objs[obj]
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilKnownObjectId(pub u32);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MilOperand {
    Register(MilType, MilRegister),
    Poison(MilType),
    AddrNull,
    RefNull,
    KnownObject(MilKnownObjectId, ClassId),
    Bool(bool),
    Int(i32),
    Long(i64),
    Float(u32),
    Double(u64)
}

impl MilOperand {
    pub fn get_type(&self) -> MilType {
        match *self {
            MilOperand::Register(ty, _) => ty,
            MilOperand::Poison(ty) => ty,
            MilOperand::AddrNull => MilType::Addr,
            MilOperand::RefNull => MilType::Ref,
            MilOperand::KnownObject(_, _) => MilType::Ref,
            MilOperand::Bool(_) => MilType::Bool,
            MilOperand::Int(_) => MilType::Int,
            MilOperand::Long(_) => MilType::Long,
            MilOperand::Float(_) => MilType::Float,
            MilOperand::Double(_) => MilType::Double
        }
    }

    pub fn is_const(&self) -> bool {
        self.as_reg().is_none()
    }

    pub fn from_const<'a>(val: Value<'a>, known_objects: &MilKnownObjectMap<'a>) -> MilOperand {
        match val {
            Value::Int(val) => MilOperand::Int(val),
            Value::Long(val) => MilOperand::Long(val),
            Value::Float(val) => MilOperand::Float(val),
            Value::Double(val) => MilOperand::Double(val),
            Value::Ref(None) => MilOperand::RefNull,
            Value::Ref(Some(val)) => {
                let class_id = val.class_id();
                MilOperand::KnownObject(known_objects.id_of(&val), class_id)
            },
            _ => unreachable!()
        }
    }

    pub fn as_reg(&self) -> Option<MilRegister> {
        match *self {
            MilOperand::Register(_, reg) => Some(reg),
            _ => None
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
            MilOperand::Register(ty, reg) => write!(f, "{}:{}", ty, reg),
            MilOperand::Poison(ty) => write!(f, "{}:poison", ty),
            MilOperand::AddrNull => write!(f, "addr:null"),
            MilOperand::RefNull => write!(f, "ref:null"),
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
pub struct MilClassConstraint(ClassId, bool, bool);

impl MilClassConstraint {
    pub fn class_id(&self) -> ClassId {
        self.0
    }

    pub fn nullable(&self) -> bool {
        self.1
    }

    pub fn is_exact(&self) -> bool {
        self.2
    }

    pub fn or_null(&self) -> MilClassConstraint {
        MilClassConstraint(self.0, true, self.2)
    }

    pub fn not_null(&self) -> MilClassConstraint {
        MilClassConstraint(self.0, false, self.2)
    }

    pub fn exact(&self) -> MilClassConstraint {
        MilClassConstraint(self.0, self.1, true)
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilClassConstraint(self, env)
    }

    pub fn for_class(class_id: ClassId) -> MilClassConstraint {
        MilClassConstraint(class_id, !class_id.is_primitive_type(), false)
    }

    pub fn null() -> MilClassConstraint {
        MilClassConstraint(ClassId::UNRESOLVED, true, true)
    }

    pub fn non_null() -> MilClassConstraint {
        MilClassConstraint(ClassId::JAVA_LANG_OBJECT, false, false)
    }

    pub fn union(a: MilClassConstraint, b: MilClassConstraint, env: &ClassEnvironment) -> MilClassConstraint {
        let (common_class_id, is_exact) = if a.0 == ClassId::UNRESOLVED {
            (b.0, b.2)
        } else if b.0 == ClassId::UNRESOLVED {
            (a.0, a.2)
        } else if a.0 == b.0 {
            (a.0, a.2 && b.2)
        } else {
            let a_chain = env.get_class_chain(a.0);
            let b_chain = env.get_class_chain(b.0);
            let mut common_class_id = ClassId::UNRESOLVED;

            for class_id in a_chain {
                if b_chain.contains(&class_id) {
                    common_class_id = class_id;
                    break;
                };
            };

            assert_ne!(common_class_id, ClassId::UNRESOLVED);
            (common_class_id, false)
        };

        MilClassConstraint(common_class_id, a.1 || b.1, is_exact)
    }

    pub fn intersection(a: MilClassConstraint, b: MilClassConstraint, env: &ClassEnvironment) -> MilClassConstraint {
        let lower_class_id = if a.0 == ClassId::UNRESOLVED || b.0 == ClassId::UNRESOLVED {
            ClassId::UNRESOLVED
        } else if env.can_convert(a.0, b.0) {
            a.0
        } else {
            assert!(env.can_convert(b.0, a.0));
            b.0
        };

        MilClassConstraint(lower_class_id, a.1 && b.1, a.2 || b.2)
    }
}

struct PrettyMilClassConstraint<'a>(&'a MilClassConstraint, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyMilClassConstraint<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if (self.0).0 == ClassId::UNRESOLVED {
            if (self.0).1 {
                write!(f, "null")?;
            } else {
                write!(f, "never")?;
            };
        } else {
            write!(f, "{}", self.1.get((self.0).0).name(self.1))?;
            if (self.0).1 {
                write!(f, "?")?;
            };
            if (self.0).2 {
                write!(f, "!")?;
            };
        };
        Ok(())
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
    D2F
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
            MilUnOp::D2F => (MilType::Float, MilType::Double)
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
            MilUnOp::D2F => write!(f, "d2f")
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
    RCmp(MilRefComparison)
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
            MilBinOp::RCmp(_) => (MilType::Bool, MilType::Ref, MilType::Ref)
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
            MilBinOp::RCmp(cmp) => Some(MilBinOp::RCmp(cmp))
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
            MilBinOp::RCmp(_) => false
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
            MilBinOp::RCmp(cond) => write!(f, "rcmp.{}", cond.name())
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
    GetParam(u16, MilClassConstraint, MilRegister),
    GetLocal(MilLocalId, MilRegister),
    SetLocal(MilLocalId, MilOperand),
    GetField(FieldId, ClassId, MilRegister, MilOperand),
    PutField(FieldId, ClassId, MilOperand, MilOperand),
    GetArrayLength(MilRegister, MilOperand),
    GetArrayElement(ClassId, MilRegister, MilOperand, MilOperand),
    PutArrayElement(ClassId, MilOperand, MilOperand, MilOperand),
    GetStatic(FieldId, ClassId, MilRegister),
    PutStatic(FieldId, ClassId, MilOperand),
    AllocObj(ClassId, MilRegister),
    AllocArray(ClassId, MilRegister, MilOperand),
    GetVTable(MilRegister, MilOperand),
    IsSubclass(ClassId, MilRegister, MilOperand)
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

pub struct FieldName<'a>(pub FieldId, pub &'a ClassEnvironment);

impl <'a> fmt::Display for FieldName<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FieldName(field_id, env) = *self;
        let (class, field) = env.get_field(field_id);

        write!(f, "{}.{} {}", class.meta.name, field.name, field.descriptor)
    }
}

pub struct MethodName<'a>(pub MethodId, pub &'a ClassEnvironment);

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
            MilInstructionKind::Select(tgt, ref cond, ref true_val, ref false_val) => {
                write!(f, "select {}, {}, {}, {}", tgt, cond.pretty(self.1), true_val.pretty(self.1), false_val.pretty(self.1))?;
            },
            MilInstructionKind::UnOp(op, tgt, ref val) => {
                write!(f, "{} {}, {}", op, tgt, val.pretty(self.1))?;
            },
            MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                write!(f, "{} {}, {}, {}", op, tgt, lhs.pretty(self.1), rhs.pretty(self.1))?;
            },
            MilInstructionKind::GetParam(n, class_constraint, tgt) => {
                write!(f, "get_param <{} {}> {}", n, class_constraint.pretty(self.1), tgt)?;
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
            MilInstructionKind::GetArrayLength(tgt, ref obj) => {
                write!(f, "get_array_length {}, {}", tgt, obj.pretty(self.1))?;
            },
            MilInstructionKind::GetArrayElement(class_id, tgt, ref obj, ref idx) => {
                write!(f, "get_array_elem <{}> {}, {}, {}", self.1.get(class_id).name(self.1), tgt, obj.pretty(self.1), idx.pretty(self.1))?;
            },
            MilInstructionKind::PutArrayElement(class_id, ref obj, ref idx, ref val) => {
                write!(f, "put_array_elem <{}> {}, {}, {}", self.1.get(class_id).name(self.1), obj.pretty(self.1), idx.pretty(self.1), val.pretty(self.1))?;
            },
            MilInstructionKind::GetStatic(field_id, _, tgt) => {
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
            },
            MilInstructionKind::GetVTable(tgt, ref obj) => {
                write!(f, "get_vtable {}, {}", tgt, obj.pretty(self.1))?;
            },
            MilInstructionKind::IsSubclass(class_id, tgt, ref vtable) => {
                write!(f, "is_subclass <{}> {}, {}", self.1.get(class_id).name(self.1), tgt, vtable.pretty(self.1))?;
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
            MilInstructionKind::GetParam(_, _, ref tgt) => Some(tgt),
            MilInstructionKind::GetLocal(_, ref tgt) => Some(tgt),
            MilInstructionKind::SetLocal(_, _) => None,
            MilInstructionKind::GetField(_, _, ref tgt, _) => Some(tgt),
            MilInstructionKind::PutField(_, _, _, _) => None,
            MilInstructionKind::GetArrayLength(ref tgt, _) => Some(tgt),
            MilInstructionKind::GetArrayElement(_, ref tgt, _, _) => Some(tgt),
            MilInstructionKind::PutArrayElement(_, _, _, _) => None,
            MilInstructionKind::GetStatic(_, _, ref tgt) => Some(tgt),
            MilInstructionKind::PutStatic(_, _, _) => None,
            MilInstructionKind::AllocObj(_, ref tgt) => Some(tgt),
            MilInstructionKind::AllocArray(_, ref tgt, _) => Some(tgt),
            MilInstructionKind::GetVTable(ref tgt, _) => Some(tgt),
            MilInstructionKind::IsSubclass(_, ref tgt, _) => Some(tgt)
        }
    }

    pub fn target_mut(&mut self) -> Option<&mut MilRegister> {
        match self.kind {
            MilInstructionKind::Nop => None,
            MilInstructionKind::Copy(ref mut tgt, _) => Some(tgt),
            MilInstructionKind::Select(ref mut tgt, _, _, _) => Some(tgt),
            MilInstructionKind::UnOp(_, ref mut tgt, _) => Some(tgt),
            MilInstructionKind::BinOp(_, ref mut tgt, _, _) => Some(tgt),
            MilInstructionKind::GetParam(_, _, ref mut tgt) => Some(tgt),
            MilInstructionKind::GetLocal(_, ref mut tgt) => Some(tgt),
            MilInstructionKind::SetLocal(_, _) => None,
            MilInstructionKind::GetField(_, _, ref mut tgt, _) => Some(tgt),
            MilInstructionKind::PutField(_, _, _, _) => None,
            MilInstructionKind::GetArrayLength(ref mut tgt, _) => Some(tgt),
            MilInstructionKind::GetArrayElement(_, ref mut tgt, _, _) => Some(tgt),
            MilInstructionKind::PutArrayElement(_, _, _, _) => None,
            MilInstructionKind::GetStatic(_, _, ref mut tgt) => Some(tgt),
            MilInstructionKind::PutStatic(_, _, _) => None,
            MilInstructionKind::AllocObj(_, ref mut tgt) => Some(tgt),
            MilInstructionKind::AllocArray(_, ref mut tgt, _) => Some(tgt),
            MilInstructionKind::GetVTable(ref mut tgt, _) => Some(tgt),
            MilInstructionKind::IsSubclass(_, ref mut tgt, _) => Some(tgt)
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
            MilInstructionKind::GetParam(_, _, _) => {},
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
            MilInstructionKind::GetArrayLength(_, ref obj) => {
                f(obj);
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
            },
            MilInstructionKind::GetVTable(_, ref obj) => {
                f(obj);
            },
            MilInstructionKind::IsSubclass(_, _, ref vtable) => {
                f(vtable);
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
            MilInstructionKind::GetParam(_, _, _) => {},
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
            MilInstructionKind::GetArrayLength(_, ref mut obj) => {
                f(obj);
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
            },
            MilInstructionKind::GetVTable(_, ref mut obj) => {
                f(obj);
            },
            MilInstructionKind::IsSubclass(_, _, ref mut vtable) => {
                f(vtable);
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
                write!(f, "call <{}> {}", MethodName(method_id, self.1), tgt)?;

                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallVirtual(_, method_id, tgt, ref obj, ref args) => {
                write!(f, "call_virtual <{}> {}, {}", MethodName(method_id, self.1), tgt, obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallInterface(_, method_id, tgt, ref obj, ref args) => {
                write!(f, "call_interface <{}> {}, {}", MethodName(method_id, self.1), tgt, obj.pretty(self.1))?;
                for a in args.iter() {
                    write!(f, ", {}", a.pretty(self.1))?;
                };
            },
            MilEndInstructionKind::CallNative(_, ref name, tgt, ref args) => {
                write!(f, "call_native <{}> {}", name, tgt)?;
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
}

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

#[derive(Debug, Clone)]
pub struct MilLineMap {
    table: Option<Arc<[(u16, u16)]>>
}

impl MilLineMap {
    pub fn from_table(table: Option<Arc<[(u16, u16)]>>) -> MilLineMap {
        MilLineMap { table }
    }

    pub fn empty() -> MilLineMap {
        MilLineMap::from_table(None)
    }

    pub fn get_line(&self, bc: u32) -> Option<u32> {
        if let Some(ref table) = self.table {
            match table.binary_search_by_key(&bc, |&(bc, _)| bc as u32) {
                Ok(idx) => Some(table[idx].1 as u32),
                Err(0) => None,
                Err(idx) => Some(table[idx - 1].1 as u32)
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct MilLocalDebugMapEntry {
    pub name: Arc<str>,
    pub ty: ClassId,
    pub local: MilLocalId
}

#[derive(Debug, Clone)]
pub struct MilLocalDebugScope {
    start_bc: u32,
    end_bc: u32,
    pub sub_scopes: Vec<MilLocalDebugScope>,
    pub locals: Vec<MilLocalDebugMapEntry>
}

impl MilLocalDebugScope {
    pub fn new(start_bc: u32, end_bc: u32) -> MilLocalDebugScope {
        assert!(start_bc < end_bc);
        MilLocalDebugScope {
            start_bc,
            end_bc,
            sub_scopes: vec![],
            locals: vec![]
        }
    }

    pub fn range(&self) -> (u32, u32) {
        (self.start_bc, self.end_bc)
    }

    pub fn split_at(self, split_bc: u32) -> (MilLocalDebugScope, MilLocalDebugScope) {
        assert!(split_bc >= self.start_bc && split_bc < self.end_bc);

        let mut scope_a = MilLocalDebugScope::new(self.start_bc, split_bc);
        let mut scope_b = MilLocalDebugScope::new(split_bc, self.end_bc);

        scope_a.locals = self.locals.clone();
        scope_b.locals = self.locals;

        for sub_scope in self.sub_scopes {
            if sub_scope.start_bc < split_bc {
                if sub_scope.end_bc > split_bc {
                    let (sub_scope_a, sub_scope_b) = sub_scope.split_at(split_bc);

                    scope_a.sub_scopes.push(sub_scope_a);
                    scope_b.sub_scopes.push(sub_scope_b);
                } else {
                    scope_a.sub_scopes.push(sub_scope);
                };
            } else {
                scope_b.sub_scopes.push(sub_scope);
            };
        };

        (scope_a, scope_b)
    }
}

#[derive(Debug, Clone)]
pub struct MilLocalDebugMap {
    top_scope: MilLocalDebugScope
}

impl MilLocalDebugMap {
    pub fn new(top_scope: MilLocalDebugScope) -> MilLocalDebugMap {
        MilLocalDebugMap { top_scope }
    }

    pub fn top_scope(&self) -> &MilLocalDebugScope {
        &self.top_scope
    }
}

#[derive(Debug, Clone)]
pub struct MilInlineSiteInfo {
    pub method_id: MethodId,
    pub call_location: (u32, u32),
    pub line_map: MilLineMap
}

#[derive(Debug, Clone)]
pub struct MilFunction {
    pub id: MethodId,
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
    pub fn new(id: MethodId) -> MilFunction {
        MilFunction {
            id,
            reg_alloc: MilRegisterAllocator::new(),
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
