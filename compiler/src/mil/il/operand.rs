use std::fmt;

use crate::resolve::{ClassEnvironment, ClassId};
use crate::static_interp::Value;
use crate::util::BitVecIndex;

use super::*;

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum MilOperand {
    Register(MilType, MilRegister),
    Poison(MilType),
    AddrNull,
    VTable(ClassId),
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
            MilOperand::VTable(_) => MilType::Addr,
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
            MilOperand::VTable(cls) => write!(f, "addr:<vtable_{}>", self.1.get(cls).name(self.1)),
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
