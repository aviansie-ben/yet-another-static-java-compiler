use std::fmt;

use crate::resolve::{ClassEnvironment, ClassId};

use super::*;

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilAnnotatedType {
    pub ty: MilType,
    pub constraint: Option<MilClassConstraint>
}

impl MilAnnotatedType {
    pub fn new(ty: MilType, constraint: Option<MilClassConstraint>) -> MilAnnotatedType {
        MilAnnotatedType { ty, constraint }
    }

    pub fn for_class_return(class_id: ClassId) -> Option<MilAnnotatedType> {
        MilType::for_class_return(class_id).map(|ty| {
            if ty == MilType::Ref {
                MilAnnotatedType::new(ty, Some(MilClassConstraint::for_class(class_id)))
            } else {
                MilAnnotatedType::new(ty, None)
            }
        })
    }

    pub fn for_class(class_id: ClassId) -> MilAnnotatedType {
        let ty = MilType::for_class(class_id);

        if ty == MilType::Ref {
            MilAnnotatedType::new(ty, Some(MilClassConstraint::for_class(class_id)))
        } else {
            MilAnnotatedType::new(ty, None)
        }
    }

    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyMilAnnotatedType(self, env)
    }
}

struct PrettyMilAnnotatedType<'a>(&'a MilAnnotatedType, &'a ClassEnvironment);

impl fmt::Display for PrettyMilAnnotatedType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyMilAnnotatedType(ty, env) = *self;

        write!(f, "{}", ty.ty)?;
        if let Some(constraint) = ty.constraint {
            write!(f, " @constraint {}", constraint.pretty(env))?;
        };

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct MilFunctionSignature {
    pub return_type: Option<MilAnnotatedType>,
    pub param_types: Vec<MilAnnotatedType>
}

impl MilFunctionSignature {
    pub fn new(return_type: Option<MilAnnotatedType>, param_types: Vec<MilAnnotatedType>) -> MilFunctionSignature {
        MilFunctionSignature { return_type, param_types }
    }

    pub fn new_bare(return_type: Option<MilType>, param_types: Vec<MilType>) -> MilFunctionSignature {
        MilFunctionSignature::new(
            return_type.map(|ty| MilAnnotatedType::new(ty, None)),
            param_types.into_iter().map(|ty| MilAnnotatedType::new(ty, None)).collect()
        )
    }

    pub fn void() -> MilFunctionSignature {
        MilFunctionSignature::new(None, vec![])
    }
}
