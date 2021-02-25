use std::fmt;

use crate::resolve::{ClassEnvironment, ClassId};

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
