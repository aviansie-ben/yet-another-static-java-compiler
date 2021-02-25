use std::collections::HashMap;

use crate::resolve::MethodId;

use super::*;

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
