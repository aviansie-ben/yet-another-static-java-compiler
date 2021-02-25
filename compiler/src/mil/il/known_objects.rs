use std::collections::HashMap;

use crate::resolve::ClassId;
use crate::static_heap::JavaStaticRef;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MilKnownObjectId(pub u32);

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
