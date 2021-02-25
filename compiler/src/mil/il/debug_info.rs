use std::sync::Arc;

use crate::resolve::{ClassId, MethodId};

use super::*;

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
