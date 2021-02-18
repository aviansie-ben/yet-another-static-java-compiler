use std::collections::HashMap;
use std::mem;

use crate::util::BitVec;

use super::il::*;

#[derive(Debug, Clone, Copy)]
enum MilRegisterSourceIndex {
    Phi(usize),
    Instr(usize),
    InstrToInsert(usize),
    EndInstr(usize)
}

#[derive(Debug, Clone)]
struct MilBlockInfo {
    id: MilBlockId,
    next_phi: usize,
    next_instr: usize,
    instrs_to_insert: Vec<(usize, usize)>
}

#[derive(Debug, Clone, Copy)]
pub enum MilRegisterSource<'a> {
    Phi(&'a MilPhiNode),
    Instr(&'a MilInstruction),
    EndInstr(&'a MilEndInstruction)
}

impl <'a> MilRegisterSource<'a> {
    pub fn kind(self) -> MilRegisterSourceKind<'a> {
        match self {
            MilRegisterSource::Phi(phi) => MilRegisterSourceKind::Phi(phi),
            MilRegisterSource::Instr(instr) => MilRegisterSourceKind::Instr(&instr.kind),
            MilRegisterSource::EndInstr(instr) => MilRegisterSourceKind::EndInstr(&instr.kind)
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum MilRegisterSourceKind<'a> {
    Phi(&'a MilPhiNode),
    Instr(&'a MilInstructionKind),
    EndInstr(&'a MilEndInstructionKind)
}

#[derive(Debug)]
pub enum MilRegisterSourceMut<'a> {
    Phi(&'a mut MilPhiNode),
    Instr(&'a mut MilInstruction),
    EndInstr(&'a mut MilEndInstruction)
}

#[derive(Debug)]
pub struct MilFlatRepr<'a> {
    blocks_ptr: *mut HashMap<MilBlockId, MilBlock>,
    phis: Vec<&'a mut MilPhiNode>,
    instrs: Vec<&'a mut MilInstruction>,
    instrs_to_insert: Vec<MilInstruction>,
    end_instrs: Vec<&'a mut MilEndInstruction>,
    reg_sources: HashMap<MilRegister, MilRegisterSourceIndex>,
    blocks: Vec<MilBlockInfo>
}

fn flat_blocks<'a>(block_order: &[MilBlockId], blocks: &'a mut HashMap<MilBlockId, MilBlock>) -> Vec<&'a mut MilBlock> {
    let mut visited_blocks = BitVec::new();

    block_order.iter().copied().map(|block_id| {
        if visited_blocks.set(block_id, true) {
            panic!("Block order ({:?}) contains the same block multiple times", block_order);
        };

        // SAFETY: visited_blocks ensures the mutable borrows here are disjoint
        unsafe { &mut *(blocks.get_mut(&block_id).unwrap() as *mut _) }
    }).collect()
}

impl <'a> MilFlatRepr<'a> {
    pub fn from_blocks(block_order: &[MilBlockId], blocks: &'a mut HashMap<MilBlockId, MilBlock>) -> Self {
        let mut flat_repr = MilFlatRepr {
            blocks_ptr: blocks as *mut _,
            phis: vec![],
            instrs: vec![],
            instrs_to_insert: vec![],
            end_instrs: vec![],
            reg_sources: HashMap::new(),
            blocks: vec![]
        };

        for block in flat_blocks(block_order, blocks) {
            for phi in block.phi_nodes.iter_mut() {
                flat_repr.reg_sources.insert(phi.target, MilRegisterSourceIndex::Phi(flat_repr.phis.len()));
                flat_repr.phis.push(phi);
            };

            for instr in block.instrs.iter_mut() {
                if let Some(tgt) = instr.target().copied() {
                    flat_repr.reg_sources.insert(tgt, MilRegisterSourceIndex::Instr(flat_repr.instrs.len()));
                };
                flat_repr.instrs.push(instr);
            };

            if let Some(tgt) = block.end_instr.target().copied() {
                flat_repr.reg_sources.insert(tgt, MilRegisterSourceIndex::EndInstr(flat_repr.end_instrs.len()));
            };
            flat_repr.end_instrs.push(&mut block.end_instr);

            flat_repr.blocks.push(MilBlockInfo {
                id: block.id,
                next_phi: flat_repr.phis.len(),
                next_instr: flat_repr.instrs.len(),
                instrs_to_insert: vec![]
            });
        };

        flat_repr
    }

    pub fn get_reg(&self, reg: MilRegister) -> Option<MilRegisterSource> {
        // SAFETY: All indexes in reg_sources are in-bounds when added during construction and none of these vectors ever shrink
        self.reg_sources.get(&reg).copied().map(|idx| match idx {
            MilRegisterSourceIndex::Phi(i) => MilRegisterSource::Phi(unsafe { *self.phis.get_unchecked(i) }),
            MilRegisterSourceIndex::Instr(i) => MilRegisterSource::Instr(unsafe { *self.instrs.get_unchecked(i) }),
            MilRegisterSourceIndex::InstrToInsert(i) => MilRegisterSource::Instr(unsafe { self.instrs_to_insert.get_unchecked(i) }),
            MilRegisterSourceIndex::EndInstr(i) => MilRegisterSource::EndInstr(unsafe { *self.end_instrs.get_unchecked(i) })
        })
    }

    pub fn visit_instrs(&mut self, mut f: impl FnMut (&mut MilFlatReprIter<'_, 'a>, MilRegisterSourceMut) -> bool) {
        let mut blocks = mem::replace(&mut self.blocks, vec![]);

        let mut next_phi = 0;
        let mut next_instr = 0;
        let mut next_end_instr = 0;

        for block_info in blocks.iter_mut() {
            let mut iter = MilFlatReprIter {
                flat_repr: self,
                block_info,
                instrs_to_insert_idx: 0,
                idx: MilRegisterSourceIndex::Phi(0)
            };

            for i in next_phi..iter.block_info.next_phi {
                iter.visit_phi(i, &mut f);
            };

            for i in next_instr..iter.block_info.next_instr {
                iter.visit_instr(i, &mut f);
            };

            iter.visit_end_instr(next_end_instr, &mut f);

            next_phi = block_info.next_phi;
            next_instr = block_info.next_instr;
            next_end_instr += 1;
        };

        self.blocks = blocks;
    }

    pub fn finish(mut self) {
        mem::drop(self.phis);
        mem::drop(self.instrs);
        mem::drop(self.end_instrs);

        // SAFETY: The block map was mutably borrowed for the lifetime of the MilFlatRepr object and all stored mutable borrows of the block
        //         map that we previously had have been dropped by this point, so it is now safe to start modifying the structure of the
        //         blocks themselves.
        let blocks = unsafe { &mut *self.blocks_ptr };
        let mut first_instr = 0;

        for block_info in self.blocks {
            let block = blocks.get_mut(&block_info.id).unwrap();
            for (idx, iti_idx) in block_info.instrs_to_insert {
                block.instrs.insert(idx - first_instr, mem::replace(&mut self.instrs_to_insert[iti_idx], MilInstruction::dummy()));
            };

            first_instr = block_info.next_instr;
        };
    }
}

#[derive(Debug)]
pub struct MilFlatReprIter<'a, 'b> {
    flat_repr: &'a mut MilFlatRepr<'b>,
    block_info: &'a mut MilBlockInfo,
    instrs_to_insert_idx: usize,
    idx: MilRegisterSourceIndex
}

impl <'a, 'b> MilFlatReprIter<'a, 'b> {
    fn visit_instrs_to_insert_before(
        &mut self,
        idx: usize,
        f: &mut impl FnMut (&mut MilFlatReprIter<'_, 'b>, MilRegisterSourceMut) -> bool
    ) {
        while self.block_info.instrs_to_insert.get(self.instrs_to_insert_idx).map_or(false, |&(iti_idx, _)| iti_idx <= idx) {
            let idx = self.block_info.instrs_to_insert[self.instrs_to_insert_idx].1;
            let old_instrs_to_insert_idx = self.instrs_to_insert_idx;
            self.idx = MilRegisterSourceIndex::InstrToInsert(idx);

            let mut instr = mem::replace(&mut self.flat_repr.instrs_to_insert[idx], MilInstruction::dummy());
            let revisit = f(self, MilRegisterSourceMut::Instr(&mut instr));
            self.flat_repr.instrs_to_insert[idx] = instr;

            if revisit {
                self.instrs_to_insert_idx = old_instrs_to_insert_idx;
            } else {
                self.instrs_to_insert_idx += 1;
            };
        };
    }

    fn visit_phi(
        &mut self,
        idx: usize,
        f: &mut impl FnMut (&mut MilFlatReprIter<'_, 'b>, MilRegisterSourceMut) -> bool
    ) {
        loop {
            let old_instrs_to_insert_idx = self.instrs_to_insert_idx;
            self.idx = MilRegisterSourceIndex::Phi(idx);

            let mut phi = mem::replace(self.flat_repr.phis[idx], MilPhiNode::dummy());
            let revisit = f(self, MilRegisterSourceMut::Phi(&mut phi));
            *self.flat_repr.phis[idx] = phi;

            if revisit {
                self.instrs_to_insert_idx = old_instrs_to_insert_idx;
            } else {
                break;
            };
        };
    }

    fn visit_instr(
        &mut self,
        idx: usize,
        f: &mut impl FnMut (&mut MilFlatReprIter<'_, 'b>, MilRegisterSourceMut) -> bool
    ) {
        loop {
            self.visit_instrs_to_insert_before(idx, f);

            let old_instrs_to_insert_idx = self.instrs_to_insert_idx;
            self.idx = MilRegisterSourceIndex::Instr(idx);

            let mut instr = mem::replace(self.flat_repr.instrs[idx], MilInstruction::dummy());
            let revisit = f(self, MilRegisterSourceMut::Instr(&mut instr));
            *self.flat_repr.instrs[idx] = instr;

            if revisit {
                self.instrs_to_insert_idx = old_instrs_to_insert_idx;
            } else {
                break;
            };
        };
    }

    fn visit_end_instr(
        &mut self,
        idx: usize,
        f: &mut impl FnMut (&mut MilFlatReprIter<'_, 'b>, MilRegisterSourceMut) -> bool
    ) {
        loop {
            self.visit_instrs_to_insert_before(self.block_info.next_instr, f);

            let old_instrs_to_insert_idx = self.instrs_to_insert_idx;
            self.idx = MilRegisterSourceIndex::EndInstr(idx);

            let mut end_instr = mem::replace(self.flat_repr.end_instrs[idx], MilEndInstruction::dummy());
            let revisit = f(self, MilRegisterSourceMut::EndInstr(&mut end_instr));
            *self.flat_repr.end_instrs[idx] = end_instr;

            if revisit {
                self.instrs_to_insert_idx = old_instrs_to_insert_idx;
            } else {
                break;
            };
        };
    }

    pub fn get_reg(&self, reg: MilRegister) -> Option<MilRegisterSource> {
        self.flat_repr.get_reg(reg)
    }

    pub fn insert_before(&mut self, instr: MilInstruction) {
        let iti_idx = self.flat_repr.instrs_to_insert.len();
        let instr_idx = match self.idx {
            MilRegisterSourceIndex::Phi(_) => panic!("Cannot insert instructions when visiting phi nodes"),
            MilRegisterSourceIndex::Instr(i) => i,
            MilRegisterSourceIndex::InstrToInsert(_) => self.block_info.instrs_to_insert[self.instrs_to_insert_idx].0,
            MilRegisterSourceIndex::EndInstr(_) => self.block_info.next_instr
        };

        self.block_info.instrs_to_insert.insert(self.instrs_to_insert_idx, (instr_idx, iti_idx));
        if let Some(tgt) = instr.target().copied() {
            self.flat_repr.reg_sources.insert(tgt, MilRegisterSourceIndex::InstrToInsert(iti_idx));
        };

        self.flat_repr.instrs_to_insert.push(instr);
        self.instrs_to_insert_idx += 1;
    }
}
