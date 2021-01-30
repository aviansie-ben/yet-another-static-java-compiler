use std::collections::HashMap;
use std::fmt;
use std::mem;

use binary_heap_plus::BinaryHeap;
use itertools::Itertools;
use smallvec::{smallvec, SmallVec};

use crate::log_writeln;
use crate::mil::il::*;
use crate::resolve::{MethodId, ClassEnvironment};
use crate::util::FuncCache;

use super::OptimizationEnvironment;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct InliningSite(SmallVec<[MilBlockId; 4]>);

impl InliningSite {
    pub fn root() -> InliningSite {
        InliningSite(smallvec![])
    }

    pub fn sub_site(&self, block: MilBlockId) -> InliningSite {
        InliningSite(self.0.iter().copied().chain(itertools::repeat_n(block, 1)).collect())
    }
}

#[derive(Debug, Clone)]
pub struct InliningPlan {
    sites_to_inline: Vec<(InliningSite, MethodId)>
}

impl InliningPlan {
    pub fn pretty<'a>(&'a self, env: &'a ClassEnvironment) -> impl fmt::Display + 'a {
        PrettyInliningPlan(self, env)
    }
}

struct PrettyInliningPlan<'a>(&'a InliningPlan, &'a ClassEnvironment);

impl <'a> fmt::Display for PrettyInliningPlan<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for &(ref site, method_id) in self.0.sites_to_inline.iter() {
            write!(f, "[ ")?;

            for block_id in site.0.iter().copied() {
                write!(f, "{} ", block_id)?;
            };

            writeln!(f, "] <- {}", MethodName(method_id, self.1))?;
        };

        Ok(())
    }
}

pub trait Inliner {
    fn create_plan(&self, program: &MilProgram, func: &MilFunction, budget: u64) -> InliningPlan;
}

fn get_callsites<'a>(func: &'a MilFunction, func_loc: &'a InliningSite) -> impl Iterator<Item=(InliningSite, MethodId)> + 'a {
    func.block_order.iter().copied().filter_map(move |block_id| {
        let block = &func.blocks[&block_id];

        if let MilEndInstructionKind::Call(_, method_id, _, _) = block.end_instr.kind {
            Some((func_loc.sub_site(block_id), method_id))
        } else {
            None
        }
    })
}

pub struct GreedyInliner<F: Fn (&MilFunction) -> u64> {
    cost_fn: F
}

impl <F: Fn (&MilFunction) -> u64> GreedyInliner<F> {
    pub fn new(cost_fn: F) -> GreedyInliner<F> {
        GreedyInliner { cost_fn }
    }
}

impl <F: Fn (&MilFunction) -> u64> Inliner for GreedyInliner<F> {
    fn create_plan(&self, program: &MilProgram, func: &MilFunction, mut budget: u64) -> InliningPlan {
        let mut costs = FuncCache::new(|method_id| {
            (self.cost_fn)(&program.funcs[&method_id])
        });
        let mut sites = BinaryHeap::new_by(|&(_, _, a_cost): &(_, _, u64), &(_, _, b_cost): &(_, _, u64)| b_cost.cmp(&a_cost));
        let mut plan = InliningPlan { sites_to_inline: vec![] };

        sites.extend(get_callsites(func, &InliningSite::root()).map(|(loc, method_id)| (loc, method_id, *costs.get(method_id))));

        while let Some((next_loc, next_method_id, next_cost)) = sites.pop() {
            if next_cost > budget {
                break;
            };

            budget -= next_cost;
            sites.extend(get_callsites(&program.funcs[&next_method_id], &next_loc).map(|(loc, method_id)| (loc, method_id, *costs.get(method_id))));
            plan.sites_to_inline.push((next_loc, next_method_id));
        };

        plan
    }
}

fn create_block_id_mapping(inlinee: &MilFunction, block_alloc: &mut MilBlockIdAllocator) -> (Vec<MilBlockId>, HashMap<MilBlockId, MilBlockId>) {
    let mut block_order = vec![];
    let mut block_mapping = HashMap::new();

    for old_block_id in inlinee.block_order.iter().copied() {
        let new_block_id = block_alloc.allocate_one();

        block_order.push(new_block_id);
        block_mapping.insert(old_block_id, new_block_id);
    };

    (block_order, block_mapping)
}

fn create_register_mapping(inlinee: &MilFunction, reg_alloc: &mut MilRegisterAllocator, reg_map: &mut MilRegisterMap) -> HashMap<MilRegister, MilRegister> {
    let mut reg_mapping = HashMap::new();
    reg_mapping.insert(MilRegister::VOID, MilRegister::VOID);

    let mut add_reg = |old_reg| {
        reg_mapping.entry(old_reg).or_insert_with(|| {
            let new_reg = reg_alloc.allocate_one();
            reg_map.add_reg_info(new_reg, inlinee.reg_map.get_reg_info(old_reg).clone());

            new_reg
        });
    };

    for block_id in inlinee.block_order.iter().copied() {
        let block = &inlinee.blocks[&block_id];

        for phi in block.phi_nodes.iter() {
            phi.for_registers(&mut add_reg);
        };
        for instr in block.instrs.iter() {
            instr.for_registers(&mut add_reg);
        };
        block.end_instr.for_registers(&mut add_reg);
    };

    reg_mapping
}

fn inline_single_method(func: &mut MilFunction, inlinee: &MilFunction, loc: MilBlockId) -> HashMap<MilBlockId, MilBlockId> {
    let block_insertion_index = func.block_order.iter().copied().find_position(|&l| l == loc).unwrap().0 + 1;
    let return_block = func.block_order[block_insertion_index];

    let call_block = func.blocks.get_mut(&loc).unwrap();
    let call_loc = call_block.end_instr.bytecode;

    let call_instr = mem::replace(&mut call_block.end_instr.kind, MilEndInstructionKind::Nop);
    let (return_reg, args) = if let MilEndInstructionKind::Call(_, _, return_reg, args) = call_instr {
        (return_reg, args)
    } else {
        unreachable!()
    };

    let (block_order, block_map) = create_block_id_mapping(inlinee, &mut func.block_alloc);
    let reg_map = create_register_mapping(inlinee, &mut func.reg_alloc, &mut func.reg_map);

    let inline_site_id = func.inline_sites.len() as u32;
    func.inline_sites.push(MilInlineSiteInfo {
        method_id: inlinee.id,
        call_location: call_loc,
        line_map: inlinee.line_map.clone()
    });

    let mut return_phi_sources = smallvec![];

    // TODO Debug info for locals in inlined methods?

    for (old_block_id, new_block_id) in inlinee.block_order.iter().copied().zip(block_order.iter().copied()) {
        let old_block = &inlinee.blocks[&old_block_id];
        let mut new_block = old_block.clone();

        new_block.id = new_block_id;

        for phi in new_block.phi_nodes.iter_mut() {
            phi.bytecode.0 = inline_site_id;
            phi.target = *&reg_map[&phi.target];

            for &mut (ref mut op, ref mut pred) in phi.sources.iter_mut() {
                if let MilOperand::Register(reg) = *op {
                    *op = MilOperand::Register(*&reg_map[&reg]);
                };

                *pred = *&block_map[pred];
            };
        };

        for instr in new_block.instrs.iter_mut() {
            instr.bytecode.0 = inline_site_id;
            instr.for_operands_mut(|op| if let MilOperand::Register(reg) = *op {
                *op = MilOperand::Register(*&reg_map[&reg]);
            });

            if let Some(target) = instr.target_mut() {
                *target = *&reg_map[target];
            };

            if let MilInstructionKind::GetParam(idx, _, tgt) = instr.kind {
                instr.kind = MilInstructionKind::Copy(tgt, args[idx as usize].clone());
            };
        };

        new_block.end_instr.bytecode.0 = inline_site_id;
        new_block.end_instr.for_operands_mut(|op| if let MilOperand::Register(reg) = *op {
            *op = MilOperand::Register(*&reg_map[&reg]);
        });

        if let Some(target) = new_block.end_instr.target_mut() {
            *target = reg_map[*&target];
        };

        new_block.end_instr.for_successors_mut(|succ| {
            *succ = *&block_map[&succ];
        });

        if let MilEndInstructionKind::Return(ref mut val) = new_block.end_instr.kind {
            return_phi_sources.push((mem::replace(val, MilOperand::Register(MilRegister::VOID)), new_block_id));
            new_block.end_instr.kind = MilEndInstructionKind::Jump(return_block);
        };

        func.blocks.insert(new_block_id, new_block);
    };

    if return_reg != MilRegister::VOID {
        func.blocks.get_mut(&return_block).unwrap().phi_nodes.push(MilPhiNode {
            target: return_reg,
            sources: return_phi_sources,
            bytecode: call_loc
        });
    };

    func.block_order.splice(block_insertion_index..block_insertion_index, block_order.into_iter());
    block_map
}

pub fn inline_from_plan(func: &mut MilFunction, program: &MilProgram, plan: &InliningPlan) {
    let mut block_map: HashMap<_, _> = func.block_order.iter().copied()
        .map(|block_id| (InliningSite::root().sub_site(block_id), block_id))
        .collect();

    for (inline_site, method_id) in plan.sites_to_inline.iter() {
        let call_block_id = *&block_map[&inline_site];
        let callee = &program.funcs[&method_id];

        for (old_block_id, new_block_id) in inline_single_method(func, callee, call_block_id) {
            block_map.insert(inline_site.sub_site(old_block_id), new_block_id);
        };
    };
}

pub fn run_inliner<I: Inliner>(program: &mut MilProgram, inliner: I, env: &OptimizationEnvironment) {
    let mut new_funcs = HashMap::new();

    for (&method_id, func) in program.funcs.iter() {
        log_writeln!(env.log, "\n===== INLINING INTO {} =====\n\n", MethodName(method_id, env.env));

        let plan = inliner.create_plan(program, func, 30);

        log_writeln!(env.log, "{}\n", plan.pretty(env.env));

        let mut func = func.clone();
        inline_from_plan(&mut func, program, &plan);

        log_writeln!(env.log, "{}\n", func.pretty(env.env));
        new_funcs.insert(method_id, func);
    };

    program.funcs = new_funcs;
}

#[cfg(test)]
mod test {
    use crate::resolve::{ClassId, MethodId};
    use crate::mil::il::*;
    use super::*;

    #[test]
    pub fn test_basic_inline() {
        let mut f = MilFunction::new(MethodId::UNRESOLVED);

        f.blocks.insert(MilBlockId(0), MilBlock {
            id: MilBlockId(0),
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Call(
                    ClassId::UNRESOLVED,
                    MethodId::UNRESOLVED,
                    MilRegister(1),
                    vec![MilOperand::Int(0), MilOperand::Register(MilRegister(0))]
                ),
                bytecode: (!0, 0)
            },
            exception_successors: vec![]
        });
        f.blocks.insert(MilBlockId(1), MilBlock {
            id: MilBlockId(1),
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Return(MilOperand::Register(MilRegister(1))),
                bytecode: (!0, 1)
            },
            exception_successors: vec![]
        });

        f.block_order = vec![MilBlockId(0), MilBlockId(1)];
        f.block_alloc = MilBlockIdAllocator::new_from(MilBlockId(2));

        f.reg_map.add_reg_info(MilRegister(0), MilRegisterInfo { ty: MilType::Int });
        f.reg_map.add_reg_info(MilRegister(1), MilRegisterInfo { ty: MilType::Int });
        f.reg_alloc = MilRegisterAllocator::new_from(MilRegister(2));

        let mut g = MilFunction::new(MethodId::UNRESOLVED);

        g.blocks.insert(MilBlockId(0), MilBlock {
            id: MilBlockId(0),
            phi_nodes: vec![],
            instrs: vec![
                MilInstruction {
                    kind: MilInstructionKind::GetParam(0, MilClassConstraint::for_class(ClassId::PRIMITIVE_INT), MilRegister(0)),
                    bytecode: (!0, 0)
                },
                MilInstruction {
                    kind: MilInstructionKind::GetParam(1, MilClassConstraint::for_class(ClassId::PRIMITIVE_INT), MilRegister(1)),
                    bytecode: (!0, 0)
                }
            ],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Return(MilOperand::Register(MilRegister(0))),
                bytecode: (!0, 1)
            },
            exception_successors: vec![]
        });

        g.block_order = vec![MilBlockId(0)];
        g.block_alloc = MilBlockIdAllocator::new_from(MilBlockId(1));

        g.reg_map.add_reg_info(MilRegister(0), MilRegisterInfo { ty: MilType::Int });
        g.reg_map.add_reg_info(MilRegister(1), MilRegisterInfo { ty: MilType::Int });
        g.reg_alloc = MilRegisterAllocator::new_from(MilRegister(2));

        inline_single_method(&mut f, &g, MilBlockId(0));

        assert_eq!(vec![MilBlockId(0), MilBlockId(2), MilBlockId(1)], f.block_order);

        let block_0 = &f.blocks[&MilBlockId(0)];
        assert!(block_0.phi_nodes.is_empty());
        assert!(block_0.instrs.is_empty());
        assert_eq!(
            MilEndInstruction {
                kind: MilEndInstructionKind::Nop,
                bytecode: (!0, 0)
            },
            block_0.end_instr
        );

        let block_2 = &f.blocks[&MilBlockId(2)];
        assert!(block_2.phi_nodes.is_empty());
        assert_eq!(
            vec![
                MilInstruction {
                    kind: MilInstructionKind::Copy(MilRegister(2), MilOperand::Int(0)),
                    bytecode: (0, 0)
                },
                MilInstruction {
                    kind: MilInstructionKind::Copy(MilRegister(3), MilOperand::Register(MilRegister(0))),
                    bytecode: (0, 0)
                }
            ],
            block_2.instrs
        );
        assert_eq!(
            MilEndInstruction {
                kind: MilEndInstructionKind::Jump(MilBlockId(1)),
                bytecode: (0, 1)
            },
            block_2.end_instr
        );

        let block_1 = &f.blocks[&MilBlockId(1)];
        assert_eq!(
            &vec![
                MilPhiNode {
                    target: MilRegister(1),
                    sources: smallvec![
                        (MilOperand::Register(MilRegister(2)), MilBlockId(2))
                    ],
                    bytecode: (!0, 0)
                }
            ],
            &block_1.phi_nodes
        );
        assert!(block_1.instrs.is_empty());
        assert_eq!(
            MilEndInstruction {
                kind: MilEndInstructionKind::Return(MilOperand::Register(MilRegister(1))),
                bytecode: (!0, 1)
            },
            block_1.end_instr
        );

        assert_eq!(1, f.inline_sites.len());
        assert_eq!(MethodId::UNRESOLVED, f.inline_sites[0].method_id);
        assert_eq!((!0, 0), f.inline_sites[0].call_location);
    }

    #[test]
    pub fn test_inline_loop() {
        let mut f = MilFunction::new(MethodId::UNRESOLVED);

        f.blocks.insert(MilBlockId(0), MilBlock {
            id: MilBlockId(0),
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Call(
                    ClassId::UNRESOLVED,
                    MethodId::UNRESOLVED,
                    MilRegister::VOID,
                    vec![]
                ),
                bytecode: (!0, 0)
            },
            exception_successors: vec![]
        });
        f.blocks.insert(MilBlockId(1), MilBlock {
            id: MilBlockId(1),
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)),
                bytecode: (!0, 1)
            },
            exception_successors: vec![]
        });

        f.block_order = vec![MilBlockId(0), MilBlockId(1)];
        f.block_alloc = MilBlockIdAllocator::new_from(MilBlockId(2));

        let mut g = MilFunction::new(MethodId::UNRESOLVED);

        g.blocks.insert(MilBlockId(0), MilBlock {
            id: MilBlockId(0),
            phi_nodes: vec![],
            instrs: vec![],
            end_instr: MilEndInstruction {
                kind: MilEndInstructionKind::Jump(MilBlockId(0)),
                bytecode: (!0, 0)
            },
            exception_successors: vec![]
        });

        g.block_order = vec![MilBlockId(0)];
        g.block_alloc = MilBlockIdAllocator::new_from(MilBlockId(1));

        inline_single_method(&mut f, &g, MilBlockId(0));

        assert_eq!(vec![MilBlockId(0), MilBlockId(2), MilBlockId(1)], f.block_order);
        assert_eq!(
            MilEndInstructionKind::Jump(MilBlockId(2)),
            f.blocks[&MilBlockId(2)].end_instr.kind
        );
    }
}
