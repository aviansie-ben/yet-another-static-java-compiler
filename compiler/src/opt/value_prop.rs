use std::collections::HashMap;

use itertools::Itertools;
use smallvec::SmallVec;

use crate::classfile::FieldFlags;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::mil::transform;
use crate::resolve::{ClassEnvironment, ClassId, ResolvedClass};

fn const_compare<T: Ord>(cmp: MilComparison, lhs: T, rhs: T) -> bool {
    match cmp {
        MilComparison::Eq => lhs == rhs,
        MilComparison::Ne => lhs != rhs,
        MilComparison::Gt => lhs > rhs,
        MilComparison::Lt => lhs < rhs,
        MilComparison::Ge => lhs >= rhs,
        MilComparison::Le => lhs <= rhs
    }
}

fn try_fold_constant_instr(instr: &MilInstructionKind, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    Some(match *instr {
        MilInstructionKind::Copy(_, ref val) => val.clone(),
        MilInstructionKind::UnOp(op, _, MilOperand::Int(val)) => match op {
            MilUnOp::INeg => MilOperand::Int(val.wrapping_neg()),
            MilUnOp::IExtB => MilOperand::Int(val as i8 as i32),
            MilUnOp::IExtS => MilOperand::Int(val as i16 as i32),
            MilUnOp::I2L => MilOperand::Long(val as i64),
            _ => unreachable!()
        },
        MilInstructionKind::UnOp(op, _, MilOperand::Long(val)) => match op {
            MilUnOp::L2I => MilOperand::Int(val as i32),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Int(lhs), MilOperand::Int(rhs)) => match op {
            MilBinOp::IAdd => MilOperand::Int(lhs.wrapping_add(rhs)),
            MilBinOp::ISub => MilOperand::Int(lhs.wrapping_sub(rhs)),
            MilBinOp::IMul => MilOperand::Int(lhs.wrapping_mul(rhs)),
            MilBinOp::IDivS if rhs == 0 => {
                return None;
            },
            MilBinOp::IDivS => MilOperand::Int(lhs.wrapping_div(rhs)),
            MilBinOp::IAnd => MilOperand::Int(lhs & rhs),
            MilBinOp::IOr => MilOperand::Int(lhs | rhs),
            MilBinOp::IXor => MilOperand::Int(lhs ^ rhs),
            MilBinOp::IShrS => MilOperand::Int(lhs >> (rhs & 0x1f)),
            MilBinOp::IShrU => MilOperand::Int(((lhs as u32) >> (rhs & 0x1f)) as i32),
            MilBinOp::IShl => MilOperand::Int(lhs << (rhs & 0x1f))
        },
        MilInstructionKind::GetField(field_id, _, _, MilOperand::KnownObject(val, _)) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                MilOperand::from_const(known_objects.get(val).read_field(field_id), known_objects)
            } else {
                return None;
            }
        },
        MilInstructionKind::GetArrayLength(_, MilOperand::KnownObject(val, _)) => {
            let val = known_objects.get(val);

            if matches!(val.class(), ResolvedClass::Array(_)) {
                MilOperand::Int(val.read_array_length())
            } else {
                return None;
            }
        },
        _ => {
            return None;
        }
    })
}

pub fn fold_constant_exprs(func: &mut MilFunction, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> usize {
    eprintln!("===== CONSTANT/COPY FOLDING =====\n");

    let mut num_folded = 0;
    let mut constants = HashMap::new();

    loop {
        for block_id in func.block_order.iter().copied() {
            let block = func.blocks.get_mut(&block_id).unwrap();

            block.instrs.drain_filter(|instr| {
                if let Some(val) = try_fold_constant_instr(&instr.kind, env, known_objects) {
                    let val = if let MilOperand::Register(reg) = val {
                        constants.get(&reg).cloned().unwrap_or(MilOperand::Register(reg))
                    } else {
                        val
                    };

                    eprintln!("Replacing {} with {}", instr.target().unwrap(), val.pretty(env));
                    constants.insert(*instr.target().unwrap(), val);
                    true
                } else {
                    false
                }
            });
        };

        if !constants.is_empty() {
            num_folded += constants.len();
            for block_id in func.block_order.iter().copied() {
                transform::replace_register(func.blocks.get_mut(&block_id).unwrap(), &constants);
            };
            constants.clear();
        } else {
            break;
        };
    };

    if num_folded != 0 {
        eprintln!("\n===== AFTER CONSTANT/COPY FOLDING =====\n\n{}", func.pretty(env));
    };

    num_folded
}

fn try_fold_constant_jump(instr: &MilEndInstructionKind, env: &ClassEnvironment, next_block_id: MilBlockId) -> Option<MilBlockId> {
    Some(match *instr {
        MilEndInstructionKind::JumpIf(cmp, tgt, MilOperand::Int(lhs), MilOperand::Int(rhs)) => {
            if const_compare(cmp, lhs, rhs) {
                tgt
            } else {
                next_block_id
            }
        },
        _ => {
            return None
        }
    })
}

pub fn transform_locals_into_phis(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment) {
    eprintln!("===== LOCAL TO PHI TRANSFORMATION =====\n");

    let locals_len = func.reg_map.local_info.iter().map(|(id, _)| id.0).max().map_or(0, |i| i + 1);
    let local_types = (0..locals_len).map(|i| func.reg_map.local_info.get(&MilLocalId(i)).map_or(MilType::Void, |info| info.ty)).collect_vec();
    let mut local_phis = HashMap::new();
    let mut locals_out = HashMap::new();

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        eprintln!("{}:", block_id);

        let reg_alloc = &mut func.reg_alloc;
        let reg_map = &mut func.reg_map;
        let mut locals = if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            local_types.iter().copied().map(|ty| {
                if ty != MilType::Void {
                    let reg = reg_alloc.allocate_one();
                    reg_map.add_reg_info(reg, MilRegisterInfo { ty });

                    let i = block.phi_nodes.len();
                    block.phi_nodes.push(MilPhiNode {
                        target: reg,
                        sources: SmallVec::new()
                    });

                    (i, MilOperand::Register(reg))
                } else {
                    (!0, MilOperand::Register(MilRegister::VOID))
                }
            }).collect_vec()
        } else {
            (0..locals_len).map(|_| (!0, MilOperand::Register(MilRegister::VOID))).collect_vec()
        };

        eprint!("  Created local phis: [");
        for (_, val) in locals.iter() {
            eprint!(" {}", val.pretty(env));
        };
        eprintln!(" ]");

        for instr in block.instrs.iter_mut() {
            match instr.kind {
                MilInstructionKind::GetLocal(local_id, tgt) => {
                    let val = locals[local_id.0 as usize].1.clone();

                    eprintln!("  Replacing get_local <{}> {} with {}", local_id, tgt, val.pretty(env));
                    instr.kind = MilInstructionKind::Copy(tgt, val);
                },
                MilInstructionKind::SetLocal(local_id, ref val) => {
                    eprintln!("  Found set_local <{}> {}", local_id, val.pretty(env));
                    locals[local_id.0 as usize].1 = val.clone();
                },
                _ => {}
            };
        };

        eprint!("  Final local values: [");
        for (_, val) in locals.iter() {
            eprint!(" {}", val.pretty(env));
        };
        eprintln!(" ]");

        local_phis.insert(block_id, locals.iter().map(|&(i, _)| i).collect_vec());
        locals_out.insert(block_id, locals.into_iter().map(|(_, val)| val).collect_vec());
    };

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();

        if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            let phis = &local_phis[&block_id];
            let pred_locals = cfg.get(block_id).incoming.iter().copied()
                .map(|pred_id| (pred_id, &locals_out[&pred_id]))
                .collect_vec();

            for (local_idx, phi) in phis.iter().copied().enumerate() {
                if phi != !0 {
                    block.phi_nodes[phi].sources = pred_locals.iter().copied()
                        .map(|(pred_id, pred_locals)| (pred_locals[local_idx].clone(), pred_id))
                        .collect();
                };
            };
        };
    };

    eprintln!("\n===== AFTER LOCAL TO PHI TRANSFORMATION =====\n\n{}", func.pretty(env));
}
