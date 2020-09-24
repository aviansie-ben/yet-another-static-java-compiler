use std::cmp::Ordering;
use std::collections::HashMap;

use itertools::Itertools;
use smallvec::SmallVec;

use crate::{log_write, log_writeln};
use crate::classfile::FieldFlags;
use crate::log::Log;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::mil::transform;
use crate::resolve::{ClassEnvironment, ResolvedClass};

fn try_fold_constant_instr(instr: &MilInstructionKind, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    Some(match *instr {
        MilInstructionKind::Copy(_, ref val) => val.clone(),
        MilInstructionKind::UnOp(op, _, MilOperand::Int(val)) => match op {
            MilUnOp::INeg => MilOperand::Int(val.wrapping_neg()),
            MilUnOp::IExtB => MilOperand::Int(val as i8 as i32),
            MilUnOp::IExtS => MilOperand::Int(val as i16 as i32),
            MilUnOp::I2L => MilOperand::Long(val as i64),
            MilUnOp::I2F => MilOperand::Float((val as f32).to_bits()),
            MilUnOp::I2D => MilOperand::Double((val as f64).to_bits()),
            _ => unreachable!()
        },
        MilInstructionKind::UnOp(op, _, MilOperand::Long(val)) => match op {
            MilUnOp::LNeg => MilOperand::Long(val.wrapping_neg()),
            MilUnOp::L2I => MilOperand::Int(val as i32),
            MilUnOp::L2F => MilOperand::Float((val as f32).to_bits()),
            MilUnOp::L2D => MilOperand::Double((val as f64).to_bits()),
            _ => unreachable!()
        },
        MilInstructionKind::UnOp(op, _, MilOperand::Float(val_bits)) => {
            let val = f32::from_bits(val_bits);
            match op {
                MilUnOp::FNeg => MilOperand::Float((-val).to_bits()),
                MilUnOp::F2I => MilOperand::Int(
                    if val.is_nan() {
                        0
                    } else if val >= (i32::MAX as f32) {
                        i32::MAX
                    } else if val <= (i32::MIN as f32) {
                        i32::MIN
                    } else {
                        val as i32
                    }
                ),
                MilUnOp::F2L => MilOperand::Long(
                    if val.is_nan() {
                        0
                    } else if val >= (i64::MAX as f32) {
                        i64::MAX
                    } else if val <= (i64::MIN as f32) {
                        i64::MIN
                    } else {
                        val as i64
                    }
                ),
                MilUnOp::F2D => MilOperand::Double((val as f64).to_bits()),
                _ => unreachable!()
            }
        }
        MilInstructionKind::UnOp(op, _, MilOperand::Double(val_bits)) => {
            let val = f64::from_bits(val_bits);
            match op {
                MilUnOp::DNeg => MilOperand::Double((-val).to_bits()),
                MilUnOp::D2I => MilOperand::Int(
                    if val.is_nan() {
                        0
                    } else if val >= (i32::MAX as f64) {
                        i32::MAX
                    } else if val <= (i32::MIN as f64) {
                        i32::MIN
                    } else {
                        val as i32
                    }
                ),
                MilUnOp::D2L => MilOperand::Long(
                    if val.is_nan() {
                        0
                    } else if val >= (i64::MAX as f64) {
                        i64::MAX
                    } else if val <= (i64::MIN as f64) {
                        i64::MIN
                    } else {
                        val as i64
                    }
                ),
                MilUnOp::D2F => MilOperand::Float((val as f32).to_bits()),
                _ => unreachable!()
            }
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Int(lhs), MilOperand::Int(rhs)) => match op {
            MilBinOp::IAdd => MilOperand::Int(lhs.wrapping_add(rhs)),
            MilBinOp::ISub => MilOperand::Int(lhs.wrapping_sub(rhs)),
            MilBinOp::IMul => MilOperand::Int(lhs.wrapping_mul(rhs)),
            MilBinOp::IDivS | MilBinOp::IRemS if rhs == 0 => {
                return None;
            },
            MilBinOp::IDivS => MilOperand::Int(lhs.wrapping_div(rhs)),
            MilBinOp::IRemS => MilOperand::Int(lhs.wrapping_rem(rhs)),
            MilBinOp::IAnd => MilOperand::Int(lhs & rhs),
            MilBinOp::IOr => MilOperand::Int(lhs | rhs),
            MilBinOp::IXor => MilOperand::Int(lhs ^ rhs),
            MilBinOp::IShrS => MilOperand::Int(lhs >> (rhs & 0x1f)),
            MilBinOp::IShrU => MilOperand::Int(((lhs as u32) >> (rhs & 0x1f)) as i32),
            MilBinOp::IShl => MilOperand::Int(lhs << (rhs & 0x1f)),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Long(lhs), MilOperand::Int(rhs)) => match op {
            MilBinOp::LShrS => MilOperand::Long(lhs >> (rhs & 0x3f)),
            MilBinOp::LShrU => MilOperand::Long(((lhs as u64) >> (rhs & 0x3f)) as i64),
            MilBinOp::LShl => MilOperand::Long(lhs << (rhs & 0x3f)),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Long(lhs), MilOperand::Long(rhs)) => match op {
            MilBinOp::LAdd => MilOperand::Long(lhs.wrapping_add(rhs)),
            MilBinOp::LSub => MilOperand::Long(lhs.wrapping_sub(rhs)),
            MilBinOp::LMul => MilOperand::Long(lhs.wrapping_mul(rhs)),
            MilBinOp::LDivS | MilBinOp::LRemS if rhs == 0 => {
                return None;
            },
            MilBinOp::LDivS => MilOperand::Long(lhs.wrapping_div(rhs)),
            MilBinOp::LRemS => MilOperand::Long(lhs.wrapping_rem(rhs)),
            MilBinOp::LAnd => MilOperand::Long(lhs & rhs),
            MilBinOp::LOr => MilOperand::Long(lhs | rhs),
            MilBinOp::LXor => MilOperand::Long(lhs ^ rhs),
            MilBinOp::LCmp => MilOperand::Int(match lhs.cmp(&rhs) {
                Ordering::Less => -1,
                Ordering::Equal => 0,
                Ordering::Greater => 1
            }),
            _ => unreachable!()
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Float(lhs_bits), MilOperand::Float(rhs_bits)) => {
            let lhs = f32::from_bits(lhs_bits);
            let rhs = f32::from_bits(rhs_bits);

            match op {
                MilBinOp::FAdd => MilOperand::Float((lhs + rhs).to_bits()),
                MilBinOp::FSub => MilOperand::Float((lhs - rhs).to_bits()),
                MilBinOp::FMul => MilOperand::Float((lhs * rhs).to_bits()),
                MilBinOp::FDiv => MilOperand::Float((lhs / rhs).to_bits()),
                MilBinOp::FCmp(mode) => MilOperand::Int(match lhs.partial_cmp(&rhs) {
                    Some(Ordering::Equal) => 0,
                    Some(Ordering::Greater) => 1,
                    Some(Ordering::Less) => -1,
                    None => match mode {
                        MilFCmpMode::L => -1,
                        MilFCmpMode::G => 1
                    }
                }),
                _ => unreachable!()
            }
        },
        MilInstructionKind::BinOp(op, _, MilOperand::Double(lhs_bits), MilOperand::Double(rhs_bits)) => {
            let lhs = f64::from_bits(lhs_bits);
            let rhs = f64::from_bits(rhs_bits);

            match op {
                MilBinOp::DAdd => MilOperand::Double((lhs + rhs).to_bits()),
                MilBinOp::DSub => MilOperand::Double((lhs - rhs).to_bits()),
                MilBinOp::DMul => MilOperand::Double((lhs * rhs).to_bits()),
                MilBinOp::DDiv => MilOperand::Double((lhs / rhs).to_bits()),
                MilBinOp::DCmp(mode) => MilOperand::Int(match lhs.partial_cmp(&rhs) {
                    Some(Ordering::Equal) => 0,
                    Some(Ordering::Greater) => 1,
                    Some(Ordering::Less) => -1,
                    None => match mode {
                        MilFCmpMode::L => -1,
                        MilFCmpMode::G => 1
                    }
                }),
                _ => unreachable!()
            }
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
        MilInstructionKind::GetStatic(field_id, _, _) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                MilOperand::from_const(
                    known_objects.get(known_objects.refs.classes[&field_id.0]).read_field(field_id),
                    known_objects
                )
            } else {
                return None;
            }
        },
        _ => {
            return None;
        }
    })
}

pub fn fold_constant_exprs(func: &mut MilFunction, env: &ClassEnvironment, known_objects: &MilKnownObjectMap, log: &Log) -> usize {
    log_writeln!(log, "\n===== CONSTANT/COPY FOLDING =====\n");

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

                    log_writeln!(log, "Replacing {} with {}", instr.target().unwrap(), val.pretty(env));
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
        log_writeln!(log, "\n===== AFTER CONSTANT/COPY FOLDING =====\n\n{}", func.pretty(env));
    };

    num_folded
}

pub fn transform_locals_into_phis(func: &mut MilFunction, cfg: &FlowGraph<MilBlockId>, env: &ClassEnvironment, log: &Log) {
    log_writeln!(log, "\n===== LOCAL TO PHI TRANSFORMATION =====\n");

    let locals_len = func.reg_map.local_info.iter().map(|(id, _)| id.0).max().map_or(0, |i| i + 1);
    let local_types = (0..locals_len).map(|i| func.reg_map.local_info.get(&MilLocalId(i)).map_or(MilType::Void, |info| info.ty)).collect_vec();
    let mut local_phis = HashMap::new();
    let mut locals_out = HashMap::new();

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        log_writeln!(log, "{}:", block_id);

        let reg_alloc = &mut func.reg_alloc;
        let reg_map = &mut func.reg_map;
        let mut locals = if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            local_types.iter().copied().map(|ty| {
                if ty != MilType::Void {
                    let reg = reg_alloc.allocate_one();
                    reg_map.add_reg_info(reg, MilRegisterInfo { ty });

                    let i = block.phi_nodes.len();
                    let bc = block.initial_bytecode();
                    block.phi_nodes.push(MilPhiNode {
                        target: reg,
                        sources: SmallVec::new(),
                        bytecode: bc
                    });

                    (i, MilOperand::Register(reg))
                } else {
                    (!0, MilOperand::Register(MilRegister::VOID))
                }
            }).collect_vec()
        } else {
            (0..locals_len).map(|_| (!0, MilOperand::Register(MilRegister::VOID))).collect_vec()
        };

        log_write!(log, "  Created local phis: [");
        for (_, val) in locals.iter() {
            log_write!(log, " {}", val.pretty(env));
        };
        log_writeln!(log, " ]");

        for instr in block.instrs.iter_mut() {
            match instr.kind {
                MilInstructionKind::GetLocal(local_id, tgt) => {
                    let val = locals[local_id.0 as usize].1.clone();

                    log_writeln!(log, "  Replacing get_local <{}> {} with {}", local_id, tgt, val.pretty(env));
                    instr.kind = MilInstructionKind::Copy(tgt, val);
                },
                MilInstructionKind::SetLocal(local_id, ref val) => {
                    log_writeln!(log, "  Found set_local <{}> {}", local_id, val.pretty(env));
                    locals[local_id.0 as usize].1 = val.clone();
                    instr.kind = MilInstructionKind::Nop;
                },
                _ => {}
            };
        };

        transform::remove_nops(block);

        log_write!(log, "  Final local values: [");
        for (_, val) in locals.iter() {
            log_write!(log, " {}", val.pretty(env));
        };
        log_writeln!(log, " ]");

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

    log_writeln!(log, "\n===== AFTER LOCAL TO PHI TRANSFORMATION =====\n\n{}", func.pretty(env));
}
