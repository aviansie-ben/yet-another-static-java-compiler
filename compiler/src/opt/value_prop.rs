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
use crate::util::BitVec;

fn try_fold_un_bool<F: Fn (bool) -> MilOperand>(val: &MilOperand, f: F) -> Option<MilOperand> {
    match *val {
        MilOperand::Bool(val) => Some(f(val)),
        _ => None
    }
}

fn try_fold_un_int<F: Fn (i32) -> MilOperand>(val: &MilOperand, f: F) -> Option<MilOperand> {
    match *val {
        MilOperand::Int(val) => Some(f(val)),
        _ => None
    }
}

fn try_fold_un_long<F: Fn (i64) -> MilOperand>(val: &MilOperand, f: F) -> Option<MilOperand> {
    match *val {
        MilOperand::Long(val) => Some(f(val)),
        _ => None
    }
}

fn try_fold_un_float<F: Fn (f32) -> MilOperand>(val: &MilOperand, f: F) -> Option<MilOperand> {
    match *val {
        MilOperand::Float(val_bits) => Some(f(f32::from_bits(val_bits))),
        _ => None
    }
}

fn try_fold_un_double<F: Fn (f64) -> MilOperand>(val: &MilOperand, f: F) -> Option<MilOperand> {
    match *val {
        MilOperand::Double(val_bits) => Some(f(f64::from_bits(val_bits))),
        _ => None
    }
}

fn try_fold_bin_int_int<F: Fn (i32, i32) -> MilOperand>(lhs: &MilOperand, rhs: &MilOperand, f: F) -> Option<MilOperand> {
    match (lhs, rhs) {
        (&MilOperand::Int(lhs), &MilOperand::Int(rhs)) => Some(f(lhs, rhs)),
        _ => None
    }
}

fn try_fold_bin_long_long<F: Fn (i64, i64) -> MilOperand>(lhs: &MilOperand, rhs: &MilOperand, f: F) -> Option<MilOperand> {
    match (lhs, rhs) {
        (&MilOperand::Long(lhs), &MilOperand::Long(rhs)) => Some(f(lhs, rhs)),
        _ => None
    }
}

fn try_fold_bin_long_int<F: Fn (i64, i32) -> MilOperand>(lhs: &MilOperand, rhs: &MilOperand, f: F) -> Option<MilOperand> {
    match (lhs, rhs) {
        (&MilOperand::Long(lhs), &MilOperand::Int(rhs)) => Some(f(lhs, rhs)),
        _ => None
    }
}

fn try_fold_bin_float_float<F: Fn (f32, f32) -> MilOperand>(lhs: &MilOperand, rhs: &MilOperand, f: F) -> Option<MilOperand> {
    match (lhs, rhs) {
        (&MilOperand::Float(lhs), &MilOperand::Float(rhs)) => Some(f(f32::from_bits(lhs), f32::from_bits(rhs))),
        _ => None
    }
}

fn try_fold_bin_double_double<F: Fn (f64, f64) -> MilOperand>(lhs: &MilOperand, rhs: &MilOperand, f: F) -> Option<MilOperand> {
    match (lhs, rhs) {
        (&MilOperand::Double(lhs), &MilOperand::Double(rhs)) => Some(f(f64::from_bits(lhs), f64::from_bits(rhs))),
        _ => None
    }
}

fn try_fold_constant_instr(instr: &MilInstructionKind, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    match *instr {
        MilInstructionKind::Copy(_, ref val) => Some(val.clone()),
        MilInstructionKind::Select(_, MilOperand::Bool(true), ref true_val, _) => Some(true_val.clone()),
        MilInstructionKind::Select(_, MilOperand::Bool(false), _, ref false_val) => Some(false_val.clone()),
        MilInstructionKind::UnOp(op, _, ref val) => match op {
            MilUnOp::ZNot => try_fold_un_bool(val, |x| MilOperand::Bool(!x)),
            MilUnOp::INeg => try_fold_un_int(val, |x| MilOperand::Int(x.wrapping_neg())),
            MilUnOp::IExtB => try_fold_un_int(val, |x| MilOperand::Int(x as i8 as i32)),
            MilUnOp::IExtS => try_fold_un_int(val, |x| MilOperand::Int(x as i16 as i32)),
            MilUnOp::LNeg => try_fold_un_long(val, |x| MilOperand::Long(x.wrapping_neg())),
            MilUnOp::FNeg => try_fold_un_float(val, |x| MilOperand::Float((-x).to_bits())),
            MilUnOp::DNeg => try_fold_un_double(val, |x| MilOperand::Double((-x).to_bits())),
            MilUnOp::I2L => try_fold_un_int(val, |x| MilOperand::Long(x as i64)),
            MilUnOp::I2D => try_fold_un_int(val, |x| MilOperand::Double((x as f64).to_bits())),
            MilUnOp::I2F => try_fold_un_int(val, |x| MilOperand::Float((x as f32).to_bits())),
            MilUnOp::L2I => try_fold_un_long(val, |x| MilOperand::Int(x as i32)),
            MilUnOp::L2F => try_fold_un_long(val, |x| MilOperand::Float((x as f32).to_bits())),
            MilUnOp::L2D => try_fold_un_long(val, |x| MilOperand::Double((x as f64).to_bits())),
            MilUnOp::F2I => try_fold_un_float(val, |x| MilOperand::Int(
                if x.is_nan() {
                    0
                } else if x >= (i32::MAX as f32) {
                    i32::MAX
                } else if x <= (i32::MIN as f32) {
                    i32::MIN
                } else {
                    x as i32
                }
            )),
            MilUnOp::F2L => try_fold_un_float(val, |x| MilOperand::Long(
                if x.is_nan() {
                    0
                } else if x >= (i64::MAX as f32) {
                    i64::MAX
                } else if x <= (i64::MIN as f32) {
                    i64::MIN
                } else {
                    x as i64
                }
            )),
            MilUnOp::F2D => try_fold_un_float(val, |x| MilOperand::Double((x as f64).to_bits())),
            MilUnOp::D2I => try_fold_un_double(val, |x| MilOperand::Int(
                if x.is_nan() {
                    0
                } else if x >= (i32::MAX as f64) {
                    i32::MAX
                } else if x <= (i32::MIN as f64) {
                    i32::MIN
                } else {
                    x as i32
                }
            )),
            MilUnOp::D2L => try_fold_un_double(val, |x| MilOperand::Long(
                if x.is_nan() {
                    0
                } else if x >= (i64::MAX as f64) {
                    i64::MAX
                } else if x <= (i64::MIN as f64) {
                    i64::MIN
                } else {
                    x as i64
                }
            )),
            MilUnOp::D2F => try_fold_un_double(val, |x| MilOperand::Float((x as f32).to_bits()))
        },
        MilInstructionKind::BinOp(op, _, ref lhs, ref rhs) => match op {
            MilBinOp::IAdd => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x.wrapping_add(y))),
            MilBinOp::ISub => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x.wrapping_sub(y))),
            MilBinOp::IMul => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x.wrapping_mul(y))),
            MilBinOp::IDivS => match (lhs, rhs) {
                (&MilOperand::Int(_), &MilOperand::Int(0)) => None,
                (&MilOperand::Int(x), &MilOperand::Int(y)) => Some(MilOperand::Int(x.wrapping_div(y))),
                _ => None
            },
            MilBinOp::IRemS => match (lhs, rhs) {
                (&MilOperand::Int(_), &MilOperand::Int(0)) => None,
                (&MilOperand::Int(x), &MilOperand::Int(y)) => Some(MilOperand::Int(x.wrapping_rem(y))),
                _ => None
            },
            MilBinOp::IAnd => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x & y)),
            MilBinOp::IOr => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x | y)),
            MilBinOp::IXor => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x ^ y)),
            MilBinOp::IShrS => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x >> (y & 0x1f))),
            MilBinOp::IShrU => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(((x as u32) >> (y & 0x1f)) as i32)),
            MilBinOp::IShl => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Int(x << (y & 0x1f))),
            MilBinOp::ICmp(cmp) => try_fold_bin_int_int(lhs, rhs, |x, y| MilOperand::Bool(match cmp {
                MilIntComparison::Eq => x == y,
                MilIntComparison::Ne => x != y,
                MilIntComparison::Gt => x > y,
                MilIntComparison::Lt => x < y,
                MilIntComparison::Ge => x >= y,
                MilIntComparison::Le => x <= y
            })),
            MilBinOp::LAdd => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x.wrapping_add(y))),
            MilBinOp::LSub => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x.wrapping_sub(y))),
            MilBinOp::LMul => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x.wrapping_mul(y))),
            MilBinOp::LDivS => match (lhs, rhs) {
                (&MilOperand::Long(_), &MilOperand::Long(0)) => None,
                (&MilOperand::Long(x), &MilOperand::Long(y)) => Some(MilOperand::Long(x.wrapping_div(y))),
                _ => None
            },
            MilBinOp::LRemS => match (lhs, rhs) {
                (&MilOperand::Long(_), &MilOperand::Long(0)) => None,
                (&MilOperand::Long(x), &MilOperand::Long(y)) => Some(MilOperand::Long(x.wrapping_rem(y))),
                _ => None
            },
            MilBinOp::LAnd => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x & y)),
            MilBinOp::LOr => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x | y)),
            MilBinOp::LXor => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Long(x ^ y)),
            MilBinOp::LShrS => try_fold_bin_long_int(lhs, rhs, |x, y| MilOperand::Long(x >> (y & 0x3f))),
            MilBinOp::LShrU => try_fold_bin_long_int(lhs, rhs, |x, y| MilOperand::Long(((x as u64) >> (y & 0x3f)) as i64)),
            MilBinOp::LShl => try_fold_bin_long_int(lhs, rhs, |x, y| MilOperand::Long(x << (y & 0x3f))),
            MilBinOp::LCmp => try_fold_bin_long_long(lhs, rhs, |x, y| MilOperand::Int(
                match x.cmp(&y) {
                    Ordering::Less => -1,
                    Ordering::Equal => 0,
                    Ordering::Greater => 1
                }
            )),
            MilBinOp::FAdd => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Float((x + y).to_bits())),
            MilBinOp::FSub => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Float((x - y).to_bits())),
            MilBinOp::FMul => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Float((x * y).to_bits())),
            MilBinOp::FDiv => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Float((x / y).to_bits())),
            MilBinOp::FCmp(mode) => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Int(
                match x.partial_cmp(&y) {
                    Some(Ordering::Less) => -1,
                    Some(Ordering::Equal) => 0,
                    Some(Ordering::Greater) => 1,
                    None => match mode {
                        MilFCmpMode::G => 1,
                        MilFCmpMode::L => -1
                    }
                }
            )),
            MilBinOp::DAdd => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Double((x + y).to_bits())),
            MilBinOp::DSub => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Double((x - y).to_bits())),
            MilBinOp::DMul => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Double((x * y).to_bits())),
            MilBinOp::DDiv => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Double((x / y).to_bits())),
            MilBinOp::DCmp(mode) => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Int(
                match x.partial_cmp(&y) {
                    Some(Ordering::Less) => -1,
                    Some(Ordering::Equal) => 0,
                    Some(Ordering::Greater) => 1,
                    None => match mode {
                        MilFCmpMode::G => 1,
                        MilFCmpMode::L => -1
                    }
                }
            )),
            MilBinOp::RCmp(cmp) => {
                let eq = match (lhs, rhs) {
                    (&MilOperand::RefNull, &MilOperand::RefNull) => Some(true),
                    (&MilOperand::KnownObject(_, _), &MilOperand::RefNull) => Some(false),
                    (&MilOperand::RefNull, &MilOperand::KnownObject(_, _)) => Some(false),
                    (&MilOperand::KnownObject(lhs, _), &MilOperand::KnownObject(rhs, _)) => Some(lhs == rhs),
                    _ => None
                };

                eq.map(|eq| MilOperand::Bool(match cmp {
                    MilRefComparison::Eq => eq,
                    MilRefComparison::Ne => !eq
                }))
            }
        },
        MilInstructionKind::GetField(field_id, _, _, MilOperand::KnownObject(val, _)) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                Some(MilOperand::from_const(known_objects.get(val).read_field(field_id), known_objects))
            } else {
                None
            }
        },
        MilInstructionKind::GetArrayLength(_, MilOperand::KnownObject(val, _)) => {
            let val = known_objects.get(val);

            if matches!(val.class(), ResolvedClass::Array(_)) {
                Some(MilOperand::Int(val.read_array_length()))
            } else {
                None
            }
        },
        MilInstructionKind::GetStatic(field_id, _, _) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                Some(MilOperand::from_const(
                    known_objects.get(known_objects.refs.classes[&field_id.0]).read_field(field_id),
                    known_objects
                ))
            } else {
                None
            }
        },
        _ => None
    }
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

pub fn eliminate_dead_stores(func: &mut MilFunction, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== DEAD STORE ELIMINATION =====\n");

    let mut num_removed = 0;

    let mut used = BitVec::new();

    loop {
        used.clear();
        let mut mark_used = |op: &MilOperand| {
            if let MilOperand::Register(reg) = *op {
                if reg != MilRegister::VOID {
                    used.set(reg, true);
                };
            };
        };

        for block in func.block_order.iter() {
            let block = &func.blocks[&block];

            for phi in block.phi_nodes.iter() {
                for src in phi.sources.iter() {
                    // Unlike other types of instructions, phi nodes can legally be self-referencial. However, a self-reference shouldn't
                    // count as an actual use of the phi node's value, as the removal of the phi node would also remove the use.
                    if &src.0 != &MilOperand::Register(phi.target) {
                        mark_used(&src.0);
                    };
                };
            };

            for instr in block.instrs.iter() {
                instr.for_operands(&mut mark_used);
            };

            block.end_instr.for_operands(&mut mark_used);
        };

        let mut new_num_removed = 0;
        for block in func.block_order.iter() {
            let block = func.blocks.get_mut(&block).unwrap();

            for phi in block.phi_nodes.drain_filter(|phi| !used.get(phi.target)) {
                log_writeln!(log, "Removed unused {}", phi.pretty(env));
                new_num_removed += 1;
            };

            for instr in block.instrs.drain_filter(|instr| {
                if let Some(&target) = instr.target() {
                    target == MilRegister::VOID || !used.get(target)
                } else {
                    false
                }
            }) {
                log_writeln!(log, "Removed unused {}", instr.pretty(env));
                new_num_removed += 1;
            };
        };

        num_removed += new_num_removed;
        if new_num_removed == 0 {
            break;
        };
    };

    if num_removed != 0 {
        log_writeln!(log, "\n===== AFTER DEAD STORE ELIMINATION =====\n\n{}", func.pretty(env));
    };

    num_removed
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
