use std::cmp::Ordering;
use std::collections::HashMap;
use std::mem;

use itertools::Itertools;
use smallvec::SmallVec;

use crate::{log_write, log_writeln};
use crate::classfile::FieldFlags;
use crate::log::Log;
use crate::mil::flat_repr::*;
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

fn try_fold_un_op(op: MilUnOp, val: &MilOperand, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    match op {
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
        MilUnOp::D2F => try_fold_un_double(val, |x| MilOperand::Float((x as f32).to_bits())),
        MilUnOp::GetVTable => match *val {
            MilOperand::RefNull => Some(MilOperand::Poison(MilType::Addr)),
            MilOperand::KnownObject(_, class_id) => Some(MilOperand::VTable(class_id)),
            _ => None
        },
        MilUnOp::GetArrayLength => match *val {
            MilOperand::RefNull => Some(MilOperand::Poison(MilType::Int)),
            MilOperand::KnownObject(obj_id, _) => {
                let obj = known_objects.get(obj_id);

                Some(match obj.class() {
                    ResolvedClass::Array(_) => MilOperand::Int(obj.read_array_length()),
                    _ => MilOperand::Poison(MilType::Int)
                })
            },
            _ => None
        }
    }
}

fn try_fold_bin_op(op: MilBinOp, lhs: &MilOperand, rhs: &MilOperand) -> Option<MilOperand> {
    match op {
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
        MilBinOp::FRem => try_fold_bin_float_float(lhs, rhs, |x, y| MilOperand::Float((x % y).to_bits())),
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
        MilBinOp::DRem => try_fold_bin_double_double(lhs, rhs, |x, y| MilOperand::Double((x % y).to_bits())),
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
    }
}

fn try_fold_constant_instr(instr: &MilInstructionKind, env: &ClassEnvironment, known_objects: &MilKnownObjectMap) -> Option<MilOperand> {
    match *instr {
        MilInstructionKind::Copy(_, ref val) => Some(val.clone()),
        MilInstructionKind::Select(_, MilOperand::Bool(true), ref true_val, _) => Some(true_val.clone()),
        MilInstructionKind::Select(_, MilOperand::Bool(false), _, ref false_val) => Some(false_val.clone()),
        MilInstructionKind::UnOp(op, _, ref val) => try_fold_un_op(op, val, known_objects),
        MilInstructionKind::BinOp(op, _, ref lhs, ref rhs) => try_fold_bin_op(op, lhs, rhs),
        MilInstructionKind::GetField(field_id, _, _, MilOperand::KnownObject(val, _)) => {
            if env.get_field(field_id).1.flags.contains(FieldFlags::FINAL) {
                Some(MilOperand::from_const(known_objects.get(val).read_field(field_id), known_objects))
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

fn simplify_instruction(instr: &mut MilInstruction, flat_func: &mut MilFlatReprIter, reg_alloc: &mut MilRegisterAllocator, env: &ClassEnvironment, known_objects: &MilKnownObjectMap, log: &Log) -> bool {
    instr.for_operands_mut(|o| if let MilOperand::Register(_, reg) = *o {
        match flat_func.get_reg(reg) {
            Some(MilRegisterSource::Instr(instr)) => match instr.kind {
                MilInstructionKind::Copy(_, ref val) => {
                    *o = val.clone();
                },
                _ => {}
            },
            _ => {}
        };
    });

    if let MilInstructionKind::Copy(_, _) = instr.kind {
        return false;
    };

    let do_simplify = |new_instrs: &mut [MilInstructionKind], flat_func: &mut MilFlatReprIter, old_instr: &mut MilInstruction| {
        log_writeln!(log, "Simplified {}", old_instr.pretty(env));

        match new_instrs {
            &mut [ref mut insert_instrs @ .., ref mut replace_instr] => {
                for instr in insert_instrs.iter_mut() {
                    let instr = MilInstruction {
                        kind: mem::replace(instr, MilInstructionKind::Nop),
                        bytecode: old_instr.bytecode
                    };

                    log_writeln!(log, "  {}", instr.pretty(env));
                    flat_func.insert_before(instr);
                };

                let replace_instr = MilInstruction {
                    kind: mem::replace(replace_instr, MilInstructionKind::Nop),
                    bytecode: old_instr.bytecode
                };

                log_writeln!(log, "  {}", replace_instr.pretty(env));
                *old_instr = replace_instr;
            },
            _ => unreachable!()
        }
    };

    if let Some(val) = try_fold_constant_instr(&instr.kind, env, known_objects) {
        do_simplify(&mut [
            MilInstructionKind::Copy(instr.target().copied().unwrap(), val)
        ], flat_func, instr);
    };

    match instr.kind {
        MilInstructionKind::UnOp(op, tgt, ref val) => {
            let val_instr = match *val {
                MilOperand::Register(_, val) => flat_func.get_reg(val).map(MilRegisterSource::kind),
                _ => None
            };

            // -select(cond, const1, const2) => select(cond, -const1, -const2)
            match val_instr {
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::Select(_, ref cond, ref true_val, ref false_val))) => {
                    if let (Some(true_val), Some(false_val)) = (try_fold_un_op(op, true_val, known_objects), try_fold_un_op(op, false_val, known_objects)) {
                        do_simplify(&mut [
                            MilInstructionKind::Select(tgt, cond.clone(), true_val, false_val)
                        ], flat_func, instr);
                        return true;
                    };
                },
                _ => {}
            };
        },
        MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
            let lhs_instr = match *lhs {
                MilOperand::Register(_, lhs) => flat_func.get_reg(lhs).map(MilRegisterSource::kind),
                _ => None
            };
            let rhs_instr = match *rhs {
                MilOperand::Register(_, rhs) => flat_func.get_reg(rhs).map(MilRegisterSource::kind),
                _ => None
            };

            // const + reg => reg + const
            if let Some(commuted_op) = op.get_commuted_op() {
                if lhs.is_const() && !rhs.is_const() {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(commuted_op, tgt, rhs.clone(), lhs.clone())
                    ], flat_func, instr);
                    return true;
                };
            };

            // (reg + const1) + const2 => reg + (const1 + const2)
            if op.is_associative() && rhs.is_const() {
                match lhs_instr {
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(lhs_op, _, ref lhs_lhs, ref lhs_rhs))) if lhs_op == op => {
                        if let Some(new_rhs) = try_fold_bin_op(op, lhs_rhs, rhs) {
                            do_simplify(&mut [
                                MilInstructionKind::BinOp(op, tgt, lhs_lhs.clone(), new_rhs)
                            ], flat_func, instr);
                            return true;
                        };
                    },
                    _ => {}
                };
            };

            // select(reg, const1, const2) + const3 => select(reg, const1 + const3, const2 + const3)
            if rhs.is_const() {
                match lhs_instr {
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::Select(_, ref cond, ref true_val, ref false_val))) => {
                        if let (Some(true_val), Some(false_val)) = (try_fold_bin_op(op, true_val, rhs), try_fold_bin_op(op, false_val, rhs)) {
                            do_simplify(&mut [
                                MilInstructionKind::Select(tgt, cond.clone(), true_val, false_val)
                            ], flat_func, instr);
                            return true;
                        };
                    },
                    _ => {}
                };
            };

            // const1 - select(reg, const2, const3)
            if lhs.is_const() {
                match rhs_instr {
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::Select(_, ref cond, ref true_val, ref false_val))) => {
                        if let (Some(true_val), Some(false_val)) = (try_fold_bin_op(op, lhs, true_val), try_fold_bin_op(op, rhs, false_val)) {
                            do_simplify(&mut [
                                MilInstructionKind::Select(tgt, cond.clone(), true_val, false_val)
                            ], flat_func, instr);
                            return true;
                        };
                    },
                    _ => {}
                };
            };
        },
        _ => {}
    };

    match instr.kind {
        MilInstructionKind::UnOp(MilUnOp::ZNot, tgt, MilOperand::Register(_, cond)) => {
            match flat_func.get_reg(cond).map(MilRegisterSource::kind) {
                // !(x > y) => x <= y
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::ICmp(cmp), _, ref lhs, ref rhs))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::ICmp(cmp.reverse()), tgt, lhs.clone(), rhs.clone())
                    ], flat_func, instr);
                    return true;
                },
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::RCmp(cmp), _, ref lhs, ref rhs))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::RCmp(cmp.reverse()), tgt, lhs.clone(), rhs.clone())
                    ], flat_func, instr);
                    return true;
                },
                // !!x => x
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::UnOp(MilUnOp::ZNot, _, ref orig_cond))) => {
                    do_simplify(&mut [
                        MilInstructionKind::Copy(tgt, orig_cond.clone())
                    ], flat_func, instr);
                    return true;
                },
                _ => {}
            }
        },
        MilInstructionKind::UnOp(MilUnOp::INeg, tgt, MilOperand::Register(_, val)) => {
            match flat_func.get_reg(val).map(MilRegisterSource::kind) {
                // --x => x
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::UnOp(MilUnOp::INeg, _, ref val))) => {
                    do_simplify(&mut [
                        MilInstructionKind::Copy(tgt, val.clone())
                    ], flat_func, instr);
                    return true;
                },
                // -(x - y) => y - x
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::ISub, _, ref lhs, ref rhs))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::ISub, tgt, rhs.clone(), lhs.clone())
                    ], flat_func, instr);
                    return true;
                },
                // -(x * const) => x * -const
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::IMul, _, ref lhs, MilOperand::Int(rhs)))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::IMul, tgt, lhs.clone(), MilOperand::Int(rhs.wrapping_neg()))
                    ], flat_func, instr);
                    return true;
                },
                _ => {}
            }
        },
        MilInstructionKind::UnOp(MilUnOp::LNeg, tgt, MilOperand::Register(_, val)) => {
            match flat_func.get_reg(val).map(MilRegisterSource::kind) {
                // --x => x
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::UnOp(MilUnOp::LNeg, _, ref val))) => {
                    do_simplify(&mut [
                        MilInstructionKind::Copy(tgt, val.clone())
                    ], flat_func, instr);
                    return true;
                },
                // -(x - y) => y - x
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::LSub, _, ref lhs, ref rhs))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::LSub, tgt, rhs.clone(), lhs.clone())
                    ], flat_func, instr);
                    return true;
                },
                // -(x * const) => x * -const
                Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(MilBinOp::LMul, _, ref lhs, MilOperand::Long(rhs)))) => {
                    do_simplify(&mut [
                        MilInstructionKind::BinOp(MilBinOp::LMul, tgt, lhs.clone(), MilOperand::Long(rhs.wrapping_neg()))
                    ], flat_func, instr);
                    return true;
                },
                _ => {}
            }
        },
        MilInstructionKind::BinOp(MilBinOp::IAdd, tgt, ref lhs, MilOperand::Int(0)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, lhs.clone())
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::ISub, tgt, ref lhs, MilOperand::Int(0)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, lhs.clone())
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IMul, tgt, _, MilOperand::Int(0)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, MilOperand::Int(0))
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IMul, tgt, ref lhs, MilOperand::Int(1)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, lhs.clone())
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IMul, tgt, ref lhs, MilOperand::Int(-1)) => {
            do_simplify(&mut [
                MilInstructionKind::UnOp(MilUnOp::INeg, tgt, lhs.clone())
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IMul, tgt, ref lhs, MilOperand::Int(rhs)) if rhs.count_ones() == 1 => {
            do_simplify(&mut [
                MilInstructionKind::BinOp(MilBinOp::IShl, tgt, lhs.clone(), MilOperand::Int(rhs.trailing_zeros() as i32))
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IDivS, tgt, ref lhs, MilOperand::Int(1)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, lhs.clone())
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::BinOp(MilBinOp::IRemS, tgt, _, MilOperand::Int(1)) => {
            do_simplify(&mut [
                MilInstructionKind::Copy(tgt, MilOperand::Int(0))
            ], flat_func, instr);
            return true;
        },
        MilInstructionKind::Select(tgt, ref cond, ref true_val, ref false_val) => {
            // select(cond, x, x) => x
            if true_val == false_val {
                do_simplify(&mut [
                    MilInstructionKind::Copy(tgt, true_val.clone())
                ], flat_func, instr);
                return true;
            };

            match (true_val, false_val) {
                // select(cond, true, false) => cond
                (&MilOperand::Bool(true), &MilOperand::Bool(false)) => {
                    do_simplify(&mut [
                        MilInstructionKind::Copy(tgt, cond.clone())
                    ], flat_func, instr);
                    return true;
                },
                // select(cond, false, true) => !cond
                (&MilOperand::Bool(false), &MilOperand::Bool(true)) => {
                    do_simplify(&mut [
                        MilInstructionKind::UnOp(MilUnOp::ZNot, tgt, cond.clone())
                    ], flat_func, instr);
                    return true;
                },
                _ => {}
            };

            let true_instr = match *true_val {
                MilOperand::Register(_, true_reg) => flat_func.get_reg(true_reg).map(MilRegisterSource::kind),
                _ => None
            };
            let false_instr = match *false_val {
                MilOperand::Register(_, false_reg) => flat_func.get_reg(false_reg).map(MilRegisterSource::kind),
                _ => None
            };

            match (true_instr, false_instr) {
                // select(cond, -x, -y) => -select(cond, x, y)
                (
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::UnOp(true_op, _, ref true_val))),
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::UnOp(false_op, _, ref false_val)))
                ) if true_op == false_op => {
                    let temp_reg = reg_alloc.allocate_one();

                    do_simplify(&mut [
                        MilInstructionKind::Select(temp_reg, cond.clone(), true_val.clone(), false_val.clone()),
                        MilInstructionKind::UnOp(true_op, tgt, MilOperand::Register(true_val.get_type(), temp_reg))
                    ], flat_func, instr);
                    return true;
                },
                // select(cond, x + z, y + z) => select(cond, x, y) + z
                (
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(true_op, _, ref true_lhs, ref true_rhs))),
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(false_op, _, ref false_lhs, ref false_rhs)))
                ) if true_op == false_op && true_rhs == false_rhs => {
                    let temp_reg = reg_alloc.allocate_one();

                    do_simplify(&mut [
                        MilInstructionKind::Select(temp_reg, cond.clone(), true_lhs.clone(), false_lhs.clone()),
                        MilInstructionKind::BinOp(true_op, tgt, MilOperand::Register(true_lhs.get_type(), temp_reg), true_rhs.clone())
                    ], flat_func, instr);
                    return true;
                },
                // select(cond, z + x, z + y) => z + select(cond, x, y)
                (
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(true_op, _, ref true_lhs, ref true_rhs))),
                    Some(MilRegisterSourceKind::Instr(&MilInstructionKind::BinOp(false_op, _, ref false_lhs, ref false_rhs)))
                ) if true_op == false_op && true_lhs == false_lhs => {
                    let temp_reg = reg_alloc.allocate_one();

                    do_simplify(&mut [
                        MilInstructionKind::Select(temp_reg, cond.clone(), true_rhs.clone(), false_rhs.clone()),
                        MilInstructionKind::BinOp(true_op, tgt, true_lhs.clone(), MilOperand::Register(true_rhs.get_type(), temp_reg))
                    ], flat_func, instr);
                    return true;
                },
                _ => {}
            }
        }
        _ => {}
    };

    false
}

pub fn simplify_instructions(func: &mut MilFunction, env: &ClassEnvironment, known_objects: &MilKnownObjectMap, log: &Log) -> usize {
    log_writeln!(log, "\n===== INSTRUCTION SIMPLIFICATION =====\n");

    let mut flat_func = MilFlatRepr::from_blocks(&func.block_order, &mut func.blocks);
    let mut num_simplified = 0;

    let reg_alloc = &mut func.reg_alloc;

    loop {
        let mut simplified_this_loop = false;
        flat_func.visit_instrs(|flat_func, instr| {
            let simplified = match instr {
                MilRegisterSourceMut::Phi(phi) => {
                    for &mut (ref mut o, _) in phi.sources.iter_mut() {
                        if let MilOperand::Register(_, reg) = *o {
                            match flat_func.get_reg(reg) {
                                Some(MilRegisterSource::Instr(instr)) => match instr.kind {
                                    MilInstructionKind::Copy(_, ref val) => {
                                        *o = val.clone();
                                    },
                                    _ => {}
                                },
                                _ => {}
                            };
                        }
                    };
                    false
                },
                MilRegisterSourceMut::Instr(instr) => simplify_instruction(instr, flat_func, reg_alloc, env, known_objects, log),
                MilRegisterSourceMut::EndInstr(instr) => {
                    instr.for_operands_mut(|o| if let MilOperand::Register(_, reg) = *o {
                        match flat_func.get_reg(reg) {
                            Some(MilRegisterSource::Instr(instr)) => match instr.kind {
                                MilInstructionKind::Copy(_, ref val) => {
                                    *o = val.clone();
                                },
                                _ => {}
                            },
                            _ => {}
                        };
                    });
                    false
                }
            };

            if simplified {
                num_simplified += 1;
                simplified_this_loop = true;
            };
            simplified
        });

        if !simplified_this_loop {
            break;
        };
    };

    flat_func.finish();

    if num_simplified != 0 {
        log_writeln!(log, "\n===== AFTER INSTRUCTION SIMPLIFICATION =====\n\n{}", func.pretty(env));
    };

    num_simplified
}

pub fn eliminate_dead_stores(func: &mut MilFunction, env: &ClassEnvironment, log: &Log) -> usize {
    log_writeln!(log, "\n===== DEAD STORE ELIMINATION =====\n");

    let mut num_removed = 0;

    let mut used = BitVec::new();

    loop {
        used.clear();
        let mut mark_used = |op: &MilOperand| {
            if let MilOperand::Register(_, reg) = *op {
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
                    if src.0.as_reg() != Some(phi.target) {
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

    let locals_len = func.local_info.iter().map(|(id, _)| id.0).max().map_or(0, |i| i + 1);
    let local_types = (0..locals_len).map(|i| func.local_info.get(&MilLocalId(i)).map(|info| info.ty)).collect_vec();
    let mut local_phis = HashMap::new();
    let mut locals_out = HashMap::new();

    for block_id in func.block_order.iter().copied() {
        let block = func.blocks.get_mut(&block_id).unwrap();
        log_writeln!(log, "{}:", block_id);

        let reg_alloc = &mut func.reg_alloc;
        let mut locals = if !cfg.get(block_id).incoming.contains(&MilBlockId::ENTRY) {
            local_types.iter().copied().map(|ty| {
                if let Some(ty) = ty {
                    let reg = reg_alloc.allocate_one();
                    let i = block.phi_nodes.len();
                    let bc = block.initial_bytecode();
                    block.phi_nodes.push(MilPhiNode {
                        target: reg,
                        ty,
                        sources: SmallVec::new(),
                        bytecode: bc
                    });

                    (i, MilOperand::Register(ty, reg))
                } else {
                    (!0, MilOperand::Poison(MilType::Int))
                }
            }).collect_vec()
        } else {
            local_types.iter().copied().map(|ty| (!0, MilOperand::Poison(ty.unwrap_or(MilType::Int)))).collect_vec()
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
