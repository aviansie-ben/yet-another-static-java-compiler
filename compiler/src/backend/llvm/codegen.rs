use std::collections::{HashMap, HashSet};
use std::ffi::CString;
use std::os::raw::c_char;
use std::ptr::NonNull;

use itertools::Itertools;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::liveness::LivenessInfo;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::{ClassId, FieldId};

use super::MochaModule;
use super::types::{LLVMTypes, VTABLE_FIRST_VSLOT_FIELD, VTABLE_ITABLE_FIELD};
use super::wrapper::*;

unsafe fn define_function(module: &MochaModule, func: &MilFunction) -> LLVMValueRef {
    let (class, method) = module.env.get_method(func.id);
    let name = CString::new(format!("{}.{}{}", class.meta.name, method.name, method.descriptor)).unwrap();

    LLVMAddFunction(module.module.ptr(), name.as_ptr(), module.types.method_types[&func.id])
}

fn create_value_ref<'a>(module: &MochaModule<'a, '_, '_>, op: &MilOperand, regs: &HashMap<MilRegister, LLVMValue<'a>>) -> LLVMValue<'a> {
    match *op {
        MilOperand::Register(r) => regs[&r],
        MilOperand::AddrNull => module.const_addr_null(),
        MilOperand::RefNull => module.const_obj_null(),
        MilOperand::KnownObject(object_id, _) => unsafe {
            LLVMValue::from_raw(LLVMConstPointerCast(
                module.find_known_object(object_id).ptr(),
                module.types.any_object_pointer
            ))
        },
        MilOperand::Bool(val) => module.const_bool(val),
        MilOperand::Int(val) => module.const_int(val),
        MilOperand::Long(val) => module.const_long(val),
        MilOperand::Float(val) => module.const_float(val),
        MilOperand::Double(val) => module.const_double(val)
    }
}

fn native_arg_type(ty: MilType, types: &LLVMTypes) -> LLVMTypeRef {
    match ty {
        MilType::Void => types.void,
        MilType::Addr => types.any_raw_pointer,
        MilType::Ref => types.any_object_pointer,
        MilType::Bool => unreachable!(),
        MilType::Int => types.int,
        MilType::Long => types.long,
        MilType::Float => types.float,
        MilType::Double => types.double
    }
}

fn register_name(reg: MilRegister) -> CString {
    if reg == MilRegister::VOID {
        CString::new("").unwrap()
    } else {
        CString::new(format!("r{}", reg.0)).unwrap()
    }
}

fn local_name(local_id: MilLocalId) -> CString {
    CString::new(format!("local_{}", local_id.0)).unwrap()
}

fn create_meta_field_gep<'a>(builder: &LLVMBuilder<'a>, field_id: FieldId, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValue<'a>>, types: &LLVMTypes) -> LLVMValue<'a> {
    let class_ty = &types.class_types[&field_id.0];
    let class_obj = obj_map[&known_objects.get(known_objects.refs.classes[&field_id.0]).as_ptr()];

    builder.build_struct_gep(class_obj, class_ty.meta_fields[&field_id] as u32, None)
}

fn create_instance_field_gep<'a>(builder: &LLVMBuilder<'a>, field_id: FieldId, obj: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    let class_ty = &types.class_types[&field_id.0];

    builder.build_struct_gep(
        builder.build_pointer_cast(obj, types.class_types[&field_id.0].field_ty, None),
        class_ty.fields[&field_id] as u32,
        None
    )
}

fn create_vtable_decompress<'a>(builder: &LLVMBuilder<'a>, compressed: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    builder.build_int_to_ptr(
        builder.build_nuw_sub(compressed, unsafe { LLVMValue::from_raw(LLVMConstInt(types.int, 8, 0)) }, None),
        types.any_raw_pointer,
        None
    )
}

fn create_itable_decompress<'a>(builder: &LLVMBuilder<'a>, compressed: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    builder.build_int_to_ptr(
        builder.build_nuw_sub(compressed, unsafe { LLVMValue::from_raw(LLVMConstInt(types.int, 8, 0)) }, None),
        unsafe { LLVMPointerType(LLVMArrayType(types.any_function_pointer, 0), 0) },
        None
    )
}

fn create_vtable_load<'a>(builder: &LLVMBuilder<'a>, obj: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    let obj = builder.build_pointer_cast(obj, types.class_types[&ClassId::JAVA_LANG_OBJECT].field_ty, None);
    let vtable = builder.build_load(builder.build_struct_gep(obj, 0, None), None);

    create_vtable_decompress(builder, vtable, types)
}

fn find_used_before_def(block: &MilBlock) -> impl Iterator<Item=MilRegister> {
    let mut used_before_def = HashSet::new();
    let mut defined = HashSet::new();

    for phi in block.phi_nodes.iter() {
        defined.insert(phi.target);
    };

    for instr in block.instrs.iter() {
        instr.for_operands(|o| if let MilOperand::Register(r) = *o {
            if r != MilRegister::VOID && !defined.contains(&r) {
                used_before_def.insert(r);
            };
        });

        if let Some(&target) = instr.target() {
            defined.insert(target);
        };
    };

    block.end_instr.for_operands(|o| if let MilOperand::Register(r) = *o {
        if r != MilRegister::VOID && !defined.contains(&r) {
            used_before_def.insert(r);
        };
    });

    used_before_def.into_iter()
}

fn set_register<'a>(builder: &LLVMBuilder<'a>, func: &MilFunction, local_regs: &mut HashMap<MilRegister, LLVMValue<'a>>, all_regs: &mut HashMap<MilRegister, LLVMValue<'a>>, reg: MilRegister, val: LLVMValue<'a>, types: &LLVMTypes) {
    if reg == MilRegister::VOID {
        return;
    };

    local_regs.insert(reg, val);

    let val = builder.build_bit_cast(val, native_arg_type(func.reg_map.get_reg_info(reg).ty, types), None);
    assert!(all_regs.insert(reg, val).is_none());
}

fn coerce_before_store<'a>(builder: &LLVMBuilder<'a>, class_id: ClassId, val: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    match class_id {
        ClassId::PRIMITIVE_BYTE | ClassId::PRIMITIVE_SHORT | ClassId::PRIMITIVE_BOOLEAN | ClassId::PRIMITIVE_CHAR => {
            builder.build_int_cast(val, types.class_types[&class_id].field_ty, None)
        },
        ClassId::PRIMITIVE_INT | ClassId::PRIMITIVE_LONG | ClassId::PRIMITIVE_FLOAT | ClassId::PRIMITIVE_DOUBLE => val,
        _ => {
            builder.build_pointer_cast(val, types.class_types[&class_id].field_ty, None)
        }
    }
}

fn coerce_after_load<'a>(builder: &LLVMBuilder<'a>, class_id: ClassId, val: LLVMValue<'a>, types: &LLVMTypes) -> LLVMValue<'a> {
    match class_id {
        ClassId::PRIMITIVE_BYTE | ClassId::PRIMITIVE_SHORT | ClassId::PRIMITIVE_BOOLEAN => {
            builder.build_int_cast_2(val, types.int, true, None)
        },
        ClassId::PRIMITIVE_CHAR => {
            builder.build_int_cast_2(val, types.int, false, None)
        },
        ClassId::PRIMITIVE_INT | ClassId::PRIMITIVE_LONG | ClassId::PRIMITIVE_FLOAT | ClassId::PRIMITIVE_DOUBLE => val,
        _ => {
            builder.build_pointer_cast(val, types.any_object_pointer, None)
        }
    }
}

fn undefined_register_value<'a>(module: &MochaModule<'a, '_, '_>, ty: MilType) -> LLVMValue<'a> {
    match ty {
        MilType::Void => module.const_addr_null(),
        MilType::Addr => module.const_addr_null(),
        MilType::Ref => module.const_obj_null(),
        MilType::Bool => module.const_bool(false),
        MilType::Int => module.const_int(0),
        MilType::Long => module.const_long(0),
        MilType::Float => module.const_float(0),
        MilType::Double => module.const_double(0)
    }
}

unsafe fn emit_basic_block<'a>(
    module: &MochaModule<'a, '_, '_>,
    func: &MilFunction,
    cfg: &FlowGraph<MilBlockId>,
    block_id: MilBlockId,
    builder: &LLVMBuilder<'a>,
    llvm_func: LLVMValue<'a>,
    locals: &mut HashMap<MilLocalId, LLVMValue<'a>>,
    llvm_blocks: &mut HashMap<MilBlockId, (LLVMBasicBlockRef, LLVMBasicBlockRef, Option<LLVMValue<'a>>)>,
    all_regs: &mut HashMap<MilRegister, LLVMValue<'a>>,
    phis_to_add: &mut Vec<(LLVMPhiValue<'a>, MilBlockId, MilOperand)>
) {
    let mut local_regs = HashMap::new();

    let block = &func.blocks[&block_id];
    let block_name = CString::new(format!("{}", block.id)).unwrap();
    let mut llvm_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func.ptr(), block_name.as_ptr());
    let llvm_start_block = llvm_block;
    let mut cond_out = None;

    LLVMPositionBuilderAtEnd(builder.ptr(), llvm_block);

    if !cfg.get(block_id).incoming.is_empty() {
        for reg in find_used_before_def(block) {
            if let Some(&val) = all_regs.get(&reg) {
                local_regs.insert(reg, val);
            } else {
                let phi = builder.build_phi(native_arg_type(func.reg_map.get_reg_info(reg).ty, &module.types), Some(register_name(reg)));

                local_regs.insert(reg, phi.into_val());

                for pred in cfg.get(block_id).incoming.iter().cloned() {
                    phis_to_add.push((phi, pred, MilOperand::Register(reg)));
                };
            };
        };
    } else {
        for reg in find_used_before_def(block) {
            local_regs.insert(reg, undefined_register_value(module, func.reg_map.get_reg_info(reg).ty));
        };
    };

    for phi in block.phi_nodes.iter() {
        if !phi.sources.is_empty() {
            let llvm_phi = builder.build_phi(native_arg_type(func.reg_map.get_reg_info(phi.target).ty, &module.types), Some(register_name(phi.target)));

            set_register(builder, func, &mut local_regs, all_regs, phi.target, llvm_phi.into_val(), &module.types);

            for (src, pred) in phi.sources.iter().cloned() {
                phis_to_add.push((llvm_phi, pred, src));
            };
        } else {
            let val = undefined_register_value(module, func.reg_map.get_reg_info(phi.target).ty);
            set_register(builder, func, &mut local_regs, all_regs, phi.target, val, &module.types);
        };
    };

    for instr in block.instrs.iter() {
        match instr.kind {
            MilInstructionKind::Nop => {},
            MilInstructionKind::Copy(tgt, ref src) => {
                let src = create_value_ref(&module, src, &local_regs);
                set_register(builder, func, &mut local_regs, all_regs, tgt, src, &module.types);
            },
            MilInstructionKind::UnOp(op, tgt, ref val) => {
                let val = create_value_ref(&module, val, &local_regs);
                set_register(builder, func, &mut local_regs, all_regs, tgt, match op {
                    MilUnOp::INeg => builder.build_neg(val, Some(register_name(tgt))),
                    MilUnOp::IExtB => builder.build_sext(
                        builder.build_trunc(val, module.types.byte, None),
                        module.types.int,
                        Some(register_name(tgt))
                    ),
                    MilUnOp::IExtS => builder.build_sext(
                        builder.build_trunc(val, module.types.byte, None),
                        module.types.int,
                        Some(register_name(tgt))
                    ),
                    MilUnOp::LNeg => builder.build_neg(val, Some(register_name(tgt))),
                    MilUnOp::FNeg => builder.build_fneg(val, Some(register_name(tgt))),
                    MilUnOp::DNeg => builder.build_fneg(val, Some(register_name(tgt))),
                    MilUnOp::I2L => builder.build_sext(val, module.types.long, Some(register_name(tgt))),
                    MilUnOp::I2F => builder.build_si_to_fp(val, module.types.float, Some(register_name(tgt))),
                    MilUnOp::I2D => builder.build_si_to_fp(val, module.types.double, Some(register_name(tgt))),
                    MilUnOp::L2I => builder.build_trunc(val, module.types.int, Some(register_name(tgt))),
                    MilUnOp::L2F => builder.build_si_to_fp(val, module.types.float, Some(register_name(tgt))),
                    MilUnOp::L2D => builder.build_si_to_fp(val, module.types.double, Some(register_name(tgt))),
                    MilUnOp::F2I => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealUNO, val, val, None),
                        module.const_int(0),
                        builder.build_select(
                            builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, val, module.const_float((i32::MAX as f32).to_bits()), None),
                            module.const_int(i32::MAX),
                            builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLE, val, module.const_float((i32::MIN as f32).to_bits()), None),
                                module.const_int(i32::MIN),
                                builder.build_fp_to_si(val, module.types.int, None),
                                None
                            ),
                            None
                        ),
                        Some(register_name(tgt))
                    ),
                    MilUnOp::F2L => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealUNO, val, val, None),
                        module.const_long(0),
                        builder.build_select(
                            builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, val, module.const_float((i64::MAX as f32).to_bits()), None),
                            module.const_long(i64::MAX),
                            builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLE, val, module.const_float((i64::MIN as f32).to_bits()), None),
                                module.const_long(i64::MIN),
                                builder.build_fp_to_si(val, module.types.long, None),
                                None
                            ),
                            None
                        ),
                        Some(register_name(tgt))
                    ),
                    MilUnOp::F2D => builder.build_fpext(val, module.types.double, Some(register_name(tgt))),
                    MilUnOp::D2I => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealUNO, val, val, None),
                        module.const_int(0),
                        builder.build_select(
                            builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, val, module.const_double((i32::MAX as f64).to_bits()), None),
                            module.const_int(i32::MAX),
                            builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLE, val, module.const_double((i32::MIN as f64).to_bits()), None),
                                module.const_int(i32::MIN),
                                builder.build_fp_to_si(val, module.types.int, None),
                                None
                            ),
                            None
                        ),
                        Some(register_name(tgt))
                    ),
                    MilUnOp::D2L => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealUNO, val, val, None),
                        module.const_long(0),
                        builder.build_select(
                            builder.build_fcmp(LLVMRealPredicate::LLVMRealOGE, val, module.const_double((i64::MAX as f64).to_bits()), None),
                            module.const_long(i64::MAX),
                            builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLE, val, module.const_double((i64::MIN as f64).to_bits()), None),
                                module.const_long(i64::MIN),
                                builder.build_fp_to_si(val, module.types.long, None),
                                None
                            ),
                            None
                        ),
                        Some(register_name(tgt))
                    ),
                    MilUnOp::D2F => builder.build_fptrunc(val, module.types.float, Some(register_name(tgt)))
                }, &module.types);
            },
            MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                let lhs = create_value_ref(&module, lhs, &local_regs);
                let rhs = create_value_ref(&module, rhs, &local_regs);
                set_register(builder, func, &mut local_regs, all_regs, tgt, match op {
                    MilBinOp::IAdd => builder.build_add(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::ISub => builder.build_sub(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IMul => builder.build_mul(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IDivS => builder.build_sdiv(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IRemS => builder.build_srem(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IAnd => builder.build_and(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IOr => builder.build_or(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IXor => builder.build_xor(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::IShrS => builder.build_ashr(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x1f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::IShrU => builder.build_lshr(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x1f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::IShl => builder.build_shl(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x1f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::LAdd => builder.build_add(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LSub => builder.build_sub(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LMul => builder.build_mul(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LDivS => builder.build_sdiv(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LRemS => builder.build_srem(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LAnd => builder.build_and(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LOr => builder.build_or(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LXor => builder.build_xor(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::LShrS => builder.build_ashr(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x3f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::LShrU => builder.build_lshr(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x3f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::LShl => builder.build_shl(
                        lhs,
                        builder.build_and(rhs, module.const_int(0x3f), None),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::LCmp => builder.build_select(
                        builder.build_icmp(LLVMIntPredicate::LLVMIntEQ, lhs, rhs, None),
                        module.const_int(0),
                        builder.build_select(
                            builder.build_icmp(LLVMIntPredicate::LLVMIntSGT, lhs, rhs, None),
                            module.const_int(1),
                            module.const_int(-1),
                            None
                        ),
                        Some(register_name(tgt))
                    ),
                    MilBinOp::FAdd => builder.build_fadd(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::FSub => builder.build_fsub(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::FMul => builder.build_fmul(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::FDiv => builder.build_fdiv(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::FCmp(mode) => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, None),
                        module.const_int(0),
                        match mode {
                            MilFCmpMode::L => builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOGT, lhs, rhs, None),
                                module.const_int(1),
                                module.const_int(-1),
                                None
                            ),
                            MilFCmpMode::G => builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLT, lhs, rhs, None),
                                module.const_int(-1),
                                module.const_int(1),
                                None
                            )
                        },
                        Some(register_name(tgt))
                    ),
                    MilBinOp::DAdd => builder.build_fadd(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::DSub => builder.build_fsub(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::DMul => builder.build_fmul(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::DDiv => builder.build_fdiv(lhs, rhs, Some(register_name(tgt))),
                    MilBinOp::DCmp(mode) => builder.build_select(
                        builder.build_fcmp(LLVMRealPredicate::LLVMRealOEQ, lhs, rhs, None),
                        module.const_int(0),
                        match mode {
                            MilFCmpMode::L => builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOGT, lhs, rhs, None),
                                module.const_int(1),
                                module.const_int(-1),
                                None
                            ),
                            MilFCmpMode::G => builder.build_select(
                                builder.build_fcmp(LLVMRealPredicate::LLVMRealOLT, lhs, rhs, None),
                                module.const_int(-1),
                                module.const_int(1),
                                None
                            )
                        },
                        Some(register_name(tgt))
                    )
                }, &module.types);
            },
            MilInstructionKind::GetParam(idx, _, tgt) => {
                set_register(builder, func, &mut local_regs, all_regs, tgt, LLVMValue::from_raw(LLVMGetParam(llvm_func.ptr(), idx as u32)), &module.types);
            },
            MilInstructionKind::GetLocal(local_id, tgt) => {
                set_register(builder, func, &mut local_regs, all_regs, tgt, builder.build_load(locals[&local_id], Some(register_name(tgt))), &module.types);
            },
            MilInstructionKind::SetLocal(local_id, ref src) => {
                let src = create_value_ref(&module, src, &local_regs);
                let local_ptr = locals[&local_id];

                builder.build_store(src, local_ptr);
            },
            MilInstructionKind::GetField(field_id, class_id, tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);

                let val = builder.build_load(
                    create_instance_field_gep(builder, field_id, obj, &module.types),
                    Some(register_name(tgt))
                );
                let val = coerce_after_load(builder, class_id, val, &module.types);

                set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutField(field_id, class_id, ref obj, ref val) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let val = create_value_ref(&module, val, &local_regs);

                builder.build_store(
                    coerce_before_store(builder, class_id, val, &module.types),
                    create_instance_field_gep(builder, field_id, obj, &module.types)
                );
            },
            MilInstructionKind::GetArrayLength(tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);

                let val = builder.build_load(
                    create_instance_field_gep(builder, FieldId(ClassId::JAVA_LANG_OBJECT_ARRAY, 0), obj,&module.types),
                    Some(register_name(tgt))
                );

                set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::GetArrayElement(class_id, tgt, ref obj, ref idx) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let idx = create_value_ref(&module, idx, &local_regs);

                let arr_data = create_instance_field_gep(builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.types);

                let val = builder.build_load(
                    builder.build_gep(arr_data, &[module.const_int(0), idx], None),
                    Some(register_name(tgt))
                );
                let val = coerce_after_load(builder, class_id, val, &module.types);

                set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutArrayElement(class_id, ref obj, ref idx, ref val) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let idx = create_value_ref(&module, idx, &local_regs);
                let val = create_value_ref(&module, val, &local_regs);

                let arr_data = create_instance_field_gep(builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.types);

                builder.build_store(
                    coerce_before_store(builder, class_id, val, &module.types),
                    builder.build_gep(arr_data, &[module.const_int(0), idx], None)
                );
            },
            MilInstructionKind::GetStatic(field_id, class_id, tgt) => {
                let val = builder.build_load(
                    create_meta_field_gep(builder, field_id, &module.known_objs, &module.obj_map, &module.types),
                    Some(register_name(tgt))
                );
                let val = coerce_after_load(builder, class_id, val, &module.types);

                set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutStatic(field_id, class_id, ref val) => {
                let val = create_value_ref(&module, val, &local_regs);

                builder.build_store(
                    coerce_before_store(builder, class_id, val, &module.types),
                    create_meta_field_gep(builder, field_id, &module.known_objs, &module.obj_map, &module.types)
                );
            },
            MilInstructionKind::AllocObj(class_id, tgt) => {
                let obj = builder.build_call(
                    LLVMValue::from_raw(module.builtins.alloc_obj),
                    &[
                        LLVMValue::from_raw(LLVMConstPointerCast(
                            module.types.class_types[&class_id].vtable,
                            module.types.any_raw_pointer
                        ))
                    ],
                    Some(register_name(tgt))
                );

                set_register(builder, func, &mut local_regs, all_regs, tgt, obj, &module.types);
            },
            MilInstructionKind::AllocArray(class_id, tgt, ref len) => {
                let len = create_value_ref(&module, len, &local_regs);
                let obj = builder.build_call(
                    LLVMValue::from_raw(module.builtins.alloc_array),
                    &[
                        LLVMValue::from_raw(LLVMConstPointerCast(
                            module.types.class_types[&class_id].vtable,
                            module.types.any_raw_pointer
                        )),
                        len
                    ],
                    Some(register_name(tgt))
                );

                set_register(builder, func, &mut local_regs, all_regs, tgt, obj, &module.types);
            },
            MilInstructionKind::GetVTable(tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let vtable = create_vtable_load(builder, obj, &module.types);

                set_register(builder, func, &mut local_regs, all_regs, tgt, vtable, &module.types);
            }
        };
    };

    match block.end_instr.kind {
        MilEndInstructionKind::Nop => {},
        MilEndInstructionKind::Unreachable => {
            builder.build_unreachable();
        },
        MilEndInstructionKind::Call(_, method_id, tgt, ref args) => {
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let val = builder.build_call(
                module.methods[&method_id],
                &args[..],
                Some(register_name(tgt))
            );

            set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
        },
        MilEndInstructionKind::CallVirtual(_, method_id, tgt, ref vtable, ref args) => {
            let (_, method) = module.env.get_method(method_id);
            let vtable = create_value_ref(&module, vtable, &local_regs);
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let vtable = builder.build_pointer_cast(
                vtable,
                LLVMPointerType(LLVMGlobalGetValueType(module.types.class_types[&method_id.0].vtable), 0),
                None
            );
            let vslot = builder.build_struct_gep(vtable, VTABLE_FIRST_VSLOT_FIELD as u32 + method.virtual_slot, None);
            let vslot = builder.build_load(vslot, None);
            let vslot = builder.build_pointer_cast(vslot, LLVMPointerType(module.types.method_types[&method_id], 0), None);

            let return_val = builder.build_call(vslot, &args[..], Some(register_name(tgt)));

            set_register(builder, func, &mut local_regs, all_regs, tgt, return_val, &module.types);
        },
        MilEndInstructionKind::CallInterface(_, method_id, tgt, ref vtable, ref args) => {
            let interface_vtable = LLVMValue::from_raw(LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(module.types.class_types[&method_id.0].vtable, module.types.long),
                    module.const_long(8).ptr()
                ),
                module.types.int,
                0
            ));

            let (_, method) = module.env.get_method(method_id);
            let vtable = create_value_ref(&module, vtable, &local_regs);
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let vtable = builder.build_pointer_cast(
                vtable,
                LLVMPointerType(LLVMGlobalGetValueType(module.types.class_types[&method_id.0].vtable), 0),
                None
            );

            let first_islot = builder.build_struct_gep(
                builder.build_load(builder.build_struct_gep(vtable, VTABLE_ITABLE_FIELD as u32, None), None),
                0,
                None
            );

            let loop_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func.ptr(), b"\0".as_ptr() as *const c_char);
            builder.build_br(loop_block);
            LLVMPositionBuilderAtEnd(builder.ptr(), loop_block);

            let islot = builder.build_phi(LLVMPointerType(module.types.itable_entry, 0), None);
            let islot_interface = builder.build_load(builder.build_struct_gep(islot.into_val(), 0, None), None);
            let next_islot = builder.build_gep(islot.into_val(), &[module.const_int(0)], None);

            islot.add_incoming(&[(llvm_block, first_islot), (loop_block, next_islot)]);

            let call_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func.ptr(), b"\0".as_ptr() as *const c_char);
            builder.build_cond_br(
                builder.build_icmp(LLVMIntPredicate::LLVMIntNE, islot_interface, interface_vtable, None),
                loop_block,
                call_block
            );

            llvm_block = call_block;
            LLVMPositionBuilderAtEnd(builder.ptr(), call_block);

            let islot = builder.build_load(
                builder.build_struct_gep(
                    create_itable_decompress(
                        builder,
                        builder.build_load(builder.build_struct_gep(islot.into_val(), 1, None), None),
                        &module.types
                    ),
                    method.virtual_slot,
                    None
                ),
                None
            );

            let return_val = builder.build_call(
                builder.build_pointer_cast(islot, LLVMPointerType(module.types.method_types[&method_id], 0), None),
                &args[..],
                Some(register_name(tgt))
            );

            set_register(builder, func, &mut local_regs, all_regs, tgt, return_val, &module.types);
        },
        MilEndInstructionKind::CallNative(ret_ty, ref name, tgt, ref args) => {
            let mut arg_tys = args.iter().map(|a| native_arg_type(a.get_type(&func.reg_map), &module.types)).collect_vec();
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let func_ty = LLVMFunctionType(
                native_arg_type(MilType::for_class(ret_ty), &module.types),
                arg_tys.as_mut_ptr(),
                arg_tys.len() as u32,
                0
            );
            let name = CString::new(name.as_str()).unwrap();
            let native_func = LLVMValue::from_raw(LLVMAddFunction(module.module.ptr(), name.as_ptr(), func_ty));

            let return_val = builder.build_call(native_func, &args[..], Some(register_name(tgt)));

            set_register(builder, func, &mut local_regs, all_regs, tgt, return_val, &module.types);
        },
        MilEndInstructionKind::Throw(ref exception) => {
            let exception = create_value_ref(&module, exception, &local_regs);

            builder.build_call(LLVMValue::from_raw(module.builtins.throw), &[exception], None);
            builder.build_unreachable();
        },
        MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)) => {
            builder.build_ret_void();
        },
        MilEndInstructionKind::Return(ref val) => {
            let val = builder.build_bit_cast(
                create_value_ref(&module, val, &local_regs),
                LLVMGetReturnType(module.types.method_types[&func.id]),
                None
            );

            builder.build_ret(val);
        },
        MilEndInstructionKind::Jump(_) => {},
        MilEndInstructionKind::JumpIfRCmp(cond, _, ref lhs, ref rhs) => {
            let lhs = create_value_ref(&module, lhs, &local_regs);
            let rhs = create_value_ref(&module, rhs, &local_regs);

            let cond = match cond {
                MilRefComparison::Eq => LLVMIntPredicate::LLVMIntEQ,
                MilRefComparison::Ne => LLVMIntPredicate::LLVMIntNE
            };

            cond_out = Some(builder.build_icmp(cond, lhs, rhs, None));
        },
        MilEndInstructionKind::JumpIfICmp(cond, _, ref lhs, ref rhs) => {
            let lhs = create_value_ref(&module, lhs, &local_regs);
            let rhs = create_value_ref(&module, rhs, &local_regs);

            let cond = match cond {
                MilIntComparison::Eq => LLVMIntPredicate::LLVMIntEQ,
                MilIntComparison::Ne => LLVMIntPredicate::LLVMIntNE,
                MilIntComparison::Gt => LLVMIntPredicate::LLVMIntSGT,
                MilIntComparison::Lt => LLVMIntPredicate::LLVMIntSLT,
                MilIntComparison::Ge => LLVMIntPredicate::LLVMIntSGE,
                MilIntComparison::Le => LLVMIntPredicate::LLVMIntSLE
            };

            cond_out = Some(builder.build_icmp(cond, lhs, rhs, None));
        }
    };

    llvm_blocks.insert(block_id, (llvm_start_block, llvm_block, cond_out));
}

unsafe fn emit_function(module: &MochaModule, func: &MilFunction) {
    let cfg = FlowGraph::for_function(func);

    let llvm_func = module.methods[&func.id];
    let builder = module.ctx.create_builder();

    LLVMSetGC(llvm_func.ptr(), "statepoint-example\0".as_ptr() as *const c_char);

    let mut llvm_blocks = HashMap::new();

    let mut locals = HashMap::new();
    let start_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func.ptr(), b"start\0".as_ptr() as *const c_char);
    LLVMPositionBuilderAtEnd(builder.ptr(), start_block);

    for (&local_id, local) in func.reg_map.local_info.iter() {
        locals.insert(
            local_id,
            builder.build_alloca(native_arg_type(local.ty, &module.types), Some(local_name(local_id)))
        );
    };

    let mut all_regs = HashMap::new();
    let mut phis_to_add = vec![];

    for block_id in func.block_order.iter().cloned() {
        emit_basic_block(&module, func, &cfg, block_id, &builder, llvm_func, &mut locals, &mut llvm_blocks, &mut all_regs, &mut phis_to_add);
    };

    for (reg, info) in func.reg_map.all_regs() {
        if !all_regs.contains_key(&reg) && info.ty != MilType::Void {
            all_regs.insert(reg, undefined_register_value(module, info.ty));
        };
    };

    for (phi, pred, val) in phis_to_add {
        phi.add_incoming(&[(llvm_blocks[&pred].1, create_value_ref(module, &val, &all_regs))]);
    };

    LLVMPositionBuilderAtEnd(builder.ptr(), start_block);
    builder.build_br(llvm_blocks[&func.block_order[0]].0);

    for (block_id, next_block_id) in func.block_order.iter().cloned().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = &func.blocks[&block_id];
        let (_, llvm_block, cond) = llvm_blocks[&block_id];

        LLVMPositionBuilderAtEnd(builder.ptr(), llvm_block);

        match block.end_instr.kind {
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Jump(tgt) => {
                builder.build_br(llvm_blocks[&tgt].0);
            },
            MilEndInstructionKind::JumpIfICmp(_, tgt, _, _) | MilEndInstructionKind::JumpIfRCmp(_, tgt, _, _) => {
                builder.build_cond_br(cond.unwrap(), llvm_blocks[&tgt].0, llvm_blocks[&next_block_id].0);
            },
            MilEndInstructionKind::Throw(_) => {},
            MilEndInstructionKind::Return(_) => {},
            _ => {
                builder.build_br(llvm_blocks[&next_block_id].0);
            }
        };
    };
}

pub(super) fn define_functions(program: &MilProgram, module: &mut MochaModule, liveness: &LivenessInfo) {
    for method_id in liveness.may_call.iter().cloned() {
        if let Some(func) = program.funcs.get(&method_id) {
            module.methods.insert(
                method_id,
                unsafe { LLVMValue::from_raw(define_function(&module, func)) }
            );
        };
    };
}

pub(super) fn emit_functions(program: &MilProgram, module: &MochaModule, liveness: &LivenessInfo) {
    for method_id in liveness.may_call.iter().sorted_by_key(|m| ((m.0).0, m.1)).cloned() {
        if let Some(func) = program.funcs.get(&method_id) {
            unsafe {
                emit_function(module, func);
            };
        };
    };
}
