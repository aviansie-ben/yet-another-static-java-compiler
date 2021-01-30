use std::collections::HashMap;
use std::convert::TryInto;
use std::ffi::CString;
use std::os::raw::c_char;

use itertools::Itertools;
use llvm_sys::core::*;
use llvm_sys::debuginfo::LLVMDIFlags;
use llvm_sys::prelude::*;

use crate::classfile::PrimitiveType;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId, ResolvedClass};
use crate::layout;
use crate::liveness::LivenessInfo;
use crate::static_heap::{JavaStaticRef, JAVA_LANG_CLASS_VTABLE_PTR_FIELD};
use crate::static_interp::Value;

use super::MochaModule;
use super::wrapper::*;

pub const VTABLE_OBJ_SIZE_FIELD: usize = 0;
pub const VTABLE_DEPTH_FIELD: usize = 1;
pub const VTABLE_FLAGS_FIELD: usize = 2;
pub const VTABLE_MODIFIERS_FIELD: usize = 3;
pub const VTABLE_NUM_INTERFACES_FIELD: usize = 4;
pub const VTABLE_ARRAY_VTABLE_FIELD: usize = 5;
pub const VTABLE_TYPE_SPECIFIC_INFO_FIELD: usize = 6;
pub const VTABLE_SUPER_VTABLES_FIELD: usize = 7;
pub const VTABLE_CLASS_OBJ_FIELD: usize = 8;
pub const VTABLE_ITABLE_FIELD: usize = 9;
pub const VTABLE_FIRST_VSLOT_FIELD: usize = 10;

#[derive(Debug)]
pub struct LLVMClassType {
    pub name: CString,
    pub value_ty: LLVMTypeRef,
    pub field_ty: LLVMTypeRef,
    pub param_ty: LLVMTypeRef,
    pub num_fields: usize,
    pub fields: HashMap<FieldId, usize>,
    pub pads: Vec<(u32, u32)>,
    pub meta_ty: LLVMTypeRef,
    pub num_meta_fields: usize,
    pub meta_fields: HashMap<FieldId, usize>,
    pub meta_pads: Vec<(u32, u32)>,
    pub vtable: LLVMValueRef,
    pub itable: LLVMValueRef
}

#[derive(Debug)]
pub struct LLVMTypes {
    pub void: LLVMTypeRef,
    pub byte: LLVMTypeRef,
    pub short: LLVMTypeRef,
    pub int: LLVMTypeRef,
    pub long: LLVMTypeRef,
    pub float: LLVMTypeRef,
    pub double: LLVMTypeRef,
    pub bool: LLVMTypeRef,
    pub any_function_pointer: LLVMTypeRef,
    pub any_raw_pointer: LLVMTypeRef,
    pub any_object_pointer: LLVMTypeRef,
    pub itable_entry: LLVMTypeRef,
    pub class_types: HashMap<ClassId, LLVMClassType>,
    pub method_types: HashMap<MethodId, LLVMTypeRef>
}

unsafe fn lay_out_fields(env: &ClassEnvironment, fields: impl IntoIterator<Item=(FieldId, u32)>, llvm_fields: &mut Vec<LLVMTypeRef>, field_map: &mut HashMap<FieldId, usize>, pads: &mut Vec<(u32, u32)>, mut current_size: u32, types: &LLVMTypes) {
    for (f, off) in fields {
        assert!(off >= current_size);
        if off != current_size {
            pads.push((llvm_fields.len() as u32, off - current_size));
            llvm_fields.push(LLVMArrayType(types.byte, off - current_size));
        };

        let field = env.get_field(f).1;

        if field.class_id != ClassId::UNRESOLVED {
            field_map.insert(f, llvm_fields.len());
            llvm_fields.push(types.class_types[&field.class_id].field_ty);
            current_size = off + layout::get_field_size_align(env, field.class_id).0;
        };
    };
}

unsafe fn fill_type(env: &ClassEnvironment, class_id: ClassId, liveness: &LivenessInfo, module: &LLVMModule, types: &mut LLVMTypes) {
    let mut meta_fields = vec![types.class_types[&ClassId::JAVA_LANG_CLASS].value_ty];
    let mut meta_pads = vec![];
    let mut meta_field_map = HashMap::new();

    match **env.get(class_id) {
        ResolvedClass::User(ref class) => {
            if liveness.may_construct.contains(&class_id) {
                let mut fields = vec![types.int, types.int];
                let mut pads = vec![];
                let mut field_map = HashMap::new();

                lay_out_fields(
                    env,
                    class.layout.fields.iter().cloned().sorted_by_key(|&(_, off)| off),
                    &mut fields,
                    &mut field_map,
                    &mut pads,
                    8,
                    &types
                );

                let class_type = types.class_types.get_mut(&class_id).unwrap();

                class_type.num_fields = fields.len();
                class_type.fields = field_map;
                class_type.pads = pads;
                LLVMStructSetBody(class_type.value_ty, fields.as_mut_ptr(), fields.len() as u32, 1);
            };

            lay_out_fields(
                env,
                class.layout.static_fields.iter().cloned().sorted_by_key(|&(_, off)| off),
                &mut meta_fields,
                &mut meta_field_map,
                &mut meta_pads,
                layout::JAVA_LANG_CLASS_SIZE,
                &types
            );
        },
        ResolvedClass::Array(elem_id) => {
            if liveness.may_construct.contains(&class_id) {
                let align = crate::layout::get_field_size_align(env, elem_id).1;

                let mut fields = vec![types.int, types.int, types.int];
                let mut pads = vec![];
                let mut field_map = HashMap::new();
                let current_size = 12;

                field_map.insert(FieldId(class_id, 0), 2);

                if current_size % align != 0 {
                    let pad = align - (current_size % align);
                    pads.push((fields.len() as u32, pad));
                    fields.push(LLVMArrayType(types.byte, pad));
                };

                field_map.insert(FieldId(class_id, 1), fields.len());
                fields.push(LLVMArrayType(
                    types.class_types[&elem_id].field_ty,
                    0
                ));

                let class_type = types.class_types.get_mut(&class_id).unwrap();

                class_type.num_fields = fields.len();
                class_type.fields = field_map;
                class_type.pads = pads;
                LLVMStructSetBody(class_type.value_ty, fields.as_mut_ptr(), fields.len() as u32, 1);
            };
        },
        ResolvedClass::Primitive(_) => {}
    };

    let vslot_class = match **env.get(class_id) {
        ResolvedClass::User(ref class) => Some(class),
        ResolvedClass::Array(_) => Some(env.get(ClassId::JAVA_LANG_OBJECT).as_user_class()),
        _ => None
    };

    let (num_vslots, num_islots) = if let Some(vslot_class) = vslot_class {
        (vslot_class.layout.virtual_slots.len(), vslot_class.layout.interface_slots.len())
    } else {
        (0, 0)
    };

    let itable_type = LLVMArrayType(types.itable_entry, num_islots as u32);
    let itable = if num_islots != 0 {
        let itable_name = CString::new(format!("itable_{}", env.get(class_id).name(env))).unwrap();
        LLVMAddGlobal(module.ptr(), itable_type, itable_name.as_ptr())
    } else {
        std::ptr::null_mut()
    };

    let mut vtable_fields = vec![types.any_function_pointer; num_vslots + VTABLE_FIRST_VSLOT_FIELD];

    vtable_fields[VTABLE_OBJ_SIZE_FIELD] = types.int;
    vtable_fields[VTABLE_DEPTH_FIELD] = types.short;
    vtable_fields[VTABLE_FLAGS_FIELD] = types.short;
    vtable_fields[VTABLE_MODIFIERS_FIELD] = types.short;
    vtable_fields[VTABLE_NUM_INTERFACES_FIELD] = types.short;
    vtable_fields[VTABLE_ARRAY_VTABLE_FIELD] = types.int;
    vtable_fields[VTABLE_TYPE_SPECIFIC_INFO_FIELD] = types.long;
    vtable_fields[VTABLE_SUPER_VTABLES_FIELD] = LLVMPointerType(types.int, 0);
    vtable_fields[VTABLE_CLASS_OBJ_FIELD] = LLVMPointerType(types.class_types[&class_id].meta_ty, 1);
    vtable_fields[VTABLE_ITABLE_FIELD] = LLVMPointerType(itable_type, 0);

    let vtable_name = CString::new(format!("vtable_{}", env.get(class_id).name(env))).unwrap();
    let vtable = LLVMAddGlobal(module.ptr(), LLVMStructType(vtable_fields.as_mut_ptr(), vtable_fields.len() as u32, 0), vtable_name.as_ptr());
    LLVMSetSection(vtable, b".mocha_vtables\0".as_ptr() as *mut c_char);

    let mut class_type = types.class_types.get_mut(&class_id).unwrap();

    LLVMStructSetBody(class_type.meta_ty, meta_fields.as_mut_ptr(), meta_fields.len() as u32, 1);
    class_type.num_meta_fields = meta_fields.len();
    class_type.meta_fields = meta_field_map;
    class_type.meta_pads = meta_pads;
    class_type.vtable = vtable;
    class_type.itable = itable;
}

pub fn create_types(env: &ClassEnvironment, liveness: &LivenessInfo, ctx: &LLVMContext, module: &LLVMModule) -> LLVMTypes {
    unsafe {
        let void = LLVMVoidTypeInContext(ctx.ptr());
        let byte = LLVMInt8TypeInContext(ctx.ptr());
        let short = LLVMInt16TypeInContext(ctx.ptr());
        let int = LLVMInt32TypeInContext(ctx.ptr());
        let long = LLVMInt64TypeInContext(ctx.ptr());
        let float = LLVMFloatTypeInContext(ctx.ptr());
        let double = LLVMDoubleTypeInContext(ctx.ptr());
        let bool = LLVMInt1TypeInContext(ctx.ptr());

        let any_function_pointer = LLVMPointerType(LLVMFunctionType(void, std::ptr::null_mut(), 0, 0), 0);
        let any_raw_pointer = LLVMPointerType(byte, 0);
        let any_object_pointer = LLVMPointerType(byte, 1);
        let itable_entry = LLVMStructType([int, int].as_mut_ptr(), 2, 0);

        let mut types = LLVMTypes {
            void,
            byte,
            short,
            int,
            long,
            float,
            double,
            bool,
            any_function_pointer,
            any_raw_pointer,
            any_object_pointer,
            itable_entry,
            class_types: env.class_ids().map(|class_id| {
                (class_id, {
                    let class = env.get(class_id);
                    let name = CString::new(&*class.name(env)).unwrap();

                    let (value_ty, field_ty, param_ty) = match **class {
                        ResolvedClass::User(_) | ResolvedClass::Array(_) => {
                            let value_ty = LLVMStructCreateNamed(ctx.ptr(), name.as_ptr());

                            (value_ty, LLVMPointerType(value_ty, 1), any_object_pointer)
                        },
                        ResolvedClass::Primitive(None) => (void, void, void),
                        ResolvedClass::Primitive(Some(PrimitiveType::Byte)) => (byte, byte, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Boolean)) => (byte, byte, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Short)) => (short, short, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Char)) => (short, short, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Int)) => (int, int, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Long)) => (long, long, long),
                        ResolvedClass::Primitive(Some(PrimitiveType::Float)) => (float, float, float),
                        ResolvedClass::Primitive(Some(PrimitiveType::Double)) => (double, double, double)
                    };

                    let meta_name = CString::new(format!("java/lang/Class#{}", class.name(env))).unwrap();
                    let meta_ty = LLVMStructCreateNamed(ctx.ptr(), meta_name.as_ptr());

                    LLVMClassType {
                        name,
                        value_ty,
                        field_ty,
                        param_ty,
                        num_fields: 0,
                        fields: HashMap::new(),
                        pads: vec![],
                        meta_ty,
                        num_meta_fields: 0,
                        meta_fields: HashMap::new(),
                        meta_pads: vec![],
                        vtable: std::ptr::null_mut(),
                        itable: std::ptr::null_mut()
                    }
                })
            }).collect(),
            method_types: HashMap::new()
        };

        types.method_types = liveness.may_call.iter().cloned().map(|method_id| {
            let (_, method) = env.get_method(method_id);

            let mut param_types = method.param_types.iter().cloned().map(|class_id| types.class_types[&class_id].param_ty).collect_vec();
            let return_type = types.class_types[&method.return_type].param_ty;

            (method_id, LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0))
        }).collect();

        for class_id in env.class_ids() {
            fill_type(env, class_id, liveness, module, &mut types);
        };

        types
    }
}

unsafe fn emit_itable(module: &MochaModule, class_id: ClassId) {
    let islot_class = match **module.env.get(class_id) {
        ResolvedClass::User(ref class) => Some(class),
        ResolvedClass::Array(_) => Some(module.env.get(ClassId::JAVA_LANG_OBJECT).as_user_class()),
        _ => None
    };

    let mut islots = if let Some(islot_class) = islot_class {
        islot_class.layout.interface_slots.iter().map(|&(interface_id, ref islots)| {
            let itable_entry = if !islots.is_empty() {
                let itable_entry_name = CString::new(format!("itable_{}_{}", module.env.get(class_id).name(module.env), module.env.get(interface_id).name(module.env))).unwrap();
                let itable_entry = LLVMAddGlobal(module.module.ptr(), LLVMArrayType(module.types.any_function_pointer, islots.len() as u32), itable_entry_name.as_ptr());

                let mut islots = islots.iter().cloned().map(|method_id| {
                    module.methods.get(&method_id).cloned().map_or_else(
                        || LLVMConstNull(module.types.any_function_pointer),
                        |f| LLVMConstBitCast(f.into_val().ptr(), module.types.any_function_pointer)
                    )
                }).collect_vec();

                LLVMSetInitializer(itable_entry, LLVMConstArray(module.types.any_function_pointer, islots.as_mut_ptr(), islots.len() as u32));
                LLVMConstIntCast(
                    LLVMConstAdd(
                        LLVMConstPointerCast(itable_entry, module.types.long),
                        module.const_long(8).ptr()
                    ),
                    module.types.int,
                    0
                )
            } else {
                module.const_int(16).ptr()
            };

            LLVMConstStruct(
                [
                    LLVMConstIntCast(
                        LLVMConstAdd(
                            LLVMConstPointerCast(module.types.class_types[&interface_id].vtable, module.types.long),
                            module.const_long(8).ptr()
                        ),
                        module.types.int,
                        0
                    ),
                    itable_entry
                ].as_mut_ptr(),
                2,
                0
            )
        }).collect_vec()
    } else {
        vec![]
    };

    if !islots.is_empty() {
        let ty = &module.types.class_types[&class_id];

        LLVMSetInitializer(ty.itable, LLVMConstArray(module.types.itable_entry, islots.as_mut_ptr(), islots.len() as u32));
    };
}

unsafe fn emit_vtable(module: &MochaModule, class_id: ClassId, liveness: &LivenessInfo) {
    let class_obj = module.find_class_object(class_id).ptr();
    let vslot_class = match **module.env.get(class_id) {
        ResolvedClass::User(ref class) => Some(class),
        ResolvedClass::Array(_) => Some(module.env.get(ClassId::JAVA_LANG_OBJECT).as_user_class()),
        _ => None
    };

    let vslots = if let Some(vslot_class) = vslot_class {
        vslot_class.layout.virtual_slots.iter().cloned().map(|method_id| {
            module.methods.get(&method_id).cloned().map_or_else(
                || LLVMConstNull(module.types.any_function_pointer),
                |f| LLVMConstBitCast(f.into_val().ptr(), module.types.any_function_pointer)
            )
        }).collect_vec()
    } else {
        vec![]
    };

    let (flags, size, type_specific_info) = match **module.env.get(class_id) {
        ResolvedClass::User(ref class) => (0x0000, class.layout.size, module.const_long(0).ptr()),
        ResolvedClass::Array(elem_id) => (
            0x0002,
            layout::get_array_header_size(
                layout::get_field_size_align(module.env, elem_id).1
            ).try_into().unwrap(),
            LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(module.types.class_types[&elem_id].vtable, module.types.long),
                    module.const_long(8).ptr()
                ),
                module.types.long,
                0
            )
        ),
        ResolvedClass::Primitive(None) => (0x0001, 0, module.const_long(b'V'.into()).ptr()),
        ResolvedClass::Primitive(Some(ty)) => (0x0001, 0, module.const_long(ty.as_char().into()).ptr())
    };

    let ty = &module.types.class_types[&class_id];

    let mut vtable_fields = [
        module.const_int(size as i32).ptr(),
        module.const_short(0).ptr(),
        module.const_short(flags).ptr(),
        module.const_short(0).ptr(),
        module.const_short(0).ptr(),
        if let Some(array_class_id) = module.env.try_find_array(class_id) {
            if liveness.may_construct.contains(&array_class_id) {
                LLVMConstIntCast(
                    LLVMConstAdd(
                        LLVMConstPointerCast(module.types.class_types[&array_class_id].vtable, module.types.long),
                        module.const_long(8).ptr()
                    ),
                    module.types.int,
                    0
                )
            } else {
                module.const_int(0).ptr()
            }
        } else {
            module.const_int(0).ptr()
        },
        type_specific_info,
        LLVMConstNull(LLVMPointerType(module.types.int, 0)),
        class_obj,
        if ty.itable.is_null() {
            LLVMConstNull(LLVMStructGetTypeAtIndex(LLVMGlobalGetValueType(ty.vtable), VTABLE_ITABLE_FIELD as u32))
        } else {
            ty.itable
        }
    ]
        .iter().cloned()
        .chain(vslots)
        .collect_vec();

    let vtable_value = LLVMConstStruct(vtable_fields.as_mut_ptr(), vtable_fields.len() as u32, 0);
    LLVMSetInitializer(ty.vtable, vtable_value);
}

pub(super) fn emit_vtables(module: &mut MochaModule, liveness: &LivenessInfo) {
    for class_id in liveness.needs_class_object.iter().cloned() {
        unsafe {
            emit_itable(module, class_id);
            emit_vtable(module, class_id, liveness);
        };
    };
}

unsafe fn value_to_llvm(module: &MochaModule, val: Value, class_id: ClassId) -> LLVMValueRef {
    match class_id {
        ClassId::PRIMITIVE_BYTE => module.const_byte(val.as_int().unwrap() as i8).ptr(),
        ClassId::PRIMITIVE_CHAR => module.const_short(val.as_int().unwrap() as i16).ptr(),
        ClassId::PRIMITIVE_DOUBLE => module.const_double(val.as_double().unwrap()).ptr(),
        ClassId::PRIMITIVE_FLOAT => module.const_float(val.as_float().unwrap()).ptr(),
        ClassId::PRIMITIVE_INT => module.const_int(val.as_int().unwrap()).ptr(),
        ClassId::PRIMITIVE_LONG => module.const_long(val.as_long().unwrap()).ptr(),
        ClassId::PRIMITIVE_SHORT => module.const_short(val.as_int().unwrap() as i16).ptr(),
        ClassId::PRIMITIVE_BOOLEAN => module.const_byte(val.as_int().unwrap() as i8).ptr(),
        _ => val.as_ref().unwrap().map_or_else(
            || LLVMConstNull(module.types.class_types[&class_id].field_ty),
            |r| LLVMConstPointerCast(module.obj_map[&r.as_ptr()].ptr(), module.types.class_types[&class_id].field_ty)
        )
    }
}

unsafe fn define_static_heap_object<'a>(module: &MochaModule, name: &CString, obj: &JavaStaticRef<'a>) -> LLVMValueRef {
    let ty = if obj.class_id() == ClassId::JAVA_LANG_CLASS {
        module.types.class_types[&ClassId(
            obj.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_int().unwrap() as u32
        )].meta_ty
    } else {
        match **module.env.get(obj.class_id()) {
            ResolvedClass::Array(elem_class) => {
                let mut fields = [
                    module.types.class_types[&obj.class_id()].value_ty,
                    LLVMArrayType(
                        module.types.class_types[&elem_class].field_ty,
                        obj.read_array_length() as u32
                    )
                ];

                LLVMStructType(fields.as_mut_ptr(), fields.len() as u32, 1)
            },
            _ => module.types.class_types[&obj.class_id()].value_ty
        }
    };

    let global = LLVMAddGlobalInAddressSpace(module.module.ptr(), ty, name.as_ptr(), 1);
    LLVMSetSection(global, b".mocha_static_heap\0".as_ptr() as *const c_char);
    LLVMSetAlignment(global, 16);

    global
}

pub(super) fn define_static_heap(module: &mut MochaModule) {
    for (i, obj) in module.objs.iter().enumerate() {
        let name = CString::new(format!("obj_{}_{}", i, obj.class().name(module.env))).unwrap();

        module.obj_map.insert(
            obj.as_ptr(),
            unsafe { LLVMValue::from_raw(define_static_heap_object(&module, &name, obj)) }
        );
    };
}

unsafe fn emit_static_heap_object<'a>(module: &MochaModule, obj: &JavaStaticRef<'a>) {
    let class_type = &module.types.class_types[&obj.class_id()];
    let (ty, meta_class) = if obj.class_id() == ClassId::JAVA_LANG_CLASS {
        let meta_class = &module.types.class_types[&ClassId(obj.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_int().unwrap() as u32)];
        (meta_class.meta_ty, Some(meta_class))
    } else {
        (class_type.value_ty, None)
    };

    let elem_class = match **module.env.get(obj.class_id()) {
        ResolvedClass::Array(elem_class) => Some(elem_class),
        _ => None
    };

    let mut field_values = vec![std::ptr::null_mut(); class_type.num_fields];

    // This may seem dumb, but it works around an issue where LLVM attempts to "&-1" addresses if
    // converted to i32 directly. This doesn't work correctly, since trying to do that causes the
    // assembler to complain that the operation cannot be relocated. For some reason, converting to
    // i64, adding a constant, then converting to i32 works correctly.
    field_values[0] = LLVMConstIntCast(
        LLVMConstAdd(
            LLVMConstPointerCast(class_type.vtable, module.types.long),
            module.const_long(8).ptr()
        ),
        module.types.int,
        0
    );
    field_values[1] = module.const_int(obj.flags().bits() as i32).ptr();

    let array_data = if let Some(elem_class) = elem_class {
        field_values[class_type.fields[&FieldId(obj.class_id(), 0)] as usize] = module.const_int(obj.read_array_length()).ptr();
        field_values[class_type.fields[&FieldId(obj.class_id(), 1)] as usize] = LLVMConstArray(module.types.class_types[&elem_class].field_ty, std::ptr::null_mut(), 0);

        let mut elements = (0..obj.read_array_length())
            .map(|i| value_to_llvm(module, obj.read_array_element(i), elem_class))
            .collect_vec();

        Some(LLVMConstArray(
            module.types.class_types[&elem_class].field_ty,
            elements.as_mut_ptr(),
            elements.len() as u32
        ))
    } else {
        for (&f, &i) in class_type.fields.iter() {
            let field_ty = module.env.get_field(f).1.class_id;

            field_values[i] = value_to_llvm(module, obj.read_field(f), field_ty);
        };
        None
    };

    for (i, size) in class_type.pads.iter().cloned() {
        let mut data = vec![module.const_byte(0).ptr(); size as usize];
        field_values[i as usize] = LLVMConstArray(module.types.byte, data.as_mut_ptr(), size);
    };

    assert!(!field_values.iter().any(|&val| val.is_null()));

    if let Some(meta_class) = meta_class {
        let vtable_value = if meta_class.vtable.is_null() {
            module.const_int(0).ptr()
        } else {
            LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(meta_class.vtable, module.types.long),
                    module.const_long(8).ptr()
                ),
                module.types.int,
                0
            )
        };
        field_values[class_type.fields[&JAVA_LANG_CLASS_VTABLE_PTR_FIELD]] = vtable_value;

        let class = LLVMConstNamedStruct(class_type.value_ty, field_values.as_mut_ptr(), field_values.len() as u32);
        field_values = vec![std::ptr::null_mut(); meta_class.num_meta_fields];

        field_values[0] = class;

        for (&f, &i) in meta_class.meta_fields.iter() {
            let field_ty = module.env.get_field(f).1.class_id;

            field_values[i] = value_to_llvm(module, obj.read_field(f), field_ty);
        };

        for (i, size) in meta_class.meta_pads.iter().cloned() {
            let mut data = vec![module.const_byte(0).ptr(); size as usize];
            field_values[i as usize] = LLVMConstArray(module.types.byte, data.as_mut_ptr(), size);
        };

        assert!(!field_values.iter().any(|&val| val.is_null()));
    };

    let value = LLVMConstNamedStruct(ty, field_values.as_mut_ptr(), field_values.len() as u32);
    let value = if let Some(array_data) = array_data {
        let mut fields = [value, array_data];
        LLVMConstStruct(fields.as_mut_ptr(), fields.len() as u32, 1)
    } else {
        value
    };

    LLVMSetInitializer(module.obj_map[&obj.as_ptr()].ptr(), value);
}

pub(super) fn emit_static_heap(module: &mut MochaModule) {
    for obj in module.objs.iter() {
        unsafe {
            emit_static_heap_object(&module, obj);
        };
    };
}

pub(super) fn create_debug_types<'a>(di_builder: &LLVMDIBuilder<'a>, env: &ClassEnvironment) -> HashMap<ClassId, LLVMMetadata<'a>> {
    let mut temp_map: HashMap<_, _> = env.class_ids().map(|class_id| (class_id, di_builder.create_temporary())).collect();
    let mut map: HashMap<_, _> = temp_map.iter().map(|(&class_id, meta)| (class_id, unsafe { meta.as_metadata() })).collect();

    let mut temp_struct_map: HashMap<_, _> = env.class_ids().map(|class_id| (class_id, di_builder.create_temporary())).collect();
    let mut struct_map: HashMap<_, _> = temp_struct_map.iter().map(|(&class_id, meta)| (class_id, unsafe { meta.as_metadata() })).collect();

    let unknown_file = di_builder.create_file("<unknown>", "");
    let length_field = di_builder.create_member_type(
        unknown_file,
        "length",
        unknown_file,
        0,
        32,
        32,
        64,
        LLVMDIFlags::LLVMDIFlagArtificial,
        map[&ClassId::PRIMITIVE_INT]
    );
    let vtable_field = di_builder.create_member_type(
        unknown_file,
        "__vtable",
        unknown_file,
        0,
        64,
        64,
        0,
        LLVMDIFlags::LLVMDIFlagArtificial,
        di_builder.create_pointer_type(di_builder.create_unspecified_type("mocha_vtable"), 64, 64, 0, None)
    );

    for class_id in env.class_ids() {
        let ty = match **env.get(class_id) {
            ResolvedClass::User(ref class) => {
                let dbg_file = if let Some(ref source_file) = class.meta.source_file {
                    di_builder.create_file(source_file, class.meta.name.rsplit_once('/').map_or("", |(dir, _)| dir))
                } else {
                    unknown_file
                };

                let mut fields = vec![];

                if class.meta.super_id == ClassId::UNRESOLVED {
                    fields.push(vtable_field);
                } else {
                    fields.push(di_builder.create_inheritance(struct_map[&class_id], struct_map[&class.meta.super_id], 0, 0, LLVMDIFlags::LLVMDIFlagZero));
                };

                for &(field_id, off) in class.layout.fields.iter() {
                    if field_id.0 == class_id {
                        let field = &class.fields[field_id.1 as usize];

                        fields.push(di_builder.create_member_type(
                            dbg_file,
                            &field.name,
                            dbg_file,
                            0,
                            0,
                            0,
                            off as u64 * 8,
                            LLVMDIFlags::LLVMDIFlagZero,
                            map[&field.class_id]
                        ));
                    };
                };

                let struct_type = di_builder.create_struct_type(
                    dbg_file,
                    &class.meta.name,
                    dbg_file,
                    0,
                    (class.layout.size as u64) * 8,
                    64,
                    LLVMDIFlags::LLVMDIFlagZero,
                    None,
                    &fields,
                    0,
                    None,
                    &class.meta.name
                );

                temp_struct_map.remove(&class_id).unwrap().replace_all_uses_with(struct_type);
                struct_map.insert(class_id, struct_type);

                di_builder.create_pointer_type(struct_type, 64, 64, 0, None)
            },
            ref class @ ResolvedClass::Array(elem_id) => {
                let name = class.name(env);
                let elem_align = layout::get_field_size_align(env, elem_id).1;
                let mut size = 12;

                if size % elem_align != 0 {
                    size += elem_align - (size % elem_align);
                };

                let struct_type = di_builder.create_struct_type(
                    unknown_file,
                    &name,
                    unknown_file,
                    0,
                    size as u64 * 8,
                    64,
                    LLVMDIFlags::LLVMDIFlagArtificial,
                    None,
                    &[
                        di_builder.create_inheritance(struct_map[&class_id], struct_map[&ClassId::JAVA_LANG_OBJECT], 0, 0, LLVMDIFlags::LLVMDIFlagZero),
                        length_field,
                        di_builder.create_member_type(
                            unknown_file,
                            "data",
                            unknown_file,
                            0,
                            0,
                            0,
                            size as u64 * 8,
                            LLVMDIFlags::LLVMDIFlagZero,
                            di_builder.create_array_type(0, 0, map[&elem_id], &[])
                        )
                    ],
                    0,
                    None,
                    &name
                );

                temp_struct_map.remove(&class_id).unwrap().replace_all_uses_with(struct_type);
                struct_map.insert(class_id, struct_type);

                di_builder.create_pointer_type(struct_type, 64, 64, 0, None)
            },
            ResolvedClass::Primitive(None) => di_builder.create_unspecified_type("void"),
            ResolvedClass::Primitive(Some(PrimitiveType::Byte)) => di_builder.create_basic_type("byte", 8, 0x05, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Boolean)) => di_builder.create_basic_type("boolean", 8, 0x02, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Short)) => di_builder.create_basic_type("short", 16, 0x05, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Char)) => di_builder.create_basic_type("char", 16, 0x08, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Int)) => di_builder.create_basic_type("int", 32, 0x05, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Long)) => di_builder.create_basic_type("long", 64, 0x05, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Float)) => di_builder.create_basic_type("float", 32, 0x04, LLVMDIFlags::LLVMDIFlagZero),
            ResolvedClass::Primitive(Some(PrimitiveType::Double)) => di_builder.create_basic_type("double", 64, 0x04, LLVMDIFlags::LLVMDIFlagZero)
        };

        temp_map.remove(&class_id).unwrap().replace_all_uses_with(ty);
        map.insert(class_id, ty);
    };

    map
}
