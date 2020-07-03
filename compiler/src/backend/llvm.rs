use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::ffi::CString;
use std::marker::PhantomData;
use std::os::raw::c_char;
use std::ptr::NonNull;

use itertools::Itertools;
use llvm_sys::LLVMIntPredicate;
use llvm_sys::analysis::*;
use llvm_sys::bit_writer::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::classfile::PrimitiveType;
use crate::layout;
use crate::liveness::LivenessInfo;
use crate::mil::flow_graph::FlowGraph;
use crate::mil::il::*;
use crate::resolve::{ClassEnvironment, ClassId, FieldId, MethodId, ResolvedClass};
use crate::static_heap::*;
use crate::static_interp::Value;

#[derive(Debug)]
pub struct LLVMContext(LLVMContextRef);

impl LLVMContext {
    pub fn new() -> LLVMContext {
        LLVMContext(unsafe {
            LLVMContextCreate()
        })
    }

    pub unsafe fn from_raw(ptr: LLVMContextRef) -> LLVMContext {
        LLVMContext(ptr)
    }

    pub fn ptr(&self) -> LLVMContextRef {
        self.0
    }

    pub fn create_module(&self, name: &str) -> LLVMModule {
        let name = CString::new(name).unwrap();
        unsafe {
            LLVMModule::from_raw(
                LLVMModuleCreateWithNameInContext(name.as_ptr(), self.ptr()),
                name
            )
        }
    }

    pub fn create_builder(&self) -> LLVMBuilder {
        unsafe {
            LLVMBuilder::from_raw(LLVMCreateBuilderInContext(self.ptr()))
        }
    }
}

impl Drop for LLVMContext {
    fn drop(&mut self) {
        unsafe {
            LLVMContextDispose(self.ptr());
        };
    }
}

#[derive(Debug)]
pub struct LLVMModule<'a>(PhantomData<&'a LLVMContext>, LLVMModuleRef, CString);

impl <'a> LLVMModule<'a> {
    pub unsafe fn from_raw(ptr: LLVMModuleRef, name: CString) -> LLVMModule<'a> {
        LLVMModule(PhantomData, ptr, name)
    }

    pub fn ptr(&self) -> LLVMModuleRef {
        self.1
    }

    pub fn write_bitcode_to_file(&self, file: &str) -> Result<(), i32> {
        unsafe {
            let file = CString::new(file).unwrap();
            match LLVMWriteBitcodeToFile(self.ptr(), file.as_ptr()) {
                0 => Result::Ok(()),
                err => Result::Err(err)
            }
        }
    }
}

impl <'a> Drop for LLVMModule<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeModule(self.ptr());
        };
    }
}

#[derive(Debug)]
pub struct LLVMBuilder<'a>(PhantomData<&'a LLVMContext>, LLVMBuilderRef);

impl <'a> LLVMBuilder<'a> {
    pub unsafe fn from_raw(ptr: LLVMBuilderRef) -> LLVMBuilder<'a> {
        LLVMBuilder(PhantomData, ptr)
    }

    pub fn ptr(&self) -> LLVMBuilderRef {
        self.1
    }
}

impl <'a> Drop for LLVMBuilder<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.ptr());
        };
    }
}

const VTABLE_OBJ_SIZE_FIELD: usize = 0;
const VTABLE_DEPTH_FIELD: usize = 1;
const VTABLE_FLAGS_FIELD: usize = 2;
const VTABLE_MODIFIERS_FIELD: usize = 3;
const VTABLE_NUM_INTERFACES_FIELD: usize = 4;
const VTABLE_ARRAY_VTABLE_FIELD: usize = 5;
const VTABLE_TYPE_SPECIFIC_INFO_FIELD: usize = 6;
const VTABLE_SUPER_VTABLES_FIELD: usize = 7;
const VTABLE_CLASS_OBJ_FIELD: usize = 8;
const VTABLE_ITABLE_FIELD: usize = 9;
const VTABLE_FIRST_VSLOT_FIELD: usize = 10;

#[derive(Debug)]
struct LLVMClassType {
    name: CString,
    value_ty: LLVMTypeRef,
    field_ty: LLVMTypeRef,
    param_ty: LLVMTypeRef,
    num_fields: usize,
    fields: HashMap<FieldId, usize>,
    pads: Vec<(u32, u32)>,
    meta_ty: LLVMTypeRef,
    num_meta_fields: usize,
    meta_fields: HashMap<FieldId, usize>,
    meta_pads: Vec<(u32, u32)>,
    vtable: LLVMValueRef,
    itable: LLVMValueRef
}

#[derive(Debug)]
struct LLVMTypes {
    void: LLVMTypeRef,
    byte: LLVMTypeRef,
    short: LLVMTypeRef,
    int: LLVMTypeRef,
    long: LLVMTypeRef,
    float: LLVMTypeRef,
    double: LLVMTypeRef,
    bool: LLVMTypeRef,
    any_function_pointer: LLVMTypeRef,
    any_raw_pointer: LLVMTypeRef,
    any_object_pointer: LLVMTypeRef,
    itable_entry: LLVMTypeRef,
    class_types: HashMap<ClassId, LLVMClassType>,
    method_types: HashMap<MethodId, LLVMTypeRef>
}

struct MochaModule<'a, 'b, 'c> {
    env: &'a ClassEnvironment,
    ctx: &'a LLVMContext,
    module: &'a LLVMModule<'b>,
    types: LLVMTypes,
    builtins: LLVMMochaBuiltins,
    objs: Vec<JavaStaticRef<'c>>,
    known_objs: &'a MilKnownObjectMap<'c>,
    obj_map: HashMap<NonNull<u8>, LLVMValueRef>,
    methods: HashMap<MethodId, LLVMValueRef>
}

impl <'a, 'b, 'c> MochaModule<'a, 'b, 'c> {
    fn find_known_object(&self, id: MilKnownObjectId) -> LLVMValueRef {
        self.obj_map[&self.known_objs.get(id).as_ptr()]
    }

    fn find_class_object(&self, class_id: ClassId) -> LLVMValueRef {
        self.find_known_object(self.known_objs.refs.classes[&class_id])
    }

    fn const_obj_null(&self) -> LLVMValueRef {
        unsafe { LLVMConstNull(self.types.any_object_pointer) }
    }

    fn const_bool(&self, val: bool) -> LLVMValueRef {
        unsafe { LLVMConstInt(self.types.bool, if val { 1 } else { 0 }, 0) }
    }

    fn const_byte(&self, val: i8) -> LLVMValueRef {
        unsafe { LLVMConstInt(self.types.byte, (val as u8).into(), 0) }
    }

    fn const_short(&self, val: i16) -> LLVMValueRef {
        unsafe { LLVMConstInt(self.types.short, (val as u16).into(), 0) }
    }

    fn const_int(&self, val: i32) -> LLVMValueRef {
        unsafe { LLVMConstInt(self.types.int, (val as u32).into(), 0) }
    }

    fn const_long(&self, val: i64) -> LLVMValueRef {
        unsafe { LLVMConstInt(self.types.long, (val as u64).into(), 0) }
    }

    fn const_float(&self, bits: u32) -> LLVMValueRef {
        unsafe { LLVMConstBitCast(LLVMConstInt(self.types.int, bits.into(), 0), self.types.float) }
    }

    fn const_double(&self, bits: u64) -> LLVMValueRef {
        unsafe { LLVMConstBitCast(LLVMConstInt(self.types.long, bits.into(), 0), self.types.double) }
    }
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
                let mut current_size = 12;

                field_map.insert(FieldId(class_id, 0), 2);

                if current_size % align != 0 {
                    let pad = align - (current_size % align);
                    pads.push((fields.len() as u32, pad));
                    fields.push(LLVMArrayType(types.byte, pad));
                    current_size += pad;
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

    let vslot_class = if liveness.may_construct.contains(&class_id) {
        match **env.get(class_id) {
            ResolvedClass::User(ref class) => Some(class),
            ResolvedClass::Array(_) => Some(env.get(ClassId::JAVA_LANG_OBJECT).as_user_class()),
            _ => None
        }
    } else {
        None
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

fn create_types(env: &ClassEnvironment, liveness: &LivenessInfo, ctx: &LLVMContext, module: &LLVMModule) -> LLVMTypes {
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

struct LLVMMochaBuiltins {
    alloc_obj: LLVMValueRef,
    alloc_array: LLVMValueRef,
    throw: LLVMValueRef
}

unsafe fn declare_builtins(module: &LLVMModule, types: &LLVMTypes) -> LLVMMochaBuiltins {
    LLVMMochaBuiltins {
        alloc_obj: LLVMAddFunction(
            module.ptr(),
            b"mocha_alloc_obj\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_object_pointer,
                [types.any_raw_pointer].as_mut_ptr(),
                1,
                0
            )
        ),
        alloc_array: LLVMAddFunction(
            module.ptr(),
            b"mocha_alloc_array\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_object_pointer,
                [types.any_raw_pointer, types.int].as_mut_ptr(),
                2,
                0
            )
        ),
        throw: LLVMAddFunction(
            module.ptr(),
            b"mocha_throw\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_object_pointer,
                [types.any_object_pointer].as_mut_ptr(),
                1,
                0
            )
        )
    }
}

unsafe fn define_function(module: &MochaModule, func: &MilFunction) -> LLVMValueRef {
    let (class, method) = module.env.get_method(func.id);
    let name = CString::new(format!("{}.{}{}", class.meta.name, method.name, method.descriptor)).unwrap();

    LLVMAddFunction(module.module.ptr(), name.as_ptr(), module.types.method_types[&func.id])
}

unsafe fn create_value_ref(module: &MochaModule, op: &MilOperand, regs: &HashMap<MilRegister, LLVMValueRef>) -> LLVMValueRef {
    match *op {
        MilOperand::Register(r) => regs[&r],
        MilOperand::Null => module.const_obj_null(),
        MilOperand::KnownObject(object_id, _) => LLVMConstPointerCast(
            module.find_known_object(object_id),
            module.types.any_object_pointer
        ),
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

unsafe fn create_meta_field_gep(builder: &LLVMBuilder, field_id: FieldId, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>, types: &LLVMTypes) -> LLVMValueRef {
    let class_ty = &types.class_types[&field_id.0];
    let class_obj = obj_map[&known_objects.get(known_objects.refs.classes[&field_id.0]).as_ptr()];

    LLVMBuildStructGEP(
        builder.ptr(),
        class_obj,
        class_ty.meta_fields[&field_id] as u32,
        b"\0".as_ptr() as *const c_char
    )
}

unsafe fn create_instance_field_gep(builder: &LLVMBuilder, field_id: FieldId, obj: LLVMValueRef, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>, types: &LLVMTypes) -> LLVMValueRef {
    let class_ty = &types.class_types[&field_id.0];

    LLVMBuildStructGEP(
        builder.ptr(),
        LLVMBuildPointerCast(
            builder.ptr(),
            obj,
            types.class_types[&field_id.0].field_ty,
            b"\0".as_ptr() as *const c_char
        ),
        class_ty.fields[&field_id] as u32,
        b"\0".as_ptr() as *const c_char
    )
}

unsafe fn create_vtable_decompress(builder: &LLVMBuilder, compressed: LLVMValueRef, class_id: ClassId, types: &LLVMTypes) -> LLVMValueRef {
    LLVMBuildIntToPtr(
        builder.ptr(),
        LLVMBuildNUWSub(
            builder.ptr(),
            compressed,
            LLVMConstInt(types.int, 8, 0),
            b"\0".as_ptr() as *const c_char
        ),
        LLVMPointerType(LLVMGlobalGetValueType(types.class_types[&class_id].vtable), 0),
        b"\0".as_ptr() as *const c_char
    )
}

unsafe fn create_itable_decompress(builder: &LLVMBuilder, compressed: LLVMValueRef, types: &LLVMTypes) -> LLVMValueRef {
    LLVMBuildIntToPtr(
        builder.ptr(),
        LLVMBuildNUWSub(
            builder.ptr(),
            compressed,
            LLVMConstInt(types.int, 8, 0),
            b"\0".as_ptr() as *const c_char
        ),
        LLVMPointerType(LLVMArrayType(types.any_function_pointer, 0), 0),
        b"\0".as_ptr() as *const c_char
    )
}

unsafe fn create_vtable_load(builder: &LLVMBuilder, obj: LLVMValueRef, class_id: ClassId, types: &LLVMTypes) -> LLVMValueRef {
    let obj = LLVMBuildPointerCast(
        builder.ptr(),
        obj,
        types.class_types[&ClassId::JAVA_LANG_OBJECT].field_ty,
        "\0".as_ptr() as *const c_char
    );

    create_vtable_decompress(
        builder,
        LLVMBuildLoad(
            builder.ptr(),
            LLVMBuildStructGEP(builder.ptr(), obj, 0, b"\0".as_ptr() as *const c_char),
            "\0".as_ptr() as *const c_char
        ),
        class_id,
        types
    )
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

unsafe fn set_register(builder: &LLVMBuilder, func: &MilFunction, local_regs: &mut HashMap<MilRegister, LLVMValueRef>, all_regs: &mut HashMap<MilRegister, LLVMValueRef>, reg: MilRegister, val: LLVMValueRef, types: &LLVMTypes) {
    if reg == MilRegister::VOID {
        return;
    };

    local_regs.insert(reg, val);

    let val = LLVMBuildBitCast(builder.ptr(), val, native_arg_type(func.reg_map.get_reg_info(reg).ty, types), b"\0".as_ptr() as *const c_char);
    assert!(all_regs.insert(reg, val).is_none());
}

unsafe fn coerce_before_store(builder: &LLVMBuilder, class_id: ClassId, val: LLVMValueRef, types: &LLVMTypes) -> LLVMValueRef {
    match class_id {
        ClassId::PRIMITIVE_BYTE | ClassId::PRIMITIVE_SHORT | ClassId::PRIMITIVE_BOOLEAN | ClassId::PRIMITIVE_CHAR => {
            LLVMBuildIntCast(builder.ptr(), val, types.class_types[&class_id].field_ty, "\0".as_ptr() as *const c_char)
        },
        ClassId::PRIMITIVE_INT | ClassId::PRIMITIVE_LONG | ClassId::PRIMITIVE_FLOAT | ClassId::PRIMITIVE_DOUBLE => val,
        _ => {
            LLVMBuildPointerCast(builder.ptr(), val, types.class_types[&class_id].field_ty, "\0".as_ptr() as *const c_char)
        }
    }
}

unsafe fn coerce_after_load(builder: &LLVMBuilder, class_id: ClassId, val: LLVMValueRef, types: &LLVMTypes) -> LLVMValueRef {
    match class_id {
        ClassId::PRIMITIVE_BYTE | ClassId::PRIMITIVE_SHORT | ClassId::PRIMITIVE_BOOLEAN => {
            LLVMBuildIntCast2(builder.ptr(), val, types.int, 1, "\0".as_ptr() as *const c_char)
        },
        ClassId::PRIMITIVE_CHAR => {
            LLVMBuildIntCast2(builder.ptr(), val, types.int, 0, "\0".as_ptr() as *const c_char)
        },
        ClassId::PRIMITIVE_INT | ClassId::PRIMITIVE_LONG | ClassId::PRIMITIVE_FLOAT | ClassId::PRIMITIVE_DOUBLE => val,
        _ => {
            LLVMBuildPointerCast(builder.ptr(), val, types.any_object_pointer, "\0".as_ptr() as *const c_char)
        }
    }
}

unsafe fn undefined_register_value(module: &MochaModule, ty: MilType) -> LLVMValueRef {
    match ty {
        MilType::Void => std::ptr::null_mut(),
        MilType::Ref => module.const_obj_null(),
        MilType::Bool => module.const_bool(false),
        MilType::Int => module.const_int(0),
        MilType::Long => module.const_long(0),
        MilType::Float => module.const_float(0),
        MilType::Double => module.const_double(0)
    }
}

unsafe fn emit_basic_block(
    module: &MochaModule,
    func: &MilFunction,
    cfg: &FlowGraph<MilBlockId>,
    block_id: MilBlockId,
    builder: &LLVMBuilder,
    llvm_func: LLVMValueRef,
    locals: &mut HashMap<MilLocalId, LLVMValueRef>,
    llvm_blocks: &mut HashMap<MilBlockId, (LLVMBasicBlockRef, LLVMBasicBlockRef, LLVMValueRef)>,
    all_regs: &mut HashMap<MilRegister, LLVMValueRef>,
    phis_to_add: &mut Vec<(LLVMValueRef, MilBlockId, MilOperand)>
) {
    let mut local_regs = HashMap::new();

    let block = &func.blocks[&block_id];
    let block_name = CString::new(format!("{}", block.id)).unwrap();
    let mut llvm_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func, block_name.as_ptr());
    let llvm_start_block = llvm_block;
    let mut cond_out = std::ptr::null_mut();

    LLVMPositionBuilderAtEnd(builder.ptr(), llvm_block);

    if !cfg.get(block_id).incoming.is_empty() {
        for reg in find_used_before_def(block) {
            if let Some(&val) = all_regs.get(&reg) {
                local_regs.insert(reg, val);
            } else {
                let phi = LLVMBuildPhi(
                    builder.ptr(),
                    native_arg_type(func.reg_map.get_reg_info(reg).ty, &module.types),
                    register_name(reg).as_ptr()
                );

                local_regs.insert(reg, phi);

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
            let llvm_phi = LLVMBuildPhi(
                builder.ptr(),
                native_arg_type(func.reg_map.get_reg_info(phi.target).ty, &module.types),
                register_name(phi.target).as_ptr()
            );

            set_register(&builder, func, &mut local_regs, all_regs, phi.target, llvm_phi, &module.types);

            for (src, pred) in phi.sources.iter().cloned() {
                phis_to_add.push((llvm_phi, pred, src));
            };
        } else {
            let val = undefined_register_value(module, func.reg_map.get_reg_info(phi.target).ty);
            set_register(&builder, func, &mut local_regs, all_regs, phi.target, val, &module.types);
        };
    };

    for instr in block.instrs.iter() {
        match instr.kind {
            MilInstructionKind::Nop => {},
            MilInstructionKind::Copy(tgt, ref src) => {
                let src = create_value_ref(&module, src, &local_regs);
                set_register(&builder, func, &mut local_regs, all_regs, tgt, src, &module.types);
            },
            MilInstructionKind::UnOp(op, tgt, ref val) => {
                let val = create_value_ref(&module, val, &local_regs);
                set_register(&builder, func, &mut local_regs, all_regs, tgt, match op {
                    MilUnOp::INeg => LLVMBuildNeg(
                        builder.ptr(),
                        val,
                        register_name(tgt).as_ptr()
                    ),
                    MilUnOp::IExtB => LLVMBuildSExt(
                        builder.ptr(),
                        LLVMBuildTrunc(
                            builder.ptr(),
                            val,
                            module.types.byte,
                            b"\0".as_ptr() as *const c_char
                        ),
                        module.types.int,
                        register_name(tgt).as_ptr()
                    ),
                    MilUnOp::IExtS => LLVMBuildSExt(
                        builder.ptr(),
                        LLVMBuildTrunc(
                            builder.ptr(),
                            val,
                            module.types.short,
                            b"\0".as_ptr() as *const c_char
                        ),
                        module.types.int,
                        register_name(tgt).as_ptr()
                    ),
                    MilUnOp::LNeg => LLVMBuildNeg(
                        builder.ptr(),
                        val,
                        register_name(tgt).as_ptr()
                    ),
                    MilUnOp::I2L => LLVMBuildSExt(
                        builder.ptr(),
                        val,
                        module.types.long,
                        register_name(tgt).as_ptr()
                    ),
                    MilUnOp::L2I => LLVMBuildTrunc(
                        builder.ptr(),
                        val,
                        module.types.int,
                        register_name(tgt).as_ptr()
                    )
                }, &module.types);
            },
            MilInstructionKind::BinOp(op, tgt, ref lhs, ref rhs) => {
                let lhs = create_value_ref(&module, lhs, &local_regs);
                let rhs = create_value_ref(&module, rhs, &local_regs);
                set_register(&builder, func, &mut local_regs, all_regs, tgt, match op {
                    MilBinOp::IAdd => LLVMBuildAdd(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::ISub => LLVMBuildSub(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IMul => LLVMBuildMul(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IDivS => LLVMBuildSDiv(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IAnd => LLVMBuildAnd(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IOr => LLVMBuildOr(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IXor => LLVMBuildXor(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IShrS => LLVMBuildAShr(
                        builder.ptr(),
                        lhs,
                        LLVMBuildAnd(
                            builder.ptr(),
                            rhs,
                            module.const_int(0x1f),
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IShrU => LLVMBuildLShr(
                        builder.ptr(),
                        lhs,
                        LLVMBuildAnd(
                            builder.ptr(),
                            rhs,
                            module.const_int(0x1f),
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::IShl => LLVMBuildShl(
                        builder.ptr(),
                        lhs,
                        LLVMBuildAnd(
                            builder.ptr(),
                            rhs,
                            module.const_int(0x1f),
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LAdd => LLVMBuildAdd(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LSub => LLVMBuildSub(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LMul => LLVMBuildMul(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LDivS => LLVMBuildSDiv(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LRemS => LLVMBuildSRem(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LAnd => LLVMBuildAnd(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LOr => LLVMBuildOr(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LXor => LLVMBuildXor(
                        builder.ptr(),
                        lhs,
                        rhs,
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LShrS => LLVMBuildAShr(
                        builder.ptr(),
                        lhs,
                        LLVMBuildZExt(
                            builder.ptr(),
                            LLVMBuildAnd(
                                builder.ptr(),
                                rhs,
                                module.const_int(0x3f),
                                b"\0".as_ptr() as *const c_char
                            ),
                            module.types.long,
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LShrU => LLVMBuildLShr(
                        builder.ptr(),
                        lhs,
                        LLVMBuildZExt(
                            builder.ptr(),
                            LLVMBuildAnd(
                                builder.ptr(),
                                rhs,
                                module.const_int(0x3f),
                                b"\0".as_ptr() as *const c_char
                            ),
                            module.types.long,
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LShl => LLVMBuildShl(
                        builder.ptr(),
                        lhs,
                        LLVMBuildZExt(
                            builder.ptr(),
                            LLVMBuildAnd(
                                builder.ptr(),
                                rhs,
                                module.const_int(0x3f),
                                b"\0".as_ptr() as *const c_char
                            ),
                            module.types.long,
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    MilBinOp::LCmp => LLVMBuildSelect(
                        builder.ptr(),
                        LLVMBuildICmp(
                            builder.ptr(),
                            LLVMIntPredicate::LLVMIntEQ,
                            lhs,
                            rhs,
                            b"\0".as_ptr() as *const c_char
                        ),
                        module.const_int(0),
                        LLVMBuildSelect(
                            builder.ptr(),
                            LLVMBuildICmp(
                                builder.ptr(),
                                LLVMIntPredicate::LLVMIntSGT,
                                lhs,
                                rhs,
                                b"\0".as_ptr() as *const c_char
                            ),
                            module.const_int(1),
                            module.const_int(-1),
                            b"\0".as_ptr() as *const c_char
                        ),
                        register_name(tgt).as_ptr()
                    ),
                    )
                }, &module.types);
            },
            MilInstructionKind::GetParam(idx, _, tgt) => {
                set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMGetParam(llvm_func, idx as u32), &module.types);
            },
            MilInstructionKind::GetLocal(local_id, tgt) => {
                set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildLoad(
                    builder.ptr(),
                    locals[&local_id],
                    register_name(tgt).as_ptr() as *const c_char
                ), &module.types);
            },
            MilInstructionKind::SetLocal(local_id, ref src) => {
                let src = create_value_ref(&module, src, &local_regs);
                let local_ptr = locals[&local_id];

                LLVMBuildStore(
                    builder.ptr(),
                    src,
                    local_ptr
                );
            },
            MilInstructionKind::GetField(field_id, class_id, tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);

                let val = coerce_after_load(builder, class_id, LLVMBuildLoad(
                    builder.ptr(),
                    create_instance_field_gep(&builder, field_id, obj, &module.known_objs, &module.obj_map, &module.types),
                    register_name(tgt).as_ptr() as *const c_char
                ), &module.types);

                set_register(&builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutField(field_id, class_id, ref obj, ref val) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let val = create_value_ref(&module, val, &local_regs);

                LLVMBuildStore(
                    builder.ptr(),
                    coerce_before_store(builder, class_id, val, &module.types),
                    create_instance_field_gep(&builder, field_id, obj, &module.known_objs, &module.obj_map, &module.types)
                );
            },
            MilInstructionKind::GetArrayLength(tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);

                set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildLoad(
                    builder.ptr(),
                    create_instance_field_gep(&builder, FieldId(ClassId::JAVA_LANG_OBJECT_ARRAY, 0), obj, &module.known_objs, &module.obj_map, &module.types),
                    register_name(tgt).as_ptr() as *const c_char
                ), &module.types);
            },
            MilInstructionKind::GetArrayElement(class_id, tgt, ref obj, ref idx) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let idx = create_value_ref(&module, idx, &local_regs);

                let arr_data = create_instance_field_gep(&builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.known_objs, &module.obj_map, &module.types);

                let val = coerce_after_load(builder, class_id, LLVMBuildLoad(
                    builder.ptr(),
                    LLVMBuildGEP(
                        builder.ptr(),
                        arr_data,
                        [module.const_int(0), idx].as_mut_ptr(),
                        2,
                        "\0".as_ptr() as *const c_char
                    ),
                    register_name(tgt).as_ptr() as *const c_char
                ), &module.types);

                set_register(&builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutArrayElement(class_id, ref obj, ref idx, ref val) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let idx = create_value_ref(&module, idx, &local_regs);
                let val = create_value_ref(&module, val, &local_regs);

                let arr_data = create_instance_field_gep(&builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.known_objs, &module.obj_map, &module.types);

                LLVMBuildStore(
                    builder.ptr(),
                    coerce_before_store(builder, class_id, val, &module.types),
                    LLVMBuildGEP(
                        builder.ptr(),
                        arr_data,
                        [module.const_int(0), idx].as_mut_ptr(),
                        2,
                        "\0".as_ptr() as *const c_char
                    )
                );
            },
            MilInstructionKind::GetStatic(field_id, class_id, tgt) => {
                let val = coerce_after_load(builder, class_id, LLVMBuildLoad(
                    builder.ptr(),
                    create_meta_field_gep(&builder, field_id, &module.known_objs, &module.obj_map, &module.types),
                    register_name(tgt).as_ptr() as *const c_char
                ), &module.types);

                set_register(&builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::PutStatic(field_id, class_id, ref val) => {
                let val = create_value_ref(&module, val, &local_regs);

                LLVMBuildStore(
                    builder.ptr(),
                    coerce_before_store(builder, class_id, val, &module.types),
                    create_meta_field_gep(&builder, field_id, &module.known_objs, &module.obj_map, &module.types)
                );
            },
            MilInstructionKind::AllocObj(class_id, tgt) => {
                let obj = LLVMBuildCall(
                    builder.ptr(),
                    module.builtins.alloc_obj,
                    [
                        LLVMConstPointerCast(
                            module.types.class_types[&class_id].vtable,
                            module.types.any_raw_pointer
                        )
                    ].as_mut_ptr(),
                    1,
                    register_name(tgt).as_ptr() as *const c_char
                );

                set_register(&builder, func, &mut local_regs, all_regs, tgt, obj, &module.types);
            },
            MilInstructionKind::AllocArray(class_id, tgt, ref len) => {
                let len = create_value_ref(&module, len, &local_regs);
                let obj = LLVMBuildCall(
                    builder.ptr(),
                    module.builtins.alloc_array,
                    [
                        LLVMConstPointerCast(
                            module.types.class_types[&class_id].vtable,
                            module.types.any_raw_pointer
                        ),
                        len
                    ].as_mut_ptr(),
                    2,
                    register_name(tgt).as_ptr() as *const c_char
                );

                set_register(&builder, func, &mut local_regs, all_regs, tgt, obj, &module.types);
            }
        };
    };

    match block.end_instr.kind {
        MilEndInstructionKind::Nop => {},
        MilEndInstructionKind::Unreachable => {
            LLVMBuildUnreachable(builder.ptr());
        },
        MilEndInstructionKind::Call(_, method_id, tgt, ref args) => {
            let mut args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildCall(
                builder.ptr(),
                module.methods[&method_id],
                args.as_mut_ptr(),
                args.len() as u32,
                register_name(tgt).as_ptr() as *const c_char
            ), &module.types);
        },
        MilEndInstructionKind::CallVirtual(_, method_id, tgt, ref obj, ref args) => {
            let (_, method) = module.env.get_method(method_id);
            let obj = create_value_ref(&module, obj, &local_regs);
            let mut args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let vslot = LLVMBuildLoad(
                builder.ptr(),
                LLVMBuildStructGEP(
                    builder.ptr(),
                    create_vtable_load(&builder, obj, method_id.0, &module.types),
                    VTABLE_FIRST_VSLOT_FIELD as u32 + method.virtual_slot,
                    b"\0".as_ptr() as *const c_char
                ),
                "\0".as_ptr() as *const c_char
            );

            set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildCall(
                builder.ptr(),
                LLVMBuildPointerCast(
                    builder.ptr(),
                    vslot,
                    LLVMPointerType(module.types.method_types[&method_id], 0),
                    "\0".as_ptr() as *const c_char
                ),
                args.as_mut_ptr(),
                args.len() as u32,
                register_name(tgt).as_ptr() as *const c_char
            ), &module.types);
        },
        MilEndInstructionKind::CallInterface(_, method_id, tgt, ref obj, ref args) => {
            let interface_vtable = LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(module.types.class_types[&method_id.0].vtable, module.types.long),
                    module.const_long(8)
                ),
                module.types.int,
                0
            );

            let (_, method) = module.env.get_method(method_id);
            let obj = create_value_ref(&module, obj, &local_regs);
            let mut args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let first_islot = LLVMBuildStructGEP(
                builder.ptr(),
                LLVMBuildLoad(
                    builder.ptr(),
                    LLVMBuildStructGEP(
                        builder.ptr(),
                        create_vtable_load(&builder, obj, ClassId::JAVA_LANG_OBJECT, &module.types),
                        VTABLE_ITABLE_FIELD as u32,
                        b"\0".as_ptr() as *const c_char
                    ),
                    b"\0".as_ptr() as *const c_char
                ),
                0,
                b"\0".as_ptr() as *const c_char
            );

            let loop_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func, b"\0".as_ptr() as *const c_char);
            LLVMBuildBr(builder.ptr(), loop_block);
            LLVMPositionBuilderAtEnd(builder.ptr(), loop_block);

            let islot = LLVMBuildPhi(builder.ptr(), LLVMPointerType(module.types.itable_entry, 0), b"\0".as_ptr() as *const c_char);
            let islot_interface = LLVMBuildLoad(
                builder.ptr(),
                LLVMBuildStructGEP(
                    builder.ptr(),
                    islot,
                    0,
                    b"\0".as_ptr() as *const c_char
                ),
                b"\0".as_ptr() as *const c_char
            );
            let next_islot = LLVMBuildGEP(builder.ptr(), islot, [module.const_int(1)].as_mut_ptr(), 1, b"\0".as_ptr() as *const c_char);

            LLVMAddIncoming(islot, [first_islot, next_islot].as_mut_ptr(), [llvm_block, loop_block].as_mut_ptr(), 2);

            let call_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func, b"\0".as_ptr() as *const c_char);
            LLVMBuildCondBr(
                builder.ptr(),
                LLVMBuildICmp(
                    builder.ptr(),
                    LLVMIntPredicate::LLVMIntNE,
                    islot_interface,
                    interface_vtable,
                    b"\0".as_ptr() as *const c_char
                ),
                loop_block,
                call_block
            );

            llvm_block = call_block;
            LLVMPositionBuilderAtEnd(builder.ptr(), call_block);

            let islot = LLVMBuildLoad(
                builder.ptr(),
                LLVMBuildStructGEP(
                    builder.ptr(),
                    create_itable_decompress(
                        &builder,
                        LLVMBuildLoad(
                            builder.ptr(),
                            LLVMBuildStructGEP(
                                builder.ptr(),
                                islot,
                                1,
                                b"\0".as_ptr() as *const c_char
                            ),
                            b"\0".as_ptr() as *const c_char
                        ),
                        &module.types
                    ),
                    method.virtual_slot,
                    b"\0".as_ptr() as *const c_char
                ),
                b"\0".as_ptr() as *const c_char
            );

            set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildCall(
                builder.ptr(),
                LLVMBuildPointerCast(
                    builder.ptr(),
                    islot,
                    LLVMPointerType(module.types.method_types[&method_id], 0),
                    "\0".as_ptr() as *const c_char
                ),
                args.as_mut_ptr(),
                args.len() as u32,
                register_name(tgt).as_ptr() as *const c_char
            ), &module.types);
        },
        MilEndInstructionKind::CallNative(ret_ty, ref name, tgt, ref args) => {
            let mut arg_tys = args.iter().map(|a| native_arg_type(a.get_type(&func.reg_map), &module.types)).collect_vec();
            let mut args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let func_ty = LLVMFunctionType(
                native_arg_type(MilType::for_class(ret_ty), &module.types),
                arg_tys.as_mut_ptr(),
                arg_tys.len() as u32,
                0
            );
            let name = CString::new(name.as_str()).unwrap();
            let native_func = LLVMAddFunction(module.module.ptr(), name.as_ptr(), func_ty);

            set_register(&builder, func, &mut local_regs, all_regs, tgt, LLVMBuildCall(
                builder.ptr(),
                native_func,
                args.as_mut_ptr(),
                args.len() as u32,
                register_name(tgt).as_ptr() as *const c_char
            ), &module.types);
        },
        MilEndInstructionKind::Throw(ref exception) => {
            let exception = create_value_ref(&module, exception, &local_regs);

            LLVMBuildCall(
                builder.ptr(),
                module.builtins.throw,
                [exception].as_mut_ptr(),
                1,
                "\0".as_ptr() as *const c_char
            );
            LLVMBuildUnreachable(builder.ptr());
        },
        MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)) => {
            LLVMBuildRetVoid(builder.ptr());
        },
        MilEndInstructionKind::Return(ref val) => {
            let val = LLVMBuildBitCast(
                builder.ptr(),
                create_value_ref(&module, val, &local_regs),
                LLVMGetReturnType(module.types.method_types[&func.id]),
                b"\0".as_ptr() as *const c_char
            );

            LLVMBuildRet(builder.ptr(), val);
        },
        MilEndInstructionKind::Jump(_) => {},
        MilEndInstructionKind::JumpIf(cond, _, ref lhs, ref rhs) => {
            let lhs = create_value_ref(&module, lhs, &local_regs);
            let rhs = create_value_ref(&module, rhs, &local_regs);

            let cond = match cond {
                MilComparison::Eq => LLVMIntPredicate::LLVMIntEQ,
                MilComparison::Ne => LLVMIntPredicate::LLVMIntNE,
                MilComparison::Gt => LLVMIntPredicate::LLVMIntSGT,
                MilComparison::Lt => LLVMIntPredicate::LLVMIntSLT,
                MilComparison::Ge => LLVMIntPredicate::LLVMIntSGE,
                MilComparison::Le => LLVMIntPredicate::LLVMIntSLE
            };

            cond_out = LLVMBuildICmp(builder.ptr(), cond, lhs, rhs, b"\0".as_ptr() as *const c_char);
        }
    };

    llvm_blocks.insert(block_id, (llvm_start_block, llvm_block, cond_out));
}

unsafe fn emit_function(module: &MochaModule, func: &MilFunction) {
    let cfg = FlowGraph::for_function(func);

    let llvm_func = module.methods[&func.id];
    let builder = module.ctx.create_builder();

    let mut llvm_blocks = HashMap::new();

    let mut locals = HashMap::new();
    let start_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), llvm_func, b"start\0".as_ptr() as *const c_char);
    LLVMPositionBuilderAtEnd(builder.ptr(), start_block);

    for (&local_id, local) in func.reg_map.local_info.iter() {
        locals.insert(
            local_id,
            LLVMBuildAlloca(
                builder.ptr(),
                native_arg_type(local.ty, &module.types),
                local_name(local_id).as_ptr() as *const c_char
            )
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
        LLVMAddIncoming(
            phi,
            &mut create_value_ref(module, &val, &all_regs) as *mut LLVMValueRef,
            &llvm_blocks[&pred].1 as *const LLVMBasicBlockRef as *mut LLVMBasicBlockRef,
            1
        );
    };

    LLVMPositionBuilderAtEnd(builder.ptr(), start_block);
    LLVMBuildBr(builder.ptr(), llvm_blocks[&func.block_order[0]].0);

    for (block_id, next_block_id) in func.block_order.iter().cloned().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = &func.blocks[&block_id];
        let (_, llvm_block, cond) = llvm_blocks[&block_id];

        LLVMPositionBuilderAtEnd(builder.ptr(), llvm_block);

        match block.end_instr.kind {
            MilEndInstructionKind::Unreachable => {},
            MilEndInstructionKind::Jump(tgt) => {
                LLVMBuildBr(builder.ptr(), llvm_blocks[&tgt].0);
            },
            MilEndInstructionKind::JumpIf(_, tgt, _, _) => {
                LLVMBuildCondBr(builder.ptr(), cond, llvm_blocks[&tgt].0, llvm_blocks[&next_block_id].0);
            },
            MilEndInstructionKind::Throw(_) => {},
            MilEndInstructionKind::Return(_) => {},
            _ => {
                LLVMBuildBr(builder.ptr(), llvm_blocks[&next_block_id].0);
            }
        };
    };
}

unsafe fn emit_main_function(module: &MochaModule, main_method: MethodId) {
    let main_func = LLVMAddFunction(
        module.module.ptr(),
        b"main\0".as_ptr() as *const c_char,
        LLVMFunctionType(
            module.types.int,
            [module.types.int, module.types.any_raw_pointer].as_mut_ptr(),
            2,
            0
        )
    );
    let builder = module.ctx.create_builder();

    let main_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), main_func, b"main\0".as_ptr() as *const c_char);
    LLVMPositionBuilderAtEnd(builder.ptr(), main_block);

    LLVMBuildCall(
        builder.ptr(),
        module.methods[&main_method],
        [module.const_obj_null()].as_mut_ptr(),
        1,
        b"\0".as_ptr() as *const c_char
    );
    LLVMBuildRet(builder.ptr(), module.const_int(0));
}

unsafe fn emit_itable(module: &MochaModule, class_id: ClassId, liveness: &LivenessInfo) {
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
                        |f| LLVMConstBitCast(f, module.types.any_function_pointer)
                    )
                }).collect_vec();

                LLVMSetInitializer(itable_entry, LLVMConstArray(module.types.any_function_pointer, islots.as_mut_ptr(), islots.len() as u32));
                LLVMConstIntCast(
                    LLVMConstAdd(
                        LLVMConstPointerCast(itable_entry, module.types.long),
                        module.const_long(8)
                    ),
                    module.types.int,
                    0
                )
            } else {
                module.const_int(16)
            };

            LLVMConstStruct(
                [
                    LLVMConstIntCast(
                        LLVMConstAdd(
                            LLVMConstPointerCast(module.types.class_types[&interface_id].vtable, module.types.long),
                            module.const_long(8)
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
    let class_obj = module.find_class_object(class_id);
    let vslot_class = match **module.env.get(class_id) {
        ResolvedClass::User(ref class) => Some(class),
        ResolvedClass::Array(_) => Some(module.env.get(ClassId::JAVA_LANG_OBJECT).as_user_class()),
        _ => None
    };

    let vslots = if let Some(vslot_class) = vslot_class {
        vslot_class.layout.virtual_slots.iter().cloned().map(|method_id| {
            module.methods.get(&method_id).cloned().map_or_else(
                || LLVMConstNull(module.types.any_function_pointer),
                |f| LLVMConstBitCast(f, module.types.any_function_pointer)
            )
        }).collect_vec()
    } else {
        vec![]
    };

    let (flags, size, type_specific_info) = match **module.env.get(class_id) {
        ResolvedClass::User(ref class) => (0x0000, class.layout.size, module.const_long(0)),
        ResolvedClass::Array(elem_id) => (
            0x0002,
            layout::get_array_header_size(
                layout::get_field_size_align(module.env, elem_id).1
            ).try_into().unwrap(),
            LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(module.types.class_types[&elem_id].vtable, module.types.long),
                    module.const_long(8)
                ),
                module.types.long,
                0
            )
        ),
        ResolvedClass::Primitive(None) => (0x0001, 0, module.const_long(b'V'.into())),
        ResolvedClass::Primitive(Some(ty)) => (0x0001, 0, module.const_long(ty.as_char().into()))
    };

    let ty = &module.types.class_types[&class_id];

    let mut vtable_fields = [
        module.const_int(size as i32),
        module.const_short(0),
        module.const_short(flags),
        module.const_short(0),
        module.const_short(0),
        if let Some(array_class_id) = module.env.try_find_array(class_id) {
            if liveness.may_construct.contains(&array_class_id) {
                LLVMConstIntCast(
                    LLVMConstAdd(
                        LLVMConstPointerCast(module.types.class_types[&array_class_id].vtable, module.types.long),
                        module.const_long(8)
                    ),
                    module.types.int,
                    0
                )
            } else {
                module.const_int(0)
            }
        } else {
            module.const_int(0)
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

unsafe fn value_to_llvm(module: &MochaModule, val: Value, class_id: ClassId) -> LLVMValueRef {
    match class_id {
        ClassId::PRIMITIVE_BYTE => module.const_byte(val.as_int().unwrap() as i8),
        ClassId::PRIMITIVE_CHAR => module.const_short(val.as_int().unwrap() as i16),
        ClassId::PRIMITIVE_DOUBLE => module.const_double(val.as_double().unwrap()),
        ClassId::PRIMITIVE_FLOAT => module.const_float(val.as_float().unwrap()),
        ClassId::PRIMITIVE_INT => module.const_int(val.as_int().unwrap()),
        ClassId::PRIMITIVE_LONG => module.const_long(val.as_long().unwrap()),
        ClassId::PRIMITIVE_SHORT => module.const_short(val.as_int().unwrap() as i16),
        ClassId::PRIMITIVE_BOOLEAN => module.const_byte(val.as_int().unwrap() as i8),
        _ => val.as_ref().unwrap().map_or_else(
            || LLVMConstNull(module.types.class_types[&class_id].field_ty),
            |r| LLVMConstPointerCast(module.obj_map[&r.as_ptr()], module.types.class_types[&class_id].field_ty)
        )
    }
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
            module.const_long(8)
        ),
        module.types.int,
        0
    );
    field_values[1] = module.const_int(obj.flags().bits() as i32);

    let array_data = if let Some(elem_class) = elem_class {
        field_values[class_type.fields[&FieldId(obj.class_id(), 0)] as usize] = module.const_int(obj.read_array_length());
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
        let mut data = vec![module.const_byte(0); size as usize];
        field_values[i as usize] = LLVMConstArray(module.types.byte, data.as_mut_ptr(), size);
    };

    assert!(!field_values.iter().any(|&val| val.is_null()));

    if let Some(meta_class) = meta_class {
        let vtable_value = if meta_class.vtable.is_null() {
            module.const_int(0)
        } else {
            LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(meta_class.vtable, module.types.long),
                    module.const_long(8)
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
            let mut data = vec![module.const_byte(0); size as usize];
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

    LLVMSetInitializer(module.obj_map[&obj.as_ptr()], value);
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

pub fn emit_llvm_ir<'a>(env: &ClassEnvironment, program: &MilProgram, liveness: &LivenessInfo, heap: &JavaStaticHeap, ctx: &'a LLVMContext, verbose: bool) -> LLVMModule<'a> {
    let module = ctx.create_module("test");
    let types = create_types(env, liveness, ctx, &module);

    unsafe {
        let builtins = declare_builtins(&module, &types);

        let mut module = MochaModule {
            env,
            ctx,
            module: &module,
            types,
            builtins,
            objs: heap.all_objs(),
            known_objs: &program.known_objects,
            obj_map: HashMap::new(),
            methods: HashMap::new()
        };

        for (i, obj) in module.objs.iter().enumerate() {
            let name = CString::new(format!("obj_{}_{}", i, obj.class().name(env))).unwrap();

            module.obj_map.insert(
                obj.as_ptr(),
                define_static_heap_object(&module, &name, obj)
            );
        };

        for method_id in liveness.may_call.iter().cloned() {
            if let Some(func) = program.funcs.get(&method_id) {
                module.methods.insert(
                    method_id,
                    define_function(&module, func)
                );
            };
        };

        for class_id in liveness.needs_class_object.iter().cloned() {
            emit_itable(&module, class_id, liveness);
            emit_vtable(&module, class_id, liveness);
        };

        for obj in module.objs.iter() {
            emit_static_heap_object(&module, obj);
        };

        for method_id in liveness.may_call.iter().sorted_by_key(|m| ((m.0).0, m.1)).cloned() {
            if let Some(func) = program.funcs.get(&method_id) {
                emit_function(&module, func);
            };
        };

        emit_main_function(&module, program.main_method);

        if verbose {
            LLVMDumpModule(module.module.ptr());
        };
        LLVMVerifyModule(module.module.ptr(), LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
    };

    module
}
