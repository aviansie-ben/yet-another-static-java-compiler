use std::collections::HashMap;
use std::ffi::CString;
use std::marker::PhantomData;
use std::os::raw::c_char;
use std::ptr::NonNull;

use itertools::Itertools;
use llvm_sys::analysis::*;
use llvm_sys::bit_writer::*;
use llvm_sys::core::*;
use llvm_sys::prelude::*;

use crate::classfile::PrimitiveType;
use crate::layout;
use crate::liveness::LivenessInfo;
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

const VTABLE_CLASS_OBJECT_FIELD: usize = 0;
const VTABLE_ITABLE_FIELD: usize = 1;
const VTABLE_FIRST_VSLOT_FIELD: usize = 2;

#[derive(Debug)]
struct LLVMClassType {
    name: CString,
    value_ty: LLVMTypeRef,
    field_ty: LLVMTypeRef,
    num_fields: usize,
    fields: HashMap<FieldId, usize>,
    pads: Vec<(u32, u32)>,
    meta_ty: LLVMTypeRef,
    num_meta_fields: usize,
    meta_fields: HashMap<FieldId, usize>,
    meta_pads: Vec<(u32, u32)>,
    vtable: LLVMValueRef
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
    any_object_pointer: LLVMTypeRef,
    class_types: HashMap<ClassId, LLVMClassType>
}

unsafe fn lay_out_fields(env: &ClassEnvironment, fields: impl IntoIterator<Item=(FieldId, u32)>, llvm_fields: &mut Vec<LLVMTypeRef>, field_map: &mut HashMap<FieldId, usize>, pads: &mut Vec<(u32, u32)>, mut current_size: u32, types: &LLVMTypes) {
    for (f, off) in fields {
        assert!(off >= current_size);
        if off != current_size {
            pads.push((llvm_fields.len() as u32, off - current_size));
            llvm_fields.push(LLVMArrayType(types.byte, off - current_size));
        };

        let field = env.get_field(f).1;

        field_map.insert(f, llvm_fields.len());
        llvm_fields.push(types.class_types[&field.class_id].field_ty);
        current_size = off + layout::get_field_size_align(env, field.class_id).0;
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

    let vtable = if let Some(vslot_class) = vslot_class {
        let mut vtable_fields = vec![types.any_function_pointer; vslot_class.layout.virtual_slots.len() + VTABLE_FIRST_VSLOT_FIELD];

        vtable_fields[VTABLE_CLASS_OBJECT_FIELD] = LLVMPointerType(types.class_types[&class_id].meta_ty, 1);
        vtable_fields[VTABLE_ITABLE_FIELD] = LLVMPointerType(types.int, 0) /* TODO */;

        let name = CString::new(format!("vtable_{}", env.get(class_id).name(env))).unwrap();
        let global = LLVMAddGlobal(module.ptr(), LLVMStructType(vtable_fields.as_mut_ptr(), vtable_fields.len() as u32, 0), name.as_ptr());
        LLVMSetSection(global, b".mocha_vtables\0".as_ptr() as *mut c_char);

        global
    } else {
        std::ptr::null_mut()
    };

    let mut class_type = types.class_types.get_mut(&class_id).unwrap();

    LLVMStructSetBody(class_type.meta_ty, meta_fields.as_mut_ptr(), meta_fields.len() as u32, 1);
    class_type.num_meta_fields = meta_fields.len();
    class_type.meta_fields = meta_field_map;
    class_type.meta_pads = meta_pads;
    class_type.vtable = vtable;
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
        let any_object_pointer = LLVMPointerType(byte, 1);

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
            any_object_pointer,
            class_types: env.class_ids().map(|class_id| {
                (class_id, {
                    let class = env.get(class_id);
                    let name = CString::new(&*class.name(env)).unwrap();

                    let (value_ty, field_ty) = match **class {
                        ResolvedClass::User(_) | ResolvedClass::Array(_) => {
                            let value_ty = LLVMStructCreateNamed(ctx.ptr(), name.as_ptr());

                            (value_ty, LLVMPointerType(value_ty, 1))
                        },
                        ResolvedClass::Primitive(None) => (void, void),
                        ResolvedClass::Primitive(Some(PrimitiveType::Byte)) => (byte, byte),
                        ResolvedClass::Primitive(Some(PrimitiveType::Boolean)) => (byte, byte),
                        ResolvedClass::Primitive(Some(PrimitiveType::Short)) => (short, short),
                        ResolvedClass::Primitive(Some(PrimitiveType::Char)) => (short, short),
                        ResolvedClass::Primitive(Some(PrimitiveType::Int)) => (int, int),
                        ResolvedClass::Primitive(Some(PrimitiveType::Long)) => (long, long),
                        ResolvedClass::Primitive(Some(PrimitiveType::Float)) => (float, float),
                        ResolvedClass::Primitive(Some(PrimitiveType::Double)) => (double, double)
                    };

                    let meta_name = CString::new(format!("java/lang/Class#{}", class.name(env))).unwrap();
                    let meta_ty = LLVMStructCreateNamed(ctx.ptr(), meta_name.as_ptr());

                    LLVMClassType {
                        name,
                        value_ty,
                        field_ty,
                        num_fields: 0,
                        fields: HashMap::new(),
                        pads: vec![],
                        meta_ty,
                        num_meta_fields: 0,
                        meta_fields: HashMap::new(),
                        meta_pads: vec![],
                        vtable: std::ptr::null_mut()
                    }
                })
            }).collect()
        };

        for class_id in env.class_ids() {
            fill_type(env, class_id, liveness, module, &mut types);
        };

        types
    }
}

struct LLVMMochaBuiltins {
    alloc_obj: LLVMValueRef
}

unsafe fn declare_builtins(module: &LLVMModule, types: &LLVMTypes) -> LLVMMochaBuiltins {
    LLVMMochaBuiltins {
        alloc_obj: LLVMAddFunction(
            module.ptr(),
            b"mocha_alloc_obj\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_object_pointer,
                [types.any_object_pointer].as_mut_ptr(),
                1,
                0
            )
        )
    }
}

unsafe fn define_function(env: &ClassEnvironment, func: &MilFunction, types: &LLVMTypes, module: &LLVMModule) -> LLVMValueRef {
    let (class, method) = env.get_method(func.id);
    let name = CString::new(format!("{}.{}{}", class.meta.name, method.name, method.descriptor)).unwrap();

    let mut param_types = method.param_types.iter().cloned().map(|class_id| types.class_types[&class_id].field_ty).collect_vec();
    let return_type = types.class_types[&method.return_type].field_ty;

    let func_type = LLVMFunctionType(return_type, param_types.as_mut_ptr(), param_types.len() as u32, 0);
    LLVMAddFunction(module.ptr(), name.as_ptr(), func_type)
}

unsafe fn create_value_ref(op: &MilOperand, regs: &HashMap<MilRegister, LLVMValueRef>, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>, types: &LLVMTypes) -> LLVMValueRef {
    match *op {
        MilOperand::Register(r) => regs[&r],
        MilOperand::Null => LLVMConstNull(types.any_object_pointer),
        MilOperand::KnownObject(object_id, _) => obj_map[&known_objects.get(object_id).as_ptr()],
        MilOperand::Bool(val) => LLVMConstInt(types.bool, if val { 1 } else { 0 }, 0),
        MilOperand::Int(val) => LLVMConstInt(types.int, val as u64, 1),
        MilOperand::Long(val) => LLVMConstInt(types.long, val as u64, 1),
        MilOperand::Float(val) => LLVMConstBitCast(LLVMConstInt(types.int, val as u64, 0), types.float),
        MilOperand::Double(val) => LLVMConstBitCast(LLVMConstInt(types.long, val, 0), types.double)
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

unsafe fn emit_function(env: &ClassEnvironment, func: &MilFunction, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>, func_map: &HashMap<MethodId, LLVMValueRef>, types: &LLVMTypes, builtins: &LLVMMochaBuiltins, ctx: &LLVMContext, module: &LLVMModule) {
    let llvm_func = func_map[&func.id];
    let builder = ctx.create_builder();

    let mut llvm_blocks = HashMap::new();

    // TODO Support control flow
    let mut local_regs = HashMap::new();

    for block_id in func.block_order.iter().cloned() {
        let block = &func.blocks[&block_id];
        let block_name = CString::new(format!("{}", block.id)).unwrap();
        let llvm_block = LLVMAppendBasicBlockInContext(ctx.ptr(), llvm_func, block_name.as_ptr());

        LLVMPositionBuilderAtEnd(builder.ptr(), llvm_block);

        for instr in block.instrs.iter() {
            match instr.kind {
                MilInstructionKind::Nop => {},
                MilInstructionKind::Copy(tgt, ref src) => {
                    local_regs.insert(tgt, create_value_ref(src, &local_regs, known_objects, obj_map, types));
                },
                MilInstructionKind::GetParam(idx, _, tgt) => {
                    local_regs.insert(tgt, LLVMGetParam(llvm_func, idx as u32));
                },
                MilInstructionKind::GetField(field_id, _, tgt, ref obj) => {
                    let obj = create_value_ref(obj, &local_regs, known_objects, obj_map, types);

                    local_regs.insert(tgt, LLVMBuildLoad(
                        builder.ptr(),
                        create_instance_field_gep(&builder, field_id, obj, known_objects, obj_map, types),
                        register_name(tgt).as_ptr() as *const c_char
                    ));
                },
                MilInstructionKind::PutField(field_id, class_id, ref obj, ref src) => {
                    let obj = create_value_ref(obj, &local_regs, known_objects, obj_map, types);
                    let src = create_value_ref(src, &local_regs, known_objects, obj_map, types);

                    LLVMBuildStore(
                        builder.ptr(),
                        LLVMBuildPointerCast(
                            builder.ptr(),
                            src,
                            types.class_types[&class_id].field_ty,
                            b"\0".as_ptr() as *const c_char
                        ),
                        create_instance_field_gep(&builder, field_id, obj, known_objects, obj_map, types)
                    );
                },
                MilInstructionKind::GetStatic(field_id, _, tgt) => {
                    let class_obj = obj_map[&known_objects.get(known_objects.refs.classes[&field_id.0]).as_ptr()];

                    local_regs.insert(tgt, LLVMBuildLoad(
                        builder.ptr(),
                        create_meta_field_gep(&builder, field_id, known_objects, obj_map, types),
                        register_name(tgt).as_ptr() as *const c_char
                    ));
                },
                MilInstructionKind::PutStatic(field_id, class_id, ref src) => {
                    let src = create_value_ref(src, &local_regs, known_objects, obj_map, types);

                    LLVMBuildStore(
                        builder.ptr(),
                        LLVMBuildPointerCast(
                            builder.ptr(),
                            src,
                            types.class_types[&class_id].field_ty,
                            b"\0".as_ptr() as *const c_char
                        ),
                        create_meta_field_gep(&builder, field_id, known_objects, obj_map, types)
                    );
                },
                MilInstructionKind::AllocObj(class_id, tgt) => {
                    let obj = LLVMBuildCall(
                        builder.ptr(),
                        builtins.alloc_obj,
                        [
                            LLVMConstPointerCast(
                                obj_map[&known_objects.get(known_objects.refs.classes[&class_id]).as_ptr()],
                                types.any_object_pointer
                            )
                        ].as_mut_ptr(),
                        1,
                        register_name(tgt).as_ptr() as *const c_char
                    );
                    let obj = LLVMBuildPointerCast(
                        builder.ptr(),
                        obj,
                        types.class_types[&class_id].field_ty,
                        register_name(tgt).as_ptr() as *const c_char
                    );

                    local_regs.insert(tgt, obj);
                },
                MilInstructionKind::AllocArray(_, _, _) => unimplemented!()
            };
        };

        match block.end_instr.kind {
            MilEndInstructionKind::Nop => {},
            MilEndInstructionKind::Call(_, method_id, tgt, ref args) => {
                let mut args = args.iter()
                    .map(|o| create_value_ref(o, &local_regs, known_objects, obj_map, types))
                    .zip(env.get_method(method_id).1.param_types.iter().cloned())
                    .map(|(val, ty)| {
                        LLVMBuildBitCast(builder.ptr(), val, types.class_types[&ty].field_ty, b"\0".as_ptr() as *const c_char)
                    })
                    .collect_vec();

                local_regs.insert(tgt, LLVMBuildCall(
                    builder.ptr(),
                    func_map[&method_id],
                    args.as_mut_ptr(),
                    args.len() as u32,
                    register_name(tgt).as_ptr() as *const c_char
                ));
            },
            MilEndInstructionKind::CallVirtual(_, _, _, _, _) => unimplemented!(),
            MilEndInstructionKind::CallNative(ret_ty, ref name, tgt, ref args) => {
                let mut arg_tys = args.iter().map(|a| native_arg_type(a.get_type(&func.reg_map), types)).collect_vec();
                let mut args = args.iter()
                    .map(|o| create_value_ref(o, &local_regs, known_objects, obj_map, types))
                    .zip(arg_tys.iter().cloned())
                    .map(|(val, ty)| {
                        LLVMBuildBitCast(builder.ptr(), val, ty, b"\0".as_ptr() as *const c_char)
                    })
                    .collect_vec();

                let func_ty = LLVMFunctionType(
                    native_arg_type(MilType::for_class(ret_ty), types),
                    arg_tys.as_mut_ptr(),
                    arg_tys.len() as u32,
                    0
                );
                let name = CString::new(name.as_str()).unwrap();
                let func = LLVMAddFunction(module.ptr(), name.as_ptr(), func_ty);

                local_regs.insert(tgt, LLVMBuildCall(
                    builder.ptr(),
                    func,
                    args.as_mut_ptr(),
                    args.len() as u32,
                    register_name(tgt).as_ptr() as *const c_char
                ));
            },
            MilEndInstructionKind::Return(MilOperand::Register(MilRegister::VOID)) => {
                LLVMBuildRetVoid(builder.ptr());
            },
            MilEndInstructionKind::Return(ref val) => {
                LLVMBuildRet(builder.ptr(), create_value_ref(val, &local_regs, known_objects, obj_map, types));
            },
            MilEndInstructionKind::Jump(_) => {}
        };

        llvm_blocks.insert(block_id, (llvm_block, llvm_block));
    };

    for (block_id, next_block_id) in func.block_order.iter().cloned().chain(itertools::repeat_n(MilBlockId::EXIT, 1)).tuple_windows() {
        let block = &func.blocks[&block_id];

        LLVMPositionBuilderAtEnd(builder.ptr(), llvm_blocks[&block_id].1);

        match block.end_instr.kind {
            MilEndInstructionKind::Jump(tgt) => {
                LLVMBuildBr(builder.ptr(), llvm_blocks[&tgt].0);
            },
            MilEndInstructionKind::Return(_) => {},
            _ => {
                LLVMBuildBr(builder.ptr(), llvm_blocks[&next_block_id].0);
            }
        };
    };
}

unsafe fn emit_main_function(env: &ClassEnvironment, main_method: MethodId, func_map: &HashMap<MethodId, LLVMValueRef>, types: &LLVMTypes, module: &LLVMModule, ctx: &LLVMContext) {
    let main_func = LLVMAddFunction(
        module.ptr(),
        b"main\0".as_ptr() as *const c_char,
        LLVMFunctionType(
            types.int,
            [types.int, types.any_object_pointer].as_mut_ptr(),
            2,
            0
        )
    );
    let builder = ctx.create_builder();

    let main_block = LLVMAppendBasicBlockInContext(ctx.ptr(), main_func, b"main\0".as_ptr() as *const c_char);
    LLVMPositionBuilderAtEnd(builder.ptr(), main_block);

    LLVMBuildCall(
        builder.ptr(),
        func_map[&main_method],
        [LLVMConstNull(types.class_types[&env.try_find("[Ljava/lang/String;").unwrap()].field_ty)].as_mut_ptr(),
        1,
        b"\0".as_ptr() as *const c_char
    );
    LLVMBuildRet(builder.ptr(), LLVMConstInt(types.int, 0, 1));
}

unsafe fn emit_vtable(env: &ClassEnvironment, class_id: ClassId, methods: &HashMap<MethodId, LLVMValueRef>, types: &LLVMTypes, ctx: &LLVMContext, module: &LLVMModule) {
    let vslot_class = match **env.get(class_id) {
        ResolvedClass::User(ref class) => class,
        ResolvedClass::Array(_) => env.get(ClassId::JAVA_LANG_OBJECT).as_user_class(),
        _ => {
            return;
        }
    };

    let vslots = vslot_class.layout.virtual_slots.iter().cloned().map(|method_id| {
        methods.get(&method_id).cloned().map_or_else(
            || LLVMConstNull(types.any_function_pointer),
            |f| LLVMConstBitCast(f, types.any_function_pointer)
        )
    });

    let mut vtable_fields = [
        LLVMConstNull(LLVMPointerType(types.class_types[&class_id].meta_ty, 1)),
        LLVMConstNull(LLVMPointerType(types.int, 0))
    ]
        .iter().cloned()
        .chain(vslots)
        .collect_vec();

    let vtable_value = LLVMConstStruct(vtable_fields.as_mut_ptr(), vtable_fields.len() as u32, 0);
    LLVMSetInitializer(types.class_types[&class_id].vtable, vtable_value);
}

unsafe fn value_to_llvm(val: Value, class_id: ClassId, types: &LLVMTypes, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>) -> LLVMValueRef {
    match class_id {
        ClassId::PRIMITIVE_BYTE => LLVMConstInt(types.byte, val.as_int().unwrap() as u64, 1),
        ClassId::PRIMITIVE_CHAR => LLVMConstInt(types.short, val.as_int().unwrap() as u64, 0),
        ClassId::PRIMITIVE_DOUBLE => LLVMConstBitCast(
            LLVMConstInt(types.long, val.as_double().unwrap(), 0),
            types.double
        ),
        ClassId::PRIMITIVE_FLOAT => LLVMConstBitCast(
            LLVMConstInt(types.int, val.as_float().unwrap() as u64, 0),
            types.float
        ),
        ClassId::PRIMITIVE_INT => LLVMConstInt(types.int, val.as_int().unwrap() as u64, 1),
        ClassId::PRIMITIVE_LONG => LLVMConstInt(types.long, val.as_long().unwrap() as u64, 1),
        ClassId::PRIMITIVE_SHORT => LLVMConstInt(types.short, val.as_int().unwrap() as u64, 1),
        ClassId::PRIMITIVE_BOOLEAN => LLVMConstInt(types.byte, val.as_int().unwrap() as u64, 0),
        _ => val.as_ref().unwrap().map_or_else(
            || LLVMConstNull(types.class_types[&class_id].field_ty),
            |r| LLVMConstPointerCast(obj_map[&r.as_ptr()], types.class_types[&class_id].field_ty)
        )
    }
}

unsafe fn emit_static_heap_object<'a>(env: &ClassEnvironment, obj: &JavaStaticRef<'a>, llvm_obj: LLVMValueRef, types: &LLVMTypes, module: &LLVMModule, obj_map: &HashMap<NonNull<u8>, LLVMValueRef>) {
    let class_type = &types.class_types[&obj.class_id()];
    let (ty, meta_class) = if obj.class_id() == ClassId::JAVA_LANG_CLASS {
        let meta_class = &types.class_types[&ClassId(obj.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_long().unwrap() as u32)];
        (meta_class.meta_ty, Some(meta_class))
    } else {
        (class_type.value_ty, None)
    };

    let elem_class = match **env.get(obj.class_id()) {
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
            LLVMConstPointerCast(class_type.vtable, types.long),
            LLVMConstInt(types.long, 8, 0)
        ),
        types.int,
        0
    );
    field_values[1] = LLVMConstInt(types.int, obj.flags().bits() as u64, 0);

    let array_data = if let Some(elem_class) = elem_class {
        field_values[class_type.fields[&FieldId(obj.class_id(), 0)] as usize] = LLVMConstInt(types.int, obj.read_array_length() as u64, 0);
        field_values[class_type.fields[&FieldId(obj.class_id(), 1)] as usize] = LLVMConstArray(types.class_types[&elem_class].field_ty, std::ptr::null_mut(), 0);

        let mut elements = (0..obj.read_array_length())
            .map(|i| value_to_llvm(obj.read_array_element(i), elem_class, types, obj_map))
            .collect_vec();

        Some(LLVMConstArray(
            types.class_types[&elem_class].field_ty,
            elements.as_mut_ptr(),
            elements.len() as u32
        ))
    } else {
        for (&f, &i) in class_type.fields.iter() {
            let field_ty = env.get_field(f).1.class_id;

            field_values[i] = value_to_llvm(obj.read_field(f), field_ty, types, obj_map);
        };
        None
    };

    for (i, size) in class_type.pads.iter().cloned() {
        let mut data = vec![LLVMConstInt(types.byte, 0, 0); size as usize];
        field_values[i as usize] = LLVMConstArray(types.byte, data.as_mut_ptr(), size);
    };

    assert!(!field_values.iter().any(|&val| val.is_null()));

    if let Some(meta_class) = meta_class {
        let vtable_value = if meta_class.vtable.is_null() {
            LLVMConstInt(types.long, 0, 0)
        } else {
            LLVMConstPointerCast(meta_class.vtable, types.long)
        };
        field_values[class_type.fields[&JAVA_LANG_CLASS_VTABLE_PTR_FIELD]] = vtable_value;

        let class = LLVMConstNamedStruct(class_type.value_ty, field_values.as_mut_ptr(), field_values.len() as u32);
        field_values = vec![std::ptr::null_mut(); meta_class.num_meta_fields];

        field_values[0] = class;

        for (&f, &i) in meta_class.meta_fields.iter() {
            let field_ty = env.get_field(f).1.class_id;

            field_values[i] = value_to_llvm(obj.read_field(f), field_ty, types, obj_map);
        };

        for (i, size) in meta_class.meta_pads.iter().cloned() {
            let mut data = vec![LLVMConstInt(types.byte, 0, 0); size as usize];
            field_values[i as usize] = LLVMConstArray(types.byte, data.as_mut_ptr(), size);
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

    LLVMSetInitializer(llvm_obj, value);
}

unsafe fn define_static_heap_object<'a>(env: &ClassEnvironment, name: &CString, obj: &JavaStaticRef<'a>, types: &LLVMTypes, module: &LLVMModule) -> LLVMValueRef {
    let ty = if obj.class_id() == ClassId::JAVA_LANG_CLASS {
        types.class_types[&ClassId(obj.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD).as_long().unwrap() as u32)].meta_ty
    } else {
        match **env.get(obj.class_id()) {
            ResolvedClass::Array(elem_class) => {
                let mut fields = [
                    types.class_types[&obj.class_id()].value_ty,
                    LLVMArrayType(types.class_types[&elem_class].field_ty, obj.read_array_length() as u32)
                ];

                LLVMStructType(fields.as_mut_ptr(), fields.len() as u32, 1)
            },
            _ => types.class_types[&obj.class_id()].value_ty
        }
    };

    let global = LLVMAddGlobalInAddressSpace(module.ptr(), ty, name.as_ptr(), 1);
    LLVMSetSection(global, b".mocha_static_heap\0".as_ptr() as *const c_char);
    LLVMSetAlignment(global, 16);

    global
}

pub fn emit_llvm_ir<'a>(env: &ClassEnvironment, program: &MilProgram, liveness: &LivenessInfo, heap: &JavaStaticHeap, ctx: &'a LLVMContext, verbose: bool) -> LLVMModule<'a> {
    let module = ctx.create_module("test");
    let types = create_types(env, liveness, ctx, &module);

    unsafe {
        let builtins = declare_builtins(&module, &types);

        let objs = heap.all_objs();
        let mut obj_map = HashMap::new();

        for (i, obj) in objs.iter().enumerate() {
            let name = CString::new(format!("obj_{}_{}", i, obj.class().name(env))).unwrap();

            obj_map.insert(
                obj.as_ptr(),
                define_static_heap_object(env, &name, obj, &types, &module)
            );
        };

        let mut methods = HashMap::new();
        for method_id in liveness.may_call.iter().cloned() {
            if let Some(func) = program.funcs.get(&method_id) {
                methods.insert(
                    method_id,
                    define_function(env, func, &types, &module)
                );
            };
        };

        for class_id in liveness.may_construct.iter().cloned() {
            emit_vtable(env, class_id, &methods, &types, ctx, &module);
        };

        for obj in objs.iter() {
            emit_static_heap_object(
                env,
                obj,
                obj_map[&obj.as_ptr()],
                &types,
                &module,
                &obj_map
            );
        };

        for method_id in liveness.may_call.iter().cloned() {
            if let Some(func) = program.funcs.get(&method_id) {
                emit_function(env, func, &program.known_objects, &obj_map, &methods, &types, &builtins, ctx, &module);
            };
        };

        emit_main_function(env, program.main_method, &methods, &types, &module, ctx);

        if verbose {
            LLVMDumpModule(module.ptr());
        };
        LLVMVerifyModule(module.ptr(), LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
    };

    module
}
