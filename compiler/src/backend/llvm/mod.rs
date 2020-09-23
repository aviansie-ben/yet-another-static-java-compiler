mod codegen;
mod types;
mod wrapper;

pub use self::wrapper::LLVMContext;

use std::collections::HashMap;
use std::ffi::CStr;
use std::os::raw::c_char;
use std::ptr::NonNull;

use itertools::Itertools;
use llvm_sys::LLVMModuleFlagBehavior;
use llvm_sys::analysis::*;
use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::prelude::*;

use crate::liveness::LivenessInfo;
use crate::mil::il::*;
use crate::resolve::{ClassEnvironment, ClassId, MethodId};
use crate::static_heap::*;

use self::codegen::{define_functions, emit_functions};
use self::types::{LLVMTypes, create_types, define_static_heap, emit_vtables, emit_static_heap};
use self::wrapper::*;

struct MochaModule<'a, 'b, 'c> {
    env: &'a ClassEnvironment,
    ctx: &'a LLVMContext,
    module: &'a LLVMModule<'b>,
    types: LLVMTypes,
    builtins: LLVMMochaBuiltins,
    objs: Vec<JavaStaticRef<'c>>,
    known_objs: &'a MilKnownObjectMap<'c>,
    obj_map: HashMap<NonNull<u8>, LLVMValue<'a>>,
    methods: HashMap<MethodId, LLVMFunctionValue<'a>>,
    builtin_class_table: LLVMValue<'a>,
    di_builder: &'a LLVMDIBuilder<'b>,
    compile_unit: LLVMMetadata<'a>
}

impl <'a, 'b, 'c> MochaModule<'a, 'b, 'c> {
    fn find_known_object(&self, id: MilKnownObjectId) -> LLVMValue<'a> {
        self.obj_map[&self.known_objs.get(id).as_ptr()]
    }

    fn find_class_object(&self, class_id: ClassId) -> LLVMValue<'a> {
        self.find_known_object(self.known_objs.refs.classes[&class_id])
    }

    fn const_addr_null(&self) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstNull(self.types.any_raw_pointer)) }
    }

    fn const_obj_null(&self) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstNull(self.types.any_object_pointer)) }
    }

    fn const_bool(&self, val: bool) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstInt(self.types.bool, if val { 1 } else { 0 }, 0)) }
    }

    fn const_byte(&self, val: i8) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstInt(self.types.byte, (val as u8).into(), 0)) }
    }

    fn const_short(&self, val: i16) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstInt(self.types.short, (val as u16).into(), 0)) }
    }

    fn const_int(&self, val: i32) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstInt(self.types.int, (val as u32).into(), 0)) }
    }

    fn const_long(&self, val: i64) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstInt(self.types.long, (val as u64).into(), 0)) }
    }

    fn const_float(&self, bits: u32) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstBitCast(LLVMConstInt(self.types.int, bits.into(), 0), self.types.float)) }
    }

    fn const_double(&self, bits: u64) -> LLVMValue<'a> {
        unsafe { LLVMValue::from_raw(LLVMConstBitCast(LLVMConstInt(self.types.long, bits.into(), 0), self.types.double)) }
    }
}

struct LLVMMochaBuiltins {
    init: LLVMValueRef,
    shutdown: LLVMValueRef,

    alloc_obj: LLVMValueRef,
    alloc_array: LLVMValueRef,

    throw: LLVMValueRef
}

unsafe fn declare_builtins(module: &LLVMModule, types: &LLVMTypes) -> LLVMMochaBuiltins {
    LLVMMochaBuiltins {
        init: LLVMAddFunction(
            module.ptr(),
            b"mocha_init\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_raw_pointer,
                [types.any_raw_pointer].as_mut_ptr(),
                1,
                0
            )
        ),
        shutdown: LLVMAddFunction(
            module.ptr(),
            b"mocha_shutdown\0".as_ptr() as *const c_char,
            LLVMFunctionType(
                types.any_raw_pointer,
                [types.any_raw_pointer].as_mut_ptr(),
                1,
                0
            )
        ),
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

unsafe fn create_builtin_class_table(module: &LLVMModule, types: &LLVMTypes) -> LLVMValueRef {
    let mut builtin_classes = ClassId::special_classes().map(|id| {
        // HACK: MethodHandle doesn't work yet, so don't try to use it
        if id != ClassId::JAVA_LANG_INVOKE_METHODHANDLE {
            LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(types.class_types[&id].vtable, types.long),
                    LLVMConstInt(types.long, 8, 0)
                ),
                types.int,
                0
            )
        } else {
            LLVMConstInt(types.int, 8, 0)
        }
    }).collect_vec();

    let builtin_classes_type = LLVMArrayType(types.int, builtin_classes.len() as u32);
    let builtin_classes = LLVMConstArray(types.int, builtin_classes.as_mut_ptr(), builtin_classes.len() as u32);

    let global = LLVMAddGlobal(module.ptr(), builtin_classes_type, b"builtin_classes\0".as_ptr() as *const c_char);
    LLVMSetInitializer(global, builtin_classes);

    global
}



unsafe fn emit_main_function<'a>(module: &MochaModule<'a, '_, '_>, main_method: MethodId) -> LLVMFunctionValue<'a> {
    let main_func = module.module.add_function(
        CStr::from_bytes_with_nul(b"main\0").unwrap(),
        LLVMFunctionType(
            module.types.int,
            [module.types.int, module.types.any_raw_pointer].as_mut_ptr(),
            2,
            0
        )
    );
    let builder = module.ctx.create_builder();

    let main_block = LLVMAppendBasicBlockInContext(module.ctx.ptr(), main_func.into_val().ptr(), b"main\0".as_ptr() as *const c_char);
    LLVMPositionBuilderAtEnd(builder.ptr(), main_block);

    let env = builder.build_call(
        LLVMValue::from_raw(module.builtins.init),
        &[LLVMValue::from_raw(LLVMConstPointerCast(module.builtin_class_table.ptr(), module.types.any_raw_pointer))],
        Some(CStr::from_ptr(b"env\0".as_ptr() as *const c_char).to_owned())
    );

    builder.build_call(module.methods[&main_method].into_val(), &[module.const_obj_null()], None);
    builder.build_call(LLVMValue::from_raw(module.builtins.shutdown), &[env], None);

    builder.build_ret(module.const_int(0));

    main_func
}

pub fn emit_llvm_ir<'a>(env: &ClassEnvironment, program: &MilProgram, liveness: &LivenessInfo, heap: &JavaStaticHeap, ctx: &'a LLVMContext, verbose: bool) -> LLVMModule<'a> {
    let module = ctx.create_module("test");
    let types = create_types(env, liveness, ctx, &module);

    unsafe {
        let builtins = declare_builtins(&module, &types);
        let builtin_class_table = LLVMValue::from_raw(create_builtin_class_table(&module, &types));

        let di_builder = module.create_di_builder();

        let mut module = MochaModule {
            env,
            ctx,
            module: &module,
            types,
            builtins,
            objs: heap.all_objs(),
            known_objs: &program.known_objects,
            obj_map: HashMap::new(),
            methods: HashMap::new(),
            builtin_class_table,
            di_builder: &di_builder,
            compile_unit: {
                di_builder.create_compile_unit(
                    LLVMDWARFSourceLanguage::LLVMDWARFSourceLanguageJava,
                    di_builder.create_file("<Java classpath>", ""),
                    "Mocha Compiler",
                    true,
                    LLVMDWARFEmissionKind::LLVMDWARFEmissionKindFull
                )
            }
        };

        module.module.add_flag(
            LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning,
            "Debug Info Version",
            LLVMMetadata::from_value(module.const_int(LLVMDebugMetadataVersion() as i32))
        );
        module.module.add_flag(
            LLVMModuleFlagBehavior::LLVMModuleFlagBehaviorWarning,
            "Dwarf Version",
            LLVMMetadata::from_value(module.const_int(4))
        );

        define_static_heap(&mut module);
        define_functions(program, &mut module, liveness);
        emit_vtables(&module, liveness);
        emit_static_heap(&module);
        emit_functions(program, &module, liveness);
        emit_main_function(&module, program.main_method);

        module.di_builder.finalize();

        if verbose {
            LLVMDumpModule(module.module.ptr());
        };
        LLVMVerifyModule(module.module.ptr(), LLVMVerifierFailureAction::LLVMAbortProcessAction, std::ptr::null_mut());
    };

    module
}

