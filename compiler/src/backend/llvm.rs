use std::collections::{HashMap, HashSet};
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::os::raw::c_char;
use std::ptr::NonNull;

use itertools::Itertools;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate};
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

#[derive(Debug, Clone, Copy)]
pub struct LLVMValue<'a>(PhantomData<&'a LLVMContext>, LLVMValueRef);

impl <'a> LLVMValue<'a> {
    pub unsafe fn from_raw(ptr: LLVMValueRef) -> LLVMValue<'a> {
        LLVMValue(PhantomData, ptr)
    }

    pub fn ptr(&self) -> LLVMValueRef {
        self.1
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMPhiValue<'a>(LLVMValue<'a>);

impl <'a> LLVMPhiValue<'a> {
    pub unsafe fn from_val_unchecked(val: LLVMValue<'a>) -> LLVMPhiValue<'a> {
        LLVMPhiValue(val)
    }

    pub fn into_val(self) -> LLVMValue<'a> {
        self.0
    }

    pub fn add_incoming(&self, incoming: &[(LLVMBasicBlockRef, LLVMValue<'a>)]) {
        let mut incoming_blocks = Vec::with_capacity(incoming.len());
        let mut incoming_values = Vec::with_capacity(incoming.len());

        for (block, val) in incoming.iter() {
            incoming_blocks.push(*block);
            incoming_values.push(val.ptr());
        };

        unsafe {
            LLVMAddIncoming(self.into_val().ptr(), incoming_values.as_mut_ptr(), incoming_blocks.as_mut_ptr(), incoming.len() as u32);
        };
    }
}

#[derive(Debug)]
pub struct LLVMBuilder<'a>(PhantomData<&'a LLVMContext>, LLVMBuilderRef);

const EMPTY_VALUE_NAME: *const c_char = b"\0".as_ptr() as *const c_char;

fn cstr_or_empty(cstr: &Option<CString>) -> *const c_char {
    cstr.as_ref().map(|s| s.as_ptr()).unwrap_or(EMPTY_VALUE_NAME)
}

impl <'a> LLVMBuilder<'a> {
    pub unsafe fn from_raw(ptr: LLVMBuilderRef) -> LLVMBuilder<'a> {
        LLVMBuilder(PhantomData, ptr)
    }

    pub fn ptr(&self) -> LLVMBuilderRef {
        self.1
    }

    unsafe fn wrap_value(&self, val: LLVMValueRef) -> LLVMValue<'a> {
        LLVMValue::from_raw(val)
    }

    pub fn build_struct_gep(&self, val: LLVMValue<'a>, idx: u32, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildStructGEP(self.ptr(), val.ptr(), idx, cstr_or_empty(&name)))
        }
    }

    pub fn build_gep(&self, val: LLVMValue<'a>, idxs: &[LLVMValue<'a>], name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            let mut idxs = idxs.iter().map(|idx| idx.ptr()).collect_vec();
            self.wrap_value(LLVMBuildGEP(self.ptr(), val.ptr(), idxs.as_mut_ptr(), idxs.len() as u32, cstr_or_empty(&name)))
        }
    }

    pub fn build_pointer_cast(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildPointerCast(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_int_to_ptr(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildIntToPtr(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_load(&self, addr: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildLoad(self.ptr(), addr.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_store(&self, val: LLVMValue<'a>, addr: LLVMValue<'a>) {
        unsafe {
            self.wrap_value(LLVMBuildStore(self.ptr(), val.ptr(), addr.ptr()));
        }
    }

    pub fn build_bit_cast(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildBitCast(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_int_cast(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildIntCast(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_int_cast_2(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, signed: bool, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildIntCast2(self.ptr(), val.ptr(), ty, signed as i32, cstr_or_empty(&name)))
        }
    }

    pub fn build_sext(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSExt(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_trunc(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildTrunc(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_si_to_fp(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSIToFP(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_fp_to_si(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFPToSI(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_fpext(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFPExt(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_fptrunc(&self, val: LLVMValue<'a>, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFPTrunc(self.ptr(), val.ptr(), ty, cstr_or_empty(&name)))
        }
    }

    pub fn build_phi(&self, ty: LLVMTypeRef, name: Option<CString>) -> LLVMPhiValue<'a> {
        unsafe {
            LLVMPhiValue::from_val_unchecked(self.wrap_value(LLVMBuildPhi(self.ptr(), ty, cstr_or_empty(&name))))
        }
    }

    pub fn build_neg(&self, val: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildNeg(self.ptr(), val.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fneg(&self, val: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFNeg(self.ptr(), val.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fcmp(&self, pred: LLVMRealPredicate, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFCmp(self.ptr(), pred, lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_select(&self, cond: LLVMValue<'a>, true_val: LLVMValue<'a>, false_val: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSelect(self.ptr(), cond.ptr(), true_val.ptr(), false_val.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_add(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildAdd(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_sub(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSub(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_nuw_sub(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildNUWSub(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_mul(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildMul(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_sdiv(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSDiv(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_srem(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildSRem(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_and(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildAnd(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_or(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildOr(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_xor(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildXor(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_ashr(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildAShr(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_lshr(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildLShr(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_shl(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildShl(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_icmp(&self, pred: LLVMIntPredicate, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildICmp(self.ptr(), pred, lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fadd(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFAdd(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fsub(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFSub(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fmul(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFMul(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_fdiv(&self, lhs: LLVMValue<'a>, rhs: LLVMValue<'a>, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildFDiv(self.ptr(), lhs.ptr(), rhs.ptr(), cstr_or_empty(&name)))
        }
    }

    pub fn build_call(&self, func: LLVMValue<'a>, args: &[LLVMValue<'a>], name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            let mut args = args.iter().map(|arg| arg.ptr()).collect_vec();
            self.wrap_value(LLVMBuildCall(self.ptr(), func.ptr(), args.as_mut_ptr(), args.len() as u32, cstr_or_empty(&name)))
        }
    }

    pub fn build_br(&self, block: LLVMBasicBlockRef) {
        unsafe {
            self.wrap_value(LLVMBuildBr(self.ptr(), block));
        }
    }

    pub fn build_cond_br(&self, cond: LLVMValue<'a>, true_block: LLVMBasicBlockRef, false_block: LLVMBasicBlockRef) {
        unsafe {
            self.wrap_value(LLVMBuildCondBr(self.ptr(), cond.ptr(), true_block, false_block));
        }
    }

    pub fn build_unreachable(&self) {
        unsafe {
            self.wrap_value(LLVMBuildUnreachable(self.ptr()));
        }
    }

    pub fn build_ret_void(&self) {
        unsafe {
            self.wrap_value(LLVMBuildRetVoid(self.ptr()));
        }
    }

    pub fn build_ret(&self, val: LLVMValue<'a>) {
        unsafe {
            self.wrap_value(LLVMBuildRet(self.ptr(), val.ptr()));
        }
    }

    pub fn build_alloca(&self, ty: LLVMTypeRef, name: Option<CString>) -> LLVMValue<'a> {
        unsafe {
            self.wrap_value(LLVMBuildAlloca(self.ptr(), ty, cstr_or_empty(&name)))
        }
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
    obj_map: HashMap<NonNull<u8>, LLVMValue<'a>>,
    methods: HashMap<MethodId, LLVMValue<'a>>,
    builtin_class_table: LLVMValue<'a>
}

impl <'a, 'b, 'c> MochaModule<'a, 'b, 'c> {
    fn find_known_object(&self, id: MilKnownObjectId) -> LLVMValue<'a> {
        self.obj_map[&self.known_objs.get(id).as_ptr()]
    }

    fn find_class_object(&self, class_id: ClassId) -> LLVMValue<'a> {
        self.find_known_object(self.known_objs.refs.classes[&class_id])
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

unsafe fn create_builtin_class_table(env: &ClassEnvironment, ctx: &LLVMContext, module: &LLVMModule, types: &LLVMTypes) -> LLVMValueRef {
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

unsafe fn define_function(module: &MochaModule, func: &MilFunction) -> LLVMValueRef {
    let (class, method) = module.env.get_method(func.id);
    let name = CString::new(format!("{}.{}{}", class.meta.name, method.name, method.descriptor)).unwrap();

    LLVMAddFunction(module.module.ptr(), name.as_ptr(), module.types.method_types[&func.id])
}

fn create_value_ref<'a>(module: &MochaModule<'a, '_, '_>, op: &MilOperand, regs: &HashMap<MilRegister, LLVMValue<'a>>) -> LLVMValue<'a> {
    match *op {
        MilOperand::Register(r) => regs[&r],
        MilOperand::Null => module.const_obj_null(),
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

fn create_instance_field_gep<'a>(builder: &LLVMBuilder<'a>, field_id: FieldId, obj: LLVMValue<'a>, known_objects: &MilKnownObjectMap, obj_map: &HashMap<NonNull<u8>, LLVMValue<'a>>, types: &LLVMTypes) -> LLVMValue<'a> {
    let class_ty = &types.class_types[&field_id.0];

    builder.build_struct_gep(
        builder.build_pointer_cast(obj, types.class_types[&field_id.0].field_ty, None),
        class_ty.fields[&field_id] as u32,
        None
    )
}

fn create_vtable_decompress<'a>(builder: &LLVMBuilder<'a>, compressed: LLVMValue<'a>, class_id: ClassId, types: &LLVMTypes) -> LLVMValue<'a> {
    builder.build_int_to_ptr(
        builder.build_nuw_sub(compressed, unsafe { LLVMValue::from_raw(LLVMConstInt(types.int, 8, 0)) }, None),
        unsafe { LLVMPointerType(LLVMGlobalGetValueType(types.class_types[&class_id].vtable), 0) },
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

fn create_vtable_load<'a>(builder: &LLVMBuilder<'a>, obj: LLVMValue<'a>, class_id: ClassId, types: &LLVMTypes) -> LLVMValue<'a> {
    let obj = builder.build_pointer_cast(obj, types.class_types[&ClassId::JAVA_LANG_OBJECT].field_ty, None);
    let vtable = builder.build_load(builder.build_struct_gep(obj, 0, None), None);

    create_vtable_decompress(builder, vtable, class_id, types)
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
        MilType::Void => module.const_obj_null(),
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
                        module.types.short,
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
                    create_instance_field_gep(builder, field_id, obj, &module.known_objs, &module.obj_map, &module.types),
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
                    create_instance_field_gep(builder, field_id, obj, &module.known_objs, &module.obj_map, &module.types)
                );
            },
            MilInstructionKind::GetArrayLength(tgt, ref obj) => {
                let obj = create_value_ref(&module, obj, &local_regs);

                let val = builder.build_load(
                    create_instance_field_gep(builder, FieldId(ClassId::JAVA_LANG_OBJECT_ARRAY, 0), obj, &module.known_objs, &module.obj_map, &module.types),
                    Some(register_name(tgt))
                );

                set_register(builder, func, &mut local_regs, all_regs, tgt, val, &module.types);
            },
            MilInstructionKind::GetArrayElement(class_id, tgt, ref obj, ref idx) => {
                let obj = create_value_ref(&module, obj, &local_regs);
                let idx = create_value_ref(&module, idx, &local_regs);

                let arr_data = create_instance_field_gep(builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.known_objs, &module.obj_map, &module.types);

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

                let arr_data = create_instance_field_gep(builder, FieldId(module.env.try_find_array(class_id).unwrap(), 1), obj, &module.known_objs, &module.obj_map, &module.types);

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
        MilEndInstructionKind::CallVirtual(_, method_id, tgt, ref obj, ref args) => {
            let (_, method) = module.env.get_method(method_id);
            let obj = create_value_ref(&module, obj, &local_regs);
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let vtable = create_vtable_load(builder, obj, method_id.0, &module.types);
            let vslot = builder.build_struct_gep(vtable, VTABLE_FIRST_VSLOT_FIELD as u32 + method.virtual_slot, None);
            let vslot = builder.build_load(vslot, None);
            let vslot = builder.build_pointer_cast(vslot, LLVMPointerType(module.types.method_types[&method_id], 0), None);

            let return_val = builder.build_call(vslot, &args[..], Some(register_name(tgt)));

            set_register(builder, func, &mut local_regs, all_regs, tgt, return_val, &module.types);
        },
        MilEndInstructionKind::CallInterface(_, method_id, tgt, ref obj, ref args) => {
            let interface_vtable = LLVMValue::from_raw(LLVMConstIntCast(
                LLVMConstAdd(
                    LLVMConstPointerCast(module.types.class_types[&method_id.0].vtable, module.types.long),
                    module.const_long(8).ptr()
                ),
                module.types.int,
                0
            ));

            let (_, method) = module.env.get_method(method_id);
            let obj = create_value_ref(&module, obj, &local_regs);
            let args = args.iter()
                .map(|o| create_value_ref(&module, o, &local_regs))
                .collect_vec();

            let first_islot = builder.build_struct_gep(
                builder.build_load(
                    builder.build_struct_gep(
                        create_vtable_load(builder, obj, ClassId::JAVA_LANG_OBJECT, &module.types),
                        VTABLE_ITABLE_FIELD as u32,
                        None
                    ),
                    None
                ),
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

unsafe fn emit_main_function(module: &MochaModule, main_method: MethodId) -> LLVMValueRef {
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

    let env = builder.build_call(
        LLVMValue::from_raw(module.builtins.init),
        &[LLVMValue::from_raw(LLVMConstPointerCast(module.builtin_class_table.ptr(), module.types.any_raw_pointer))],
        Some(CStr::from_ptr(b"env\0".as_ptr() as *const c_char).to_owned())
    );

    builder.build_call(module.methods[&main_method], &[module.const_obj_null()], None);
    builder.build_call(LLVMValue::from_raw(module.builtins.shutdown), &[env], None);

    builder.build_ret(module.const_int(0));

    main_func
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
                        |f| LLVMConstBitCast(f.ptr(), module.types.any_function_pointer)
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
                |f| LLVMConstBitCast(f.ptr(), module.types.any_function_pointer)
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
        let builtin_class_table = LLVMValue::from_raw(create_builtin_class_table(env, ctx, &module, &types));

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
            builtin_class_table
        };

        for (i, obj) in module.objs.iter().enumerate() {
            let name = CString::new(format!("obj_{}_{}", i, obj.class().name(env))).unwrap();

            module.obj_map.insert(
                obj.as_ptr(),
                LLVMValue::from_raw(define_static_heap_object(&module, &name, obj))
            );
        };

        for method_id in liveness.may_call.iter().cloned() {
            if let Some(func) = program.funcs.get(&method_id) {
                module.methods.insert(
                    method_id,
                    LLVMValue::from_raw(define_function(&module, func))
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
