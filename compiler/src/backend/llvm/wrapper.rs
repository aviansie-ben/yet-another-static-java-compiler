use std::ffi::{CStr, CString};
use std::marker::PhantomData;
use std::os::raw::c_char;

use itertools::Itertools;
use llvm_sys::{LLVMIntPredicate, LLVMRealPredicate, LLVMModuleFlagBehavior};
use llvm_sys::bit_writer::*;
use llvm_sys::core::*;
use llvm_sys::debuginfo::*;
use llvm_sys::prelude::*;

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
                self,
                LLVMModuleCreateWithNameInContext(name.as_ptr(), self.ptr()),
                name
            )
        }
    }

    pub fn create_builder(&self) -> LLVMBuilder {
        unsafe {
            LLVMBuilder::from_raw(self, LLVMCreateBuilderInContext(self.ptr()))
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
pub struct LLVMModule<'a>(&'a LLVMContext, LLVMModuleRef, CString);

impl <'a> LLVMModule<'a> {
    pub unsafe fn from_raw(ctx: &'a LLVMContext, ptr: LLVMModuleRef, name: CString) -> LLVMModule<'a> {
        LLVMModule(ctx, ptr, name)
    }

    pub fn ctx(&self) -> &'a LLVMContext {
        self.0
    }

    pub fn ptr(&self) -> LLVMModuleRef {
        self.1
    }

    pub fn add_function(&self, name: &CStr, func_ty: LLVMTypeRef) -> LLVMFunctionValue<'a> {
        unsafe {
            LLVMFunctionValue::from_val_unchecked(LLVMValue::from_raw(
                LLVMAddFunction(self.ptr(), name.as_ptr(), func_ty)
            ))
        }
    }

    pub fn add_flag(&self, behavior: LLVMModuleFlagBehavior, name: &str, metadata: LLVMMetadata<'a>) {
        unsafe {
            LLVMAddModuleFlag(self.ptr(), behavior, name.as_ptr() as *const c_char, name.len(), metadata.ptr())
        }
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

    pub fn create_di_builder(&self) -> LLVMDIBuilder<'a> {
        unsafe {
            LLVMDIBuilder::from_raw(self.ctx(), LLVMCreateDIBuilder(self.ptr()))
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

#[derive(Debug, Clone, Copy)]
pub struct LLVMFunctionValue<'a>(LLVMValue<'a>);

impl <'a> LLVMFunctionValue<'a> {
    pub unsafe fn from_val_unchecked(val: LLVMValue<'a>) -> LLVMFunctionValue<'a> {
        LLVMFunctionValue(val)
    }

    pub fn into_val(self) -> LLVMValue<'a> {
        self.0
    }

    pub fn set_subprogram(&self, subprogram: LLVMMetadata<'a>) {
        unsafe {
            LLVMSetSubprogram(self.into_val().ptr(), subprogram.ptr());
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LLVMMetadata<'a>(PhantomData<&'a LLVMContext>, LLVMMetadataRef);

impl <'a> LLVMMetadata<'a> {
    pub unsafe fn from_raw(ptr: LLVMMetadataRef) -> LLVMMetadata<'a> {
        LLVMMetadata(PhantomData, ptr)
    }

    pub fn from_value(val: LLVMValue<'a>) -> LLVMMetadata<'a> {
        unsafe {
            LLVMMetadata::from_raw(LLVMValueAsMetadata(val.ptr()))
        }
    }

    pub fn ptr(&self) -> LLVMMetadataRef {
        self.1
    }
}

#[derive(Debug)]
pub struct LLVMDIBuilder<'a>(&'a LLVMContext, LLVMDIBuilderRef);

impl <'a> LLVMDIBuilder<'a> {
    pub unsafe fn from_raw(ctx: &'a LLVMContext, ptr: LLVMDIBuilderRef) -> LLVMDIBuilder<'a> {
        LLVMDIBuilder(ctx, ptr)
    }

    pub fn ctx(&self) -> &'a LLVMContext {
        self.0
    }

    pub fn ptr(&self) -> LLVMDIBuilderRef {
        self.1
    }

    unsafe fn wrap_metadata(&self, metadata: LLVMMetadataRef) -> LLVMMetadata<'a> {
        LLVMMetadata::from_raw(metadata)
    }

    pub fn create_file(&self, file: &str, dir: &str) -> LLVMMetadata<'a> {
        unsafe {
            self.wrap_metadata(LLVMDIBuilderCreateFile(
                self.ptr(),
                file.as_ptr() as *const c_char,
                file.len(),
                dir.as_ptr() as *const c_char,
                dir.len()
            ))
        }
    }

    pub fn create_compile_unit(&self, lang: LLVMDWARFSourceLanguage, file: LLVMMetadata<'a>, producer: &str, is_optimized: bool, kind: LLVMDWARFEmissionKind) -> LLVMMetadata<'a> {
        unsafe {
            self.wrap_metadata(LLVMDIBuilderCreateCompileUnit(
                self.ptr(),
                lang,
                file.ptr(),
                producer.as_ptr() as *const c_char,
                producer.len(),
                if is_optimized { 1 } else { 0 },
                std::ptr::null_mut(),
                0,
                0,
                std::ptr::null_mut(),
                0,
                kind,
                0,
                0,
                0
            ))
        }
    }

    pub fn create_function(
        &self,
        scope: LLVMMetadata<'a>,
        file: LLVMMetadata<'a>,
        line: u32,
        name: &str,
        linkage_name: &str,
        return_type: Option<LLVMMetadata<'a>>,
        param_types: &[LLVMMetadata<'a>],
        is_definition: bool,
        flags: LLVMDIFlags,
        is_optimized: bool
    ) -> LLVMMetadata<'a> {
        unsafe {
            let mut types = itertools::repeat_n(return_type.map_or(std::ptr::null_mut(), |ty| ty.ptr()), 1)
                .chain(param_types.iter().map(|ty| ty.ptr()))
                .collect_vec();
            let ty = LLVMDIBuilderCreateSubroutineType(self.ptr(), file.ptr(), types.as_mut_ptr(), types.len() as u32, LLVMDIFlags::LLVMDIFlagZero);

            self.wrap_metadata(LLVMDIBuilderCreateFunction(
                self.ptr(),
                scope.ptr(),
                name.as_ptr() as *const c_char,
                name.len(),
                linkage_name.as_ptr() as *const c_char,
                linkage_name.len(),
                file.ptr(),
                line,
                ty,
                0,
                if is_definition { 1 } else { 0 },
                line,
                flags,
                if is_optimized { 1 } else { 0 }
            ))
        }
    }

    pub fn create_debug_location(&self, line: u32, col: u32, scope: LLVMMetadata<'a>, inlined_at: Option<LLVMMetadata<'a>>) -> LLVMMetadata<'a> {
        unsafe {
            self.wrap_metadata(LLVMDIBuilderCreateDebugLocation(
                self.0.ptr(),
                line,
                col,
                scope.ptr(),
                inlined_at.map_or(std::ptr::null_mut(), |m| m.ptr())
            ))
        }
    }

    pub fn finalize(&self) {
        unsafe {
            LLVMDIBuilderFinalize(self.ptr());
        }
    }
}

impl <'a> Drop for LLVMDIBuilder<'a> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeDIBuilder(self.ptr())
        }
    }
}

#[derive(Debug)]
pub struct LLVMBuilder<'a>(&'a LLVMContext, LLVMBuilderRef);

const EMPTY_VALUE_NAME: *const c_char = b"\0".as_ptr() as *const c_char;

fn cstr_or_empty(cstr: &Option<CString>) -> *const c_char {
    cstr.as_ref().map(|s| s.as_ptr()).unwrap_or(EMPTY_VALUE_NAME)
}

impl <'a> LLVMBuilder<'a> {
    pub unsafe fn from_raw(ctx: &'a LLVMContext, ptr: LLVMBuilderRef) -> LLVMBuilder<'a> {
        LLVMBuilder(ctx, ptr)
    }

    pub fn ptr(&self) -> LLVMBuilderRef {
        self.1
    }

    unsafe fn wrap_value(&self, val: LLVMValueRef) -> LLVMValue<'a> {
        LLVMValue::from_raw(val)
    }

    pub fn set_current_debug_location(&self, loc: Option<LLVMMetadata<'a>>) {
        unsafe {
            LLVMSetCurrentDebugLocation(self.ptr(), loc.map_or(std::ptr::null_mut(), |loc| LLVMMetadataAsValue(self.0.ptr(), loc.ptr())));
        }
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
