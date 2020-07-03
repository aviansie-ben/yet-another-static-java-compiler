use crate::obj::{MochaClass, MochaObject};

#[no_mangle]
pub unsafe extern fn java_lang_Object_getClass(obj: *mut MochaObject) -> *mut MochaClass {
    obj.as_ref().unwrap().vtable().class_obj as *const MochaClass as *mut MochaClass
}

#[no_mangle]
pub unsafe extern fn java_lang_Float_floatToRawIntBits(_: *mut MochaClass, val: f32) -> i32 {
    val.to_bits() as i32
}

#[no_mangle]
pub unsafe extern fn java_lang_Float_intBitsToFloat(_: *mut MochaClass, val: i32) -> f32 {
    f32::from_bits(val as u32)
}

#[no_mangle]
pub unsafe extern fn java_lang_Double_doubleToRawLongBits(_: *mut MochaClass, val: f64) -> i64 {
    val.to_bits() as i64
}

#[no_mangle]
pub unsafe extern fn java_lang_Double_longBitsToDouble(_: *mut MochaClass, val: i64) -> f64 {
    f64::from_bits(val as u64)
}
