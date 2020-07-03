use crate::obj::MochaClass;

#[no_mangle]
pub unsafe extern fn java_lang_StrictMath_log(_: *mut MochaClass, val: f64) -> f64 {
    val.ln()
}
