use crate::obj::{MochaClass, MochaString};

#[no_mangle]
pub unsafe extern fn org_mocha_test_Output_writeString(_: *mut MochaClass, msg: *mut MochaString) {
    print!("{}", msg.as_ref().unwrap().as_string());
}

#[no_mangle]
pub unsafe extern fn org_mocha_test_Output_writeInt(_: *mut MochaClass, val: i32) {
    print!("{}", val);
}
