use crate::obj::MochaObject;

#[no_mangle]
pub unsafe extern fn mocha_throw(exc: *mut MochaObject) -> ! {
    panic!("Threw instance of {}",
        exc.as_ref().map_or_else(
            || String::from("java/lang/NullPointerException"),
            |exc| exc.vtable().class_obj.canonical_name.as_ref().unwrap().as_string()
        )
    );
}
