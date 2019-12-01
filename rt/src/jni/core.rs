use crate::obj::{MochaClass, MochaObject};

#[no_mangle]
pub unsafe extern fn java_lang_Object_getClass(obj: *mut MochaObject) -> *mut MochaClass {
    obj.as_ref().unwrap().vtable().class_obj as *const MochaClass as *mut MochaClass
}
