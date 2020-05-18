use crate::obj::{MochaAnyArray, MochaClass, MochaVTable};

#[no_mangle]
pub unsafe extern fn java_lang_reflect_Array_newArray(_: *mut MochaClass, elem_class: *mut MochaClass, len: i32) -> *mut MochaAnyArray {
    let elem_vtable = MochaVTable::from_compressed(elem_class.as_ref().unwrap().vtable);

    if elem_vtable.array_vtable == 0 {
        panic!("Attempt to allocate array of {} without loaded array class");
    };

    MochaAnyArray::allocate(MochaVTable::from_compressed(elem_vtable.array_vtable), len)
}

#[no_mangle]
pub unsafe extern fn java_lang_Class_isPrimitive0(_: *mut MochaClass, vtable: u32) -> i32 {
    if MochaVTable::from_compressed(vtable).is_primitive() {
        1
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern fn java_lang_Class_isArray0(_: *mut MochaClass, vtable: u32) -> i32 {
    if MochaVTable::from_compressed(vtable).is_array() {
        1
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern fn java_lang_Class_getComponentType0(_: *mut MochaClass, vtable: u32) -> u32 {
    MochaVTable::from_compressed(vtable).array_component_type().as_compressed()
}

#[no_mangle]
pub unsafe extern fn java_lang_Class_getClassForVTable(_: *mut MochaClass, vtable: u32) -> *mut MochaClass {
    MochaVTable::from_compressed(vtable).class_obj as *const MochaClass as *mut MochaClass
}
