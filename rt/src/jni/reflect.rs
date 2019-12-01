use crate::obj::{MochaAnyArray, MochaClass, MochaVTable};

#[no_mangle]
pub unsafe extern fn java_lang_reflect_Array_newArray(_: *mut MochaClass, elem_class: *mut MochaClass, len: i32) -> *mut MochaAnyArray {
    let elem_vtable = MochaVTable::from_compressed(elem_class.as_ref().unwrap().vtable);

    if elem_vtable.array_vtable == 0 {
        panic!("Attempt to allocate array of {} without loaded array class");
    };

    MochaAnyArray::allocate(MochaVTable::from_compressed(elem_vtable.array_vtable), len)
}
