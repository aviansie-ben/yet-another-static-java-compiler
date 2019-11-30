use std::convert::TryFrom;

use crate::obj::{MochaClass, MochaObject};

#[no_mangle]
pub unsafe extern fn java_lang_System_arraycopy(_: *mut MochaClass, src: *mut MochaObject, src_pos: i32, dst: *mut MochaObject, dst_pos: i32, len: i32) {
    let src_pos = u32::try_from(src_pos).unwrap() as usize;
    let dst_pos = u32::try_from(dst_pos).unwrap() as usize;
    let len = u32::try_from(len).unwrap() as usize;

    let src = src.as_mut().unwrap().as_array().unwrap();
    let dst = dst.as_mut().unwrap().as_array().unwrap();

    let src_vtable = src.obj.vtable();
    let dst_vtable = dst.obj.vtable();

    assert_eq!(src_vtable.array_element_size(), dst_vtable.array_element_size());
    assert!(src_pos < (src.len as usize) && len <= ((src.len as usize) - src_pos));
    assert!(dst_pos < (dst.len as usize) && len <= ((dst.len as usize) - dst_pos));

    let src = src.arr_data.as_mut_ptr();
    let dst = dst.arr_data.as_mut_ptr();

    match src_vtable.array_element_size() {
        1 => {
            std::ptr::copy(src.add(src_pos), dst.add(dst_pos), len as usize)
        },
        2 => {
            std::ptr::copy((src as *mut u16).add(src_pos), (dst as *mut u16).add(dst_pos), len as usize);
        },
        4 => {
            std::ptr::copy((src as *mut u32).add(src_pos), (dst as *mut u32).add(dst_pos), len as usize);
        },
        8 => {
            std::ptr::copy((src as *mut u64).add(src_pos), (dst as *mut u64).add(dst_pos), len as usize);
        },
        _ => unreachable!()
    };
}
