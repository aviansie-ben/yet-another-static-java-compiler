use std::sync::atomic::{AtomicI32, AtomicI64, AtomicPtr, Ordering};

use crate::obj::MochaObject;

unsafe fn unsafe_get<T: Copy>(obj: *mut MochaObject, off: u64) -> T {
    ((obj as *mut u8).add(off as usize) as *mut T).read()
}

unsafe fn unsafe_put<T: Copy>(obj: *mut MochaObject, off: u64, val: T) {
    ((obj as *mut u8).add(off as usize) as *mut T).write(val)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getBoolean(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i32 {
    unsafe_get::<i8>(obj, off) as i32
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getByte(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i32 {
    unsafe_get::<i8>(obj, off) as i32
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getShort(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i32 {
    unsafe_get::<i16>(obj, off) as i32
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getChar(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i32 {
    unsafe_get::<u16>(obj, off) as i32
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getInt(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i32 {
    unsafe_get::<i32>(obj, off)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getLong(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> i64 {
    unsafe_get::<i64>(obj, off)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getFloat(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> f32 {
    unsafe_get::<f32>(obj, off)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getDouble(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> f64 {
    unsafe_get::<f64>(obj, off)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_getObject(_: *mut MochaObject, obj: *mut MochaObject, off: u64) -> *mut MochaObject {
    unsafe_get::<*mut MochaObject>(obj, off)
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putBoolean(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i32) {
    unsafe_put(obj, off, val as i8);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putByte(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i32) {
    unsafe_put(obj, off, val as i8);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putShort(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i32) {
    unsafe_put(obj, off, val as i16);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putChar(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i32) {
    unsafe_put(obj, off, val as u16);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putInt(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i32) {
    unsafe_put(obj, off, val);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putLong(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: i64) {
    unsafe_put(obj, off, val);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putFloat(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: f32) {
    unsafe_put(obj, off, val);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_putDouble(_: *mut MochaObject, obj: *mut MochaObject, off: u64, val: f64) {
    unsafe_put(obj, off, val);
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_compareAndSwapInt(_: *mut MochaObject, obj: *mut MochaObject, off: u64, expected: i32, val: i32) -> i32 {
    if (*((obj as *mut u8).add(off as usize) as *mut AtomicI32)).compare_and_swap(expected, val, Ordering::SeqCst) == expected {
        1
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_compareAndSwapLong(_: *mut MochaObject, obj: *mut MochaObject, off: u64, expected: i64, val: i64) -> i32 {
    if (*((obj as *mut u8).add(off as usize) as *mut AtomicI64)).compare_and_swap(expected, val, Ordering::SeqCst) == expected {
        1
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern fn sun_misc_Unsafe_compareAndSwapObject(_: *mut MochaObject, obj: *mut MochaObject, off: u64, expected: *mut MochaObject, val: *mut MochaObject) -> i32 {
    if (*((obj as *mut u8).add(off as usize) as *mut AtomicPtr<MochaObject>)).compare_and_swap(expected, val, Ordering::SeqCst) == expected {
        1
    } else {
        0
    }
}
