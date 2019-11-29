use std::alloc::{Alloc, Global, Layout};
use std::cell::Cell;

#[repr(C)]
pub struct MochaVTable {
    pub obj_size: u32,
    pub depth: u16,
    pub flags: u16,
    pub modifiers: u16,
    pub num_interfaces: u16,
    pub type_specific_info: u32,
    pub super_vtables: *const u32,
    pub class_obj: &'static MochaClass,
    pub itable: *mut (),
    pub vslots: [fn () -> (); 0]
}

impl MochaVTable {
    pub const FLAG_PRIMITIVE: u16 = 0x0001;
    pub const FLAG_ARRAY: u16 = 0x0002;
    pub const FLAG_INTERFACE: u16 = 0x0004;

    pub unsafe fn from_compressed(ptr: u32) -> &'static MochaVTable {
        assert!(ptr > 8);
        &*((ptr - 8) as usize as *mut MochaVTable)
    }

    pub fn as_compressed(&'static self) -> u32 {
        assert!((self as *const MochaVTable as usize) < 0xfffffff8);

        (self as *const MochaVTable as usize as u32) + 8
    }
}

#[repr(C)]
pub struct MochaObject {
    pub vtable: u32,
    pub flags: u32
}

impl MochaObject {
    pub fn class(&self) -> &'static MochaClass {
        self.vtable().class_obj
    }

    pub fn vtable(&self) -> &'static MochaVTable {
        unsafe {
            MochaVTable::from_compressed(self.vtable)
        }
    }
}

#[repr(C)]
pub struct MochaClass {
    pub obj: MochaObject,
    pub class_value_map: *mut MochaObject,
    pub vtable: u32
}

#[repr(C)]
pub struct MochaCharArray {
    pub obj: MochaObject,
    pub len: u32,
    pub arr_data: [u16; 0]
}

impl MochaCharArray {
    pub fn data_cell(&self) -> &[Cell<u16>] {
        unsafe {
            std::slice::from_raw_parts(self.arr_data.as_ptr() as *const Cell<u16>, self.len as usize)
        }
    }

    pub unsafe fn data(&self) -> &[u16] {
        std::slice::from_raw_parts(self.arr_data.as_ptr(), self.len as usize)
    }

    pub unsafe fn data_mut(&self) -> &mut [u16] {
        std::slice::from_raw_parts_mut(self.arr_data.as_ptr() as *mut u16, self.len as usize)
    }
}

#[repr(C)]
pub struct MochaString {
    pub obj: MochaObject,
    pub chars: *mut MochaCharArray
}

impl MochaString {
    pub fn as_string(&self) -> String {
        String::from_utf16_lossy(unsafe { self.chars.as_ref().unwrap().data() })
    }
}

#[no_mangle]
pub unsafe extern fn mocha_alloc_obj(vtable: *mut MochaVTable) -> *mut MochaObject {
    let vtable = vtable.as_ref().unwrap();

    assert!(vtable.obj_size > 0);
    let ptr = match Global.alloc(Layout::from_size_align_unchecked(vtable.obj_size as usize, 16)) {
        Ok(ptr) => ptr,
        Err(_) => panic!("Out of memory")
    };

    std::ptr::write_bytes(ptr.as_ptr(), 0, vtable.obj_size as usize);

    let obj = ptr.cast::<MochaObject>().as_ptr().as_mut().unwrap();
    obj.vtable = vtable.as_compressed();

    obj as *mut MochaObject
}
