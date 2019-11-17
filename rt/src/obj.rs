use std::cell::Cell;

#[repr(C)]
pub struct MochaVTable {
    pub class_obj: &'static MochaClass,
    pub itable: *mut (),
    pub vslots: [fn () -> (); 0]
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
            &*(self.vtable as usize as *mut MochaVTable)
        }
    }
}

#[repr(C)]
pub struct MochaClass {
    pub obj: MochaObject
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
