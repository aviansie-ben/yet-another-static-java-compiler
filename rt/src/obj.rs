use std::alloc::{AllocInit, AllocRef, Global, Layout};
use std::cell::Cell;

#[repr(C)]
pub struct MochaVTable {
    pub obj_size: u32,
    pub depth: u16,
    pub flags: u16,
    pub modifiers: u16,
    pub num_interfaces: u16,
    pub array_vtable: u32,
    pub type_specific_info: u64,
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

    pub fn is_primitive(&self) -> bool {
        (self.flags & MochaVTable::FLAG_PRIMITIVE) == MochaVTable::FLAG_PRIMITIVE
    }

    pub fn field_size(&self) -> usize {
        if self.is_primitive() {
            match self.type_specific_info as u8 {
                b'B' => 1,
                b'C' => 2,
                b'D' => 8,
                b'F' => 4,
                b'I' => 4,
                b'J' => 8,
                b'S' => 2,
                b'Z' => 1,
                ty => panic!("Unknown primitive type {:?}", char::from(ty))
            }
        } else {
            8
        }
    }

    pub fn is_array(&self) -> bool {
        (self.flags & MochaVTable::FLAG_ARRAY) == MochaVTable::FLAG_ARRAY
    }

    pub fn array_component_type(&self) -> &'static MochaVTable {
        assert!(self.is_array());
        unsafe { MochaVTable::from_compressed(self.type_specific_info as u32) }
    }

    pub fn array_element_size(&self) -> usize {
        self.array_component_type().field_size()
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

    pub fn as_array(&mut self) -> Option<&mut MochaAnyArray> {
        if (self.vtable().flags & MochaVTable::FLAG_ARRAY) == MochaVTable::FLAG_ARRAY {
            Some(unsafe { &mut *(self as *mut MochaObject as *mut MochaAnyArray) })
        } else {
            None
        }
    }

    pub fn allocate(vtable: &'static MochaVTable) -> *mut MochaObject {
        unsafe {
            assert!(vtable.obj_size > 0);
            let ptr = match Global.alloc(Layout::from_size_align_unchecked(vtable.obj_size as usize, 16), AllocInit::Uninitialized) {
                Ok(block) => block.ptr,
                Err(_) => panic!("Out of memory")
            };

            std::ptr::write_bytes(ptr.as_ptr(), 0, vtable.obj_size as usize);

            let obj = ptr.cast::<MochaObject>().as_ptr().as_mut().unwrap();
            obj.vtable = vtable.as_compressed();

            obj as *mut MochaObject
        }
    }
}

#[repr(C)]
pub struct MochaClass {
    pub obj: MochaObject,
    pub class_value_map: *mut MochaObject,
    pub vtable: u32,
    pub canonical_name: *mut MochaString
}

#[repr(C)]
pub struct MochaAnyArray {
    pub obj: MochaObject,
    pub len: u32,
    pub arr_data: [u8; 0]
}

impl MochaAnyArray {
    pub fn allocate(vtable: &'static MochaVTable, len: i32) -> *mut MochaAnyArray {
        unsafe {
            assert!(len >= 0);
            assert!(vtable.obj_size > 0);

            let real_size = vtable.array_element_size().checked_mul(len as usize)
                .and_then(|data_size| (vtable.obj_size as usize).checked_add(data_size))
                .expect("Array size overflow");

            let ptr = match Global.alloc(Layout::from_size_align_unchecked(real_size, 16), AllocInit::Uninitialized) {
                Ok(block) => block.ptr,
                Err(_) => panic!("Out of memory")
            };

            std::ptr::write_bytes(ptr.as_ptr(), 0, real_size);

            let arr = ptr.cast::<MochaAnyArray>().as_ptr().as_mut().unwrap();
            arr.obj.vtable = vtable.as_compressed();
            arr.len = len as u32;

            arr as *mut MochaAnyArray
        }
    }
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
pub unsafe extern fn mocha_alloc_obj(vtable: *const MochaVTable) -> *mut MochaObject {
    MochaObject::allocate(vtable.as_ref().unwrap())
}

#[no_mangle]
pub unsafe extern fn mocha_alloc_array(vtable: *const MochaVTable, len: i32) -> *mut MochaAnyArray {
    MochaAnyArray::allocate(vtable.as_ref().unwrap(), len)
}
