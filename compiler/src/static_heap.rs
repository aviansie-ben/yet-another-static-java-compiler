use std::alloc::{AllocErr, AllocInit, AllocRef, Global, Layout};
use std::cell::{Cell, UnsafeCell};
use std::collections::hash_map::{self, HashMap};
use std::convert::TryInto;
use std::hash::{Hash, Hasher};
use std::fmt;
use std::ptr::NonNull;
use std::sync::Arc;

use bitflags::bitflags;
use itertools::Itertools;

use crate::classfile::{Class, ConstantPoolEntry, Field, FieldFlags, PrimitiveType};
use crate::layout;
use crate::resolve::{ClassEnvironment, ClassId, ConstantId, FieldId, ResolvedClass};
use crate::static_interp::Value;

bitflags! {
    pub struct ObjectFlags: u32 {
        const GC_MARKED = 0x00000001;
        const CLINIT_RUNNING = 0x00000002;
        const CLINIT_DONE = 0x00000004;
    }
}

pub const JAVA_LANG_CLASS_VTABLE_PTR_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_CLASS, 1);
pub const JAVA_LANG_CLASS_CANONICAL_NAME_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_CLASS, 2);

pub const JAVA_LANG_STRING_DATA_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_STRING, 0);

pub const JAVA_LANG_REFLECT_FIELD_CLAZZ_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_REFLECT_FIELD, 0);
pub const JAVA_LANG_REFLECT_FIELD_OFFSET_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_REFLECT_FIELD, 1);
pub const JAVA_LANG_REFLECT_FIELD_NAME_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_REFLECT_FIELD, 2);
pub const JAVA_LANG_REFLECT_FIELD_TYPE_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_REFLECT_FIELD, 3);
pub const JAVA_LANG_REFLECT_FIELD_MOD_FIELD: FieldId = FieldId(ClassId::JAVA_LANG_REFLECT_FIELD, 4);

pub fn collect_constant_strings<'a>(strings: impl IntoIterator<Item=ConstantId>, env: &mut ClassEnvironment) -> Vec<Arc<str>> {
    let mut strs = vec![];
    let mut str_map = HashMap::new();

    for string_id in strings {
        let cpe = match env.get_mut(string_id.0).as_user_class_mut().constant_pool[string_id.1 as usize] {
            ConstantPoolEntry::String(ref mut cpe) => cpe,
            _ => unreachable!()
        };

        cpe.index = match str_map.entry(cpe.contents.clone()) {
            hash_map::Entry::Occupied(entry) => *entry.get(),
            hash_map::Entry::Vacant(entry) => {
                let index = strs.len();

                entry.insert(index);
                strs.push(cpe.contents.clone());
                index
            }
        };
    };

    strs
}

pub trait GcRoot {
    type RefIterator: Iterator<Item=*mut u8>;

    fn iter(&self) -> Self::RefIterator;
}

pub struct GcRootSet {
    map: HashMap<NonNull<u8>, u32>
}

impl GcRootSet {
    pub fn new() -> GcRootSet {
        GcRootSet { map: HashMap::new() }
    }

    pub fn add_ref(&mut self, ptr: NonNull<u8>) {
        match self.map.entry(ptr) {
            hash_map::Entry::Occupied(mut entry) => {
                *entry.get_mut() += 1;
            },
            hash_map::Entry::Vacant(entry) => {
                entry.insert(1);
            }
        };
    }

    pub fn remove_ref(&mut self, ptr: NonNull<u8>) {
        let rc = self.map.get_mut(&ptr).unwrap();

        if *rc == 1 {
            self.map.remove(&ptr);
        } else {
            *rc -= 1;
        };
    }
}

pub struct JavaStaticRef<'a> {
    env: &'a ClassEnvironment,
    root: Option<NonNull<GcRootSet>>,
    ptr: NonNull<u8>
}

impl <'a> JavaStaticRef<'a> {
    pub unsafe fn from_raw(env: &'a ClassEnvironment, ptr: NonNull<u8>) -> JavaStaticRef<'a> {
        JavaStaticRef { env, root: None, ptr }
    }

    pub unsafe fn with_root(self, mut root: NonNull<GcRootSet>) -> JavaStaticRef<'a> {
        assert!(self.root.is_none());
        root.as_mut().add_ref(self.ptr);

        JavaStaticRef {
            env: self.env,
            root: Some(root),
            ptr: self.ptr
        }
    }

    pub unsafe fn read_raw<T: Copy>(&self, off: isize) -> T {
        (self.ptr.as_ptr().offset(off) as *mut T).read()
    }

    pub unsafe fn read_raw_ref(&self, off: isize) -> Option<JavaStaticRef<'a>> {
        let addr = self.read_raw::<u64>(off);

        if addr != 0 {
            Some(JavaStaticRef::from_raw(self.env, NonNull::new_unchecked(addr as *mut u8)))
        } else {
            None
        }
    }

    pub unsafe fn write_raw<T: Copy>(&self, off: isize, val: T) {
        (self.ptr.as_ptr().offset(off) as *mut T).write(val)
    }

    pub unsafe fn write_raw_ref(&self, off: isize, val: Option<JavaStaticRef<'a>>) {
        self.write_raw(off, val.map_or(0, |v| v.ptr.as_ptr() as u64));
    }

    pub fn is_subclass_of(&self, test_class: ClassId) -> bool {
        let mut class = self.class_id();

        while class != test_class && class != ClassId::UNRESOLVED {
            class = match **self.env.get(class) {
                ResolvedClass::User(ref class) => class.meta.super_id,
                _ => ClassId::UNRESOLVED
            };
        };

        class != ClassId::UNRESOLVED
    }

    pub unsafe fn read_array_length_unchecked(&self) -> i32 {
        self.read_raw::<i32>(8)
    }

    pub fn read_array_length(&self) -> i32 {
        match *self.class() {
            ResolvedClass::Array(_) => unsafe { self.read_array_length_unchecked() },
            _ => {
                panic!("Invalid read of array length on object of type {}", self.class().name(self.env));
            }
        }
    }

    pub fn calculate_array_element_offset<T: Copy>(idx: i32) -> isize {
        let elem_size = std::mem::size_of::<T>();
        let header_size = layout::get_array_header_size(elem_size.try_into().unwrap()) as usize;

        (header_size + (idx as usize) * elem_size) as isize
    }

    pub unsafe fn array_data<T: Copy>(&self) -> NonNull<T> {
        NonNull::new_unchecked(self.ptr.as_ptr().offset(JavaStaticRef::calculate_array_element_offset::<T>(0)) as *mut T)
    }

    pub unsafe fn read_array_element_raw<T: Copy>(&self, idx: i32) -> T {
        self.read_raw::<T>(JavaStaticRef::calculate_array_element_offset::<T>(idx))
    }

    pub unsafe fn read_array_element_raw_ref(&self, idx: i32) -> Option<JavaStaticRef<'a>> {
        self.read_raw_ref(JavaStaticRef::calculate_array_element_offset::<u64>(idx))
    }

    pub fn read_array_element(&self, idx: i32) -> Value<'a> {
        match *self.class() {
            ResolvedClass::Array(elem_class_id) => unsafe {
                assert!(idx >= 0 && idx < self.read_array_length_unchecked());

                match elem_class_id {
                    ClassId::PRIMITIVE_BYTE => {
                        Value::Int(self.read_array_element_raw::<i8>(idx) as i32)
                    },
                    ClassId::PRIMITIVE_CHAR => {
                        Value::Int(self.read_array_element_raw::<u16>(idx) as i32)
                    },
                    ClassId::PRIMITIVE_DOUBLE => {
                        Value::Double(self.read_array_element_raw::<u64>(idx))
                    },
                    ClassId::PRIMITIVE_FLOAT => {
                        Value::Float(self.read_array_element_raw::<u32>(idx))
                    },
                    ClassId::PRIMITIVE_INT => {
                        Value::Int(self.read_array_element_raw::<i32>(idx))
                    },
                    ClassId::PRIMITIVE_LONG => {
                        Value::Long(self.read_array_element_raw::<i64>(idx))
                    },
                    ClassId::PRIMITIVE_SHORT => {
                        Value::Int(self.read_array_element_raw::<i16>(idx) as i32)
                    },
                    ClassId::PRIMITIVE_BOOLEAN => {
                        Value::Int(self.read_array_element_raw::<u8>(idx) as i32)
                    },
                    _ => {
                        let java_ref = self.read_array_element_raw_ref(idx);

                        Value::Ref(if let Some(root) = self.root {
                            java_ref.map(|r| r.with_root(root))
                        } else {
                            java_ref
                        })
                    }
                }
            },
            _ => {
                panic!("Invalid read of array element on object of type {}", self.class().name(self.env));
            }
        }
    }

    pub fn read_field(&self, field_id: FieldId) -> Value<'a> {
        let (class, field) = self.env.get_field(field_id);

        if field.flags.contains(FieldFlags::STATIC) {
            let class_of = match self.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD) {
                Value::Int(val) => ClassId(val as u32),
                _ => unreachable!()
            };

            if class_of != class.meta.this_id {
                panic!("Invalid read of static field {:?} on java/lang/Class for type {}", field_id, self.env.get(class_of).name(self.env));
            };

            unsafe { self.read_field_unchecked(field) }
        } else {
            if !self.is_subclass_of(class.meta.this_id) {
                panic!("Invalid read of instance field {:?} on object of type {}", field_id, self.class().name(self.env));
            };

            unsafe { self.read_field_unchecked(field) }
        }
    }

    pub unsafe fn read_field_unchecked(&self, field: &Field) -> Value<'a> {
        if field.class_id == ClassId::UNRESOLVED {
            return Value::Ref(None);
        };

        match field.class_id {
            ClassId::PRIMITIVE_BYTE => {
                Value::Int(self.read_raw::<i8>(field.off as isize) as i32)
            },
            ClassId::PRIMITIVE_CHAR => {
                Value::Int(self.read_raw::<u16>(field.off as isize) as i32)
            },
            ClassId::PRIMITIVE_DOUBLE => {
                Value::Double(self.read_raw::<u64>(field.off as isize))
            },
            ClassId::PRIMITIVE_FLOAT => {
                Value::Float(self.read_raw::<u32>(field.off as isize))
            },
            ClassId::PRIMITIVE_INT => {
                Value::Int(self.read_raw::<i32>(field.off as isize))
            },
            ClassId::PRIMITIVE_LONG => {
                Value::Long(self.read_raw::<i64>(field.off as isize))
            },
            ClassId::PRIMITIVE_SHORT => {
                Value::Int(self.read_raw::<i16>(field.off as isize) as i32)
            },
            ClassId::PRIMITIVE_BOOLEAN => {
                Value::Int(self.read_raw::<i8>(field.off as isize) as i32)
            },
            _ => {
                let java_ref = self.read_raw_ref(field.off as isize);

                Value::Ref(if let Some(root) = self.root {
                    java_ref.map(|r| r.with_root(root))
                } else {
                    java_ref
                })
            }
        }
    }

    pub unsafe fn write_array_element_raw<T: Copy>(&self, idx: i32, val: T) {
        self.write_raw::<T>(JavaStaticRef::calculate_array_element_offset::<T>(idx), val)
    }

    pub unsafe fn write_array_element_raw_ref(&self, idx: i32, val: Option<JavaStaticRef<'a>>) {
        self.write_raw_ref(JavaStaticRef::calculate_array_element_offset::<u64>(idx), val)
    }

    pub fn write_array_element(&self, idx: i32, value: Value<'a>) {
        match *self.class() {
            ResolvedClass::Array(elem_class_id) => unsafe {
                assert!(idx >= 0 && idx < self.read_array_length_unchecked());

                match elem_class_id {
                    ClassId::PRIMITIVE_BYTE => {
                        self.write_array_element_raw(
                            idx,
                            value.as_int().expect("Invalid write of wrong type") as i8
                        );
                    },
                    ClassId::PRIMITIVE_CHAR => {
                        self.write_array_element_raw(
                            idx,
                            value.as_int().expect("Invalid write of wrong type") as u16
                        );
                    },
                    ClassId::PRIMITIVE_DOUBLE => {
                        self.write_array_element_raw(
                            idx,
                            value.as_double().expect("Invalid write of wrong type")
                        );
                    },
                    ClassId::PRIMITIVE_FLOAT => {
                        self.write_array_element_raw(
                            idx,
                            value.as_float().expect("Invalid write of wrong type")
                        );
                    },
                    ClassId::PRIMITIVE_INT => {
                        self.write_array_element_raw(
                            idx,
                            value.as_int().expect("Invalid write of wrong type")
                        );
                    },
                    ClassId::PRIMITIVE_LONG => {
                        self.write_array_element_raw(
                            idx,
                            value.as_long().expect("Invalid write of wrong type")
                        );
                    },
                    ClassId::PRIMITIVE_SHORT => {
                        self.write_array_element_raw(
                            idx,
                            value.as_int().expect("Invalid write of wrong type") as i16
                        );
                    },
                    ClassId::PRIMITIVE_BOOLEAN => {
                        self.write_array_element_raw(
                            idx,
                            if value.as_int().expect("Invalid write of wrong type") != 0 {
                                1i8
                            } else {
                                0i8
                            }
                        );
                    },
                    _ => {
                        self.write_array_element_raw_ref(
                            idx,
                            value.as_ref().expect("Invalid write of wrong type").map(|r| r.clone_untracked())
                        );
                    }
                }
            },
            _ => {
                panic!("Invalid read of array element on object of type {}", self.class().name(self.env));
            }
        }
    }

    pub fn write_field(&self, field_id: FieldId, value: Value<'a>) {
        let (class, field) = self.env.get_field(field_id);

        if field.flags.contains(FieldFlags::STATIC) {
            let class_of = match self.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD) {
                Value::Int(val) => ClassId(val as u32),
                _ => unreachable!()
            };

            if class_of != class.meta.this_id {
                panic!("Invalid write of static field {:?} on java/lang/Class for type {}", field_id, self.env.get(class_of).name(self.env));
            };

            unsafe {
                self.write_field_unchecked(field, value);
            };
        } else {
            if !self.is_subclass_of(class.meta.this_id) {
                panic!("Invalid write of instance field {:?} on object of type {}", field_id, self.class().name(self.env));
            };

            unsafe {
                self.write_field_unchecked(field, value)
            };
        };
    }

    pub unsafe fn write_field_unchecked(&self, field: &Field, value: Value<'a>) {
        if field.class_id == ClassId::UNRESOLVED {
            assert_eq!(value.as_ref(), Some(None));
            return;
        };

        match field.class_id {
            ClassId::PRIMITIVE_BYTE => {
                self.write_raw(
                    field.off as isize,
                    value.as_int().expect("Invalid write of wrong type") as i8
                );
            },
            ClassId::PRIMITIVE_CHAR => {
                self.write_raw(
                    field.off as isize,
                    value.as_int().expect("Invalid write of wrong type") as u16
                );
            },
            ClassId::PRIMITIVE_DOUBLE => {
                self.write_raw(
                    field.off as isize,
                    value.as_double().expect("Invalid write of wrong type")
                );
            },
            ClassId::PRIMITIVE_FLOAT => {
                self.write_raw(
                    field.off as isize,
                    value.as_float().expect("Invalid write of wrong type")
                );
            },
            ClassId::PRIMITIVE_INT => {
                self.write_raw(
                    field.off as isize,
                    value.as_int().expect("Invalid write of wrong type")
                );
            },
            ClassId::PRIMITIVE_LONG => {
                self.write_raw(
                    field.off as isize,
                    value.as_long().expect("Invalid write of wrong type")
                );
            },
            ClassId::PRIMITIVE_SHORT => {
                self.write_raw(
                    field.off as isize,
                    value.as_int().expect("Invalid write of wrong type") as i16
                );
            },
            ClassId::PRIMITIVE_BOOLEAN => {
                self.write_raw(
                    field.off as isize,
                    if value.as_int().expect("Invalid write of wrong type") != 0 {
                        1i8
                    } else {
                        0i8
                    }
                );
            },
            _ => {
                self.write_raw_ref(
                    field.off as isize,
                    value.as_ref().expect("Invalid write of wrong type").map(|r| r.clone_untracked())
                );
            }
        }
    }

    pub fn class_id(&self) -> ClassId {
        unsafe { ClassId(self.read_raw::<u32>(0)) }
    }

    pub fn class(&self) -> &'a ResolvedClass {
        self.env.get(self.class_id())
    }

    pub fn flags(&self) -> ObjectFlags {
        ObjectFlags::from_bits_truncate(unsafe { self.read_raw::<u32>(4) })
    }

    pub unsafe fn set_flags(&self, flags: ObjectFlags) {
        self.write_raw(4, (self.read_raw::<u32>(4) & !ObjectFlags::all().bits()) | flags.bits());
    }

    pub fn as_ptr(&self) -> NonNull<u8> {
        self.ptr
    }

    pub fn read_string(&self) -> String {
        if self.class_id() != ClassId::JAVA_LANG_STRING {
            panic!("Attempt to read non-string object of type {} as string", self.class().name(self.env));
        };

        unsafe {
            let arr_ref = self.read_field(JAVA_LANG_STRING_DATA_FIELD).into_ref().unwrap().unwrap();
            let arr_data = std::slice::from_raw_parts(arr_ref.array_data().as_ptr(), arr_ref.read_array_length_unchecked() as usize);

            String::from_utf16_lossy(arr_data)
        }
    }

    pub unsafe fn clone_untracked(&self) -> JavaStaticRef<'a> {
        JavaStaticRef::from_raw(self.env, self.ptr)
    }
}

impl <'a> fmt::Debug for JavaStaticRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "JavaStaticRef({:?})", self.ptr)
    }
}

impl <'a> fmt::Display for JavaStaticRef<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<{}@{:?}", self.class().name(self.env), self.ptr)?;
        match self.class_id() {
            ClassId::JAVA_LANG_CLASS => {
                write!(f, " (class: {})", self.read_field(JAVA_LANG_CLASS_CANONICAL_NAME_FIELD).as_ref().unwrap().unwrap().read_string())?;
            },
            ClassId::JAVA_LANG_STRING => {
                if self.read_field(JAVA_LANG_STRING_DATA_FIELD) == Value::Ref(None) {
                    write!(f, " (uninit)")?;
                } else {
                    write!(f, " {:?}", self.read_string())?;
                };
            },
            _ => match self.class() {
                ResolvedClass::Array(_) => {
                    write!(f, " (len: {})", self.read_array_length())?;
                },
                _ => {}
            }
        };
        write!(f, ">")?;

        Ok(())
    }
}

impl <'a> Clone for JavaStaticRef<'a> {
    fn clone(&self) -> Self {
        let java_ref = unsafe { self.clone_untracked() };

        if let Some(root) = self.root {
            unsafe { java_ref.with_root(root) }
        } else {
            java_ref
        }
    }
}

impl <'a> PartialEq for JavaStaticRef<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl <'a> Eq for JavaStaticRef<'a> {}

impl <'a> Hash for JavaStaticRef<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl <'a> Drop for JavaStaticRef<'a> {
    fn drop(&mut self) {
        if let Some(mut root) = self.root {
            unsafe { root.as_mut().remove_ref(self.ptr) };
        };
    }
}

#[derive(Debug)]
struct JavaStaticObject {
    bytes: NonNull<u8>,
    checkpoint: NonNull<u8>,
    size: usize,
    live_at_checkpoint: bool
}

impl JavaStaticObject {
    unsafe fn allocate_uninit(size: usize) -> Result<JavaStaticObject, AllocErr> {
        let bytes = Global.alloc(Layout::from_size_align(size, 16).unwrap(), AllocInit::Uninitialized)?.ptr;
        let checkpoint = match Global.alloc(Layout::from_size_align(size, 16).unwrap(), AllocInit::Uninitialized) {
            Result::Ok(checkpoint_block) => checkpoint_block.ptr,
            Result::Err(err) => {
                Global.dealloc(bytes, Layout::from_size_align(size, 16).unwrap());
                return Result::Err(err);
            }
        };

        Result::Ok(JavaStaticObject {
            bytes,
            checkpoint,
            size,
            live_at_checkpoint: false
        })
    }

    unsafe fn allocate_raw(class_id: ClassId, size: usize, env: &ClassEnvironment) -> Result<JavaStaticObject, AllocErr> {
        let obj = JavaStaticObject::allocate_uninit(size)?;

        obj.as_java_ref(env).write_raw::<u32>(0, class_id.0);
        obj.as_java_ref(env).write_raw::<u32>(4, 0);
        std::ptr::write_bytes(obj.bytes.as_ptr().offset(8), 0, size - 8);
        Result::Ok(obj)
    }

    fn allocate_object(class: &Class, env: &ClassEnvironment) -> Result<JavaStaticObject, AllocErr> {
        unsafe {
            JavaStaticObject::allocate_raw(class.meta.this_id, class.layout.size as usize, env)
        }
    }

    fn allocate_object_sized(class: &Class, env: &ClassEnvironment, size: usize) -> Result<JavaStaticObject, AllocErr> {
        unsafe {
            assert!(class.layout.size as usize <= size);
            JavaStaticObject::allocate_raw(class.meta.this_id, size, env)
        }
    }

    fn calculate_array_size(class_id: ClassId, env: &ClassEnvironment, len: u32) -> Option<usize> {
        let elem_class = match **env.get(class_id) {
            ResolvedClass::Array(elem_class_id) => elem_class_id,
            _ => unimplemented!()
        };

        let (elem_size, elem_align) = layout::get_field_size_align(env, elem_class);
        let header_size = layout::get_array_header_size(elem_align);

        (header_size as usize).checked_add((elem_size as usize).checked_mul(len as usize)?)
    }

    fn allocate_array(class_id: ClassId, env: &ClassEnvironment, len: u32) -> Result<JavaStaticObject, AllocErr> {
        let size = match JavaStaticObject::calculate_array_size(class_id, env, len) {
            Some(size) => size,
            None => {
                return Result::Err(AllocErr);
            }
        };

        unsafe {
            let obj = JavaStaticObject::allocate_raw(class_id, size, env)?;
            obj.as_java_ref(env).write_raw(8, len);

            Result::Ok(obj)
        }
    }

    unsafe fn as_java_ref<'a>(&self, env: &'a ClassEnvironment) -> JavaStaticRef<'a> {
        JavaStaticRef::from_raw(env, self.bytes)
    }
}

impl Drop for JavaStaticObject {
    fn drop(&mut self) {
        unsafe {
            Global.dealloc(self.bytes, Layout::from_size_align(self.size, 16).unwrap());
            Global.dealloc(self.checkpoint, Layout::from_size_align(self.size, 16).unwrap());
        }
    }
}

pub struct JavaStaticHeap<'a> {
    env: &'a ClassEnvironment,
    objs: UnsafeCell<Vec<JavaStaticObject>>,
    class_objs: HashMap<ClassId, JavaStaticRef<'a>>,
    constant_strings: Vec<JavaStaticRef<'a>>,
    ref_root: Box<UnsafeCell<GcRootSet>>,
    remaining_size: Cell<usize>
}

impl <'a> JavaStaticHeap<'a> {
    pub unsafe fn new(env: &'a ClassEnvironment, max_size: usize) -> JavaStaticHeap<'a> {
        JavaStaticHeap {
            env,
            objs: UnsafeCell::new(vec![]),
            class_objs: HashMap::new(),
            constant_strings: vec![],
            ref_root: Box::new(UnsafeCell::new(GcRootSet::new())),
            remaining_size: Cell::new(max_size)
        }
    }

    pub fn init_class_objects(&mut self, classes: impl Iterator<Item=ClassId>) -> Result<(), AllocErr> {
        for class_id in classes {
            let class_ref = unsafe { self.allocate_class_object_untracked(class_id)? };
            self.class_objs.insert(class_id, class_ref);
        };

        assert!(self.class_objs.contains_key(&ClassId::JAVA_LANG_CLASS));
        self.commit();

        Result::Ok(())
    }

    pub fn init_constant_strings<'b>(&mut self, strings: impl IntoIterator<Item=&'b str>) -> Result<(), AllocErr> {
        for str in strings.into_iter() {
            let str_ref = unsafe { self.allocate_string_untracked(str)? };
            self.constant_strings.push(str_ref);
        };

        Result::Ok(())
    }

    pub fn commit(&self) {
        unsafe {
            for obj in self.objs.get().as_mut().unwrap().iter_mut() {
                std::ptr::copy_nonoverlapping(obj.bytes.as_ptr(), obj.checkpoint.as_ptr(), obj.size);
                obj.live_at_checkpoint = true;
            };
        };
    }

    pub fn rollback(&self) {
        unsafe {
            self.objs.get().as_mut().unwrap().drain_filter(|obj| {
                if obj.live_at_checkpoint {
                    std::ptr::copy_nonoverlapping(obj.checkpoint.as_ptr(), obj.bytes.as_ptr(), obj.size);
                    false
                } else {
                    if self.ref_root.get().as_ref().unwrap().map.contains_key(&obj.bytes) {
                        panic!("Attempt to rollback with live references to non-checkpointed object {:?}", obj.bytes.as_ptr());
                    };
                    true
                }
            });
        };
    }

    pub fn all_objs(&self) -> Vec<JavaStaticRef<'a>> {
        unsafe {
            self.objs.get().as_ref().unwrap().iter().map(|o| {
                o.as_java_ref(self.env).with_root(NonNull::new_unchecked(self.ref_root.get()))
            }).collect()
        }
    }

    pub fn do_gc(&self) {
        // TODO
    }

    unsafe fn allocate_class_object_untracked(&self, class_id: ClassId) -> Result<JavaStaticRef<'a>, AllocErr> {
        let real_size = match **self.env.get(class_id) {
            ResolvedClass::User(ref class) => class.layout.static_size as usize,
            _ => self.env.get(ClassId::JAVA_LANG_CLASS).as_user_class().layout.size as usize
        };
        let size = (real_size + 15) / 16 * 16;

        if self.remaining_size.get() < size {
            self.do_gc();
            if self.remaining_size.get() < size {
                return Result::Err(AllocErr);
            };
        };

        let class_class = self.env.get(ClassId::JAVA_LANG_CLASS).as_user_class();

        let obj = match JavaStaticObject::allocate_object_sized(class_class, self.env, real_size) {
            Result::Ok(obj) => obj,
            Result::Err(_) => {
                self.do_gc();
                JavaStaticObject::allocate_object_sized(class_class, self.env, real_size)?
            }
        };
        self.remaining_size.set(self.remaining_size.get() - size);

        let java_ref = obj.as_java_ref(self.env);
        java_ref.write_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD, Value::Int(class_id.0 as i32));
        java_ref.write_field(
            JAVA_LANG_CLASS_CANONICAL_NAME_FIELD,
            Value::Ref(Some(self.allocate_string_untracked(&self.env.get(class_id).name(self.env))?))
        );

        self.objs.get().as_mut().unwrap().push(obj);
        Result::Ok(java_ref)
    }

    pub unsafe fn allocate_object_untracked(&self, class_id: ClassId) -> Result<JavaStaticRef<'a>, AllocErr> {
        let class = self.env.get(class_id).as_user_class();
        let size = (class.layout.size as usize + 15) / 16 * 16;

        if self.remaining_size.get() < size {
            self.do_gc();
            if self.remaining_size.get() < size {
                return Result::Err(AllocErr);
            };
        };

        let obj = match JavaStaticObject::allocate_object(class, self.env) {
            Result::Ok(obj) => obj,
            Result::Err(_) => {
                self.do_gc();
                JavaStaticObject::allocate_object(class, self.env)?
            }
        };
        let java_ref = obj.as_java_ref(self.env);

        self.remaining_size.set(self.remaining_size.get() - size);

        self.objs.get().as_mut().unwrap().push(obj);
        Result::Ok(java_ref)
    }

    pub fn allocate_object(&self, class_id: ClassId) -> Result<JavaStaticRef<'a>, AllocErr> {
        unsafe {
            self.allocate_object_untracked(class_id)
                .map(|r| r.with_root(NonNull::new_unchecked(self.ref_root.get())))
        }
    }

    pub unsafe fn allocate_string_untracked(&self, value: &str) -> Result<JavaStaticRef<'a>, AllocErr> {
        let value_utf16 = value.encode_utf16().collect_vec();

        assert!(value_utf16.len() <= i32::max_value() as usize);
        let array_ref = self.allocate_array_untracked(ClassId::PRIMITIVE_CHAR_ARRAY, value_utf16.len() as u32)?;
        let string_ref = self.allocate_object_untracked(ClassId::JAVA_LANG_STRING)?;

        if value_utf16.len() != 0 {
            std::ptr::copy_nonoverlapping(
                &value_utf16[0] as *const u16,
                array_ref.array_data::<u16>().as_ptr(),
                value_utf16.len()
            );
        };
        string_ref.write_field(JAVA_LANG_STRING_DATA_FIELD, Value::Ref(Some(array_ref)));

        Result::Ok(string_ref)
    }

    pub fn allocate_string(&self, value: &str) -> Result<JavaStaticRef<'a>, AllocErr> {
        unsafe {
            self.allocate_string_untracked(value)
                .map(|r| r.with_root(NonNull::new_unchecked(self.ref_root.get())))
        }
    }

    pub unsafe fn allocate_array_untracked(&self, class_id: ClassId, len: u32) -> Result<JavaStaticRef<'a>, AllocErr> {
        let size = match JavaStaticObject::calculate_array_size(class_id, self.env, len) {
            Some(size) => size,
            None => {
                return Result::Err(AllocErr);
            }
        };
        let size = (size + 15) / 16 * 16;

        if self.remaining_size.get() < size {
            self.do_gc();
            if self.remaining_size.get() < size {
                return Result::Err(AllocErr);
            }
        };

        let obj = match JavaStaticObject::allocate_array(class_id, self.env, len) {
            Result::Ok(obj) => obj,
            Result::Err(_) => {
                self.do_gc();
                JavaStaticObject::allocate_array(class_id, self.env, len)?
            }
        };
        let java_ref = obj.as_java_ref(self.env);

        self.remaining_size.set(self.remaining_size.get() - size);

        self.objs.get().as_mut().unwrap().push(obj);
        Result::Ok(java_ref)
    }

    pub fn allocate_array(&self, class_id: ClassId, len: u32) -> Result<JavaStaticRef<'a>, AllocErr> {
        unsafe {
            self.allocate_array_untracked(class_id, len)
                .map(|r| r.with_root(NonNull::new_unchecked(self.ref_root.get())))
        }
    }

    pub fn has_class_object(&self, class_id: ClassId) -> bool {
        self.class_objs.contains_key(&class_id)
    }

    pub unsafe fn get_class_object_untracked(&self, class_id: ClassId) -> JavaStaticRef<'a> {
        self.class_objs[&class_id].clone_untracked()
    }

    pub fn get_class_object(&self, class_id: ClassId) -> JavaStaticRef<'a> {
        unsafe {
            self.get_class_object_untracked(class_id)
                .with_root(NonNull::new_unchecked(self.ref_root.get()))
        }
    }

    pub unsafe fn get_constant_string_untracked(&self, index: usize) -> JavaStaticRef<'a> {
        self.constant_strings[index].clone_untracked()
    }

    pub fn get_constant_string(&self, index: usize) -> JavaStaticRef<'a> {
        unsafe {
            self.get_constant_string_untracked(index)
                .with_root(NonNull::new_unchecked(self.ref_root.get()))
        }
    }
}

impl <'a> Drop for JavaStaticHeap<'a> {
    fn drop(&mut self) {
        if !(unsafe { self.ref_root.get().as_mut().unwrap().map.is_empty() }) {
            // Leak memory that outstanding references might be pointing to before panicking,
            // otherwise dropping those references during a panic may result in use-after-free.
            std::mem::forget(std::mem::replace(&mut self.objs, UnsafeCell::new(vec![])));
            std::mem::forget(std::mem::replace(&mut self.ref_root, Box::new(UnsafeCell::new(GcRootSet::new()))));

            panic!("Attempt to drop JavaStaticHeap while tracked JavaStaticRefs exist");
        };
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::sync::Arc;

    use itertools::Itertools;
    use lazy_static::lazy_static;

    use crate::resolve;
    use crate::classfile::{ClassFlags, ClassMeta, ConstantClass, ConstantPoolEntry, Field, FieldFlags, TypeDescriptor};
    use crate::layout::{compute_all_layouts, ObjectLayout};
    use crate::liveness;
    use crate::test_util::TEST_ENV;

    #[test]
    fn test_allocate_empty_object() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let java_ref = heap.allocate_object(ClassId::JAVA_LANG_OBJECT).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 8);
        assert_eq!(heap.remaining_size.get(), 84);
        assert_eq!(java_ref.class_id(), ClassId::JAVA_LANG_OBJECT);
    }

    #[test]
    fn test_allocate_single_ref_object() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };
        let single_ref_class = TEST_ENV.try_find("mocha/$test/SingleRef").unwrap();

        let java_ref = heap.allocate_object(single_ref_class).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 16);
        assert_eq!(heap.remaining_size.get(), 84);
        assert_eq!(java_ref.class_id(), single_ref_class);
        assert_eq!(java_ref.read_field(FieldId(single_ref_class, 0)), Value::Ref(None));
    }

    #[test]
    fn test_allocate_empty_ref_array() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let java_ref = heap.allocate_array(ClassId::JAVA_LANG_OBJECT_ARRAY, 0).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 16);
        assert_eq!(heap.remaining_size.get(), 84);
        assert_eq!(java_ref.class_id(), ClassId::JAVA_LANG_OBJECT_ARRAY);
    }

    #[test]
    fn test_allocate_ref_array() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let java_ref = heap.allocate_array(ClassId::JAVA_LANG_OBJECT_ARRAY, 10).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 96);
        assert_eq!(heap.remaining_size.get(), 4);
        assert_eq!(java_ref.class_id(), ClassId::JAVA_LANG_OBJECT_ARRAY);
    }

    #[test]
    fn test_allocate_empty_byte_array() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let java_ref = heap.allocate_array(ClassId::PRIMITIVE_BYTE_ARRAY, 0).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 12);
        assert_eq!(heap.remaining_size.get(), 84);
        assert_eq!(java_ref.class_id(), ClassId::PRIMITIVE_BYTE_ARRAY);
    }

    #[test]
    fn test_allocate_byte_array() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let java_ref = heap.allocate_array(ClassId::PRIMITIVE_BYTE_ARRAY, 5).unwrap();
        let obj = unsafe { heap.objs.get().as_ref().unwrap().last().unwrap() };

        assert_eq!(obj.bytes, java_ref.ptr);
        assert_eq!(obj.size, 17);
        assert_eq!(heap.remaining_size.get(), 68);
        assert_eq!(java_ref.class_id(), ClassId::PRIMITIVE_BYTE_ARRAY);
    }

    #[test]
    fn test_allocate_object_is_tracked() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let obj = heap.allocate_object(ClassId::JAVA_LANG_OBJECT).unwrap();
        let ptr = obj.ptr;

        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&ptr) }, Some(&1));
        assert_eq!(obj.root, NonNull::new(heap.ref_root.get()));
        std::mem::drop(obj);
        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&ptr) }, None);
    }

    #[test]
    fn test_allocate_object_untracked_is_untracked() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let obj = unsafe { heap.allocate_object_untracked(ClassId::JAVA_LANG_OBJECT).unwrap() };
        let ptr = obj.ptr;

        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&ptr) }, None);
        assert_eq!(obj.root, None);
        std::mem::drop(obj);
        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&ptr) }, None);
    }

    #[test]
    fn test_read_write_ref() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };
        let single_ref_class = TEST_ENV.try_find("mocha/$test/SingleRef").unwrap();
        let ref_field = FieldId(single_ref_class, 0);

        let java_ref = heap.allocate_object(single_ref_class).unwrap();

        java_ref.write_field(ref_field, Value::Ref(Some(java_ref.clone())));
        assert_eq!(java_ref.read_field(ref_field), Value::Ref(Some(java_ref.clone())));

        java_ref.write_field(ref_field, Value::Ref(None));
        assert_eq!(java_ref.read_field(ref_field), Value::Ref(None));
    }

    #[test]
    fn test_allocate_string() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let string_ref = heap.allocate_string("abcd").unwrap();
        let array_ref = string_ref.read_field(JAVA_LANG_STRING_DATA_FIELD).as_ref().unwrap().unwrap().clone();

        assert_eq!(array_ref.read_array_length(), 4);
        assert_eq!(array_ref.read_array_element(0), Value::Int('a' as i32));
        assert_eq!(array_ref.read_array_element(1), Value::Int('b' as i32));
        assert_eq!(array_ref.read_array_element(2), Value::Int('c' as i32));
        assert_eq!(array_ref.read_array_element(3), Value::Int('d' as i32));
    }

    #[test]
    fn test_objects_do_not_overlap() {
        let mut heap = unsafe { JavaStaticHeap::new(&TEST_ENV, !0) };
        heap.init_class_objects(TEST_ENV.class_ids()).unwrap();

        let single_ref_class = TEST_ENV.try_find("mocha/$test/SingleRef").unwrap();

        for _ in 0..10 {
            heap.allocate_object(ClassId::JAVA_LANG_OBJECT).unwrap();
        };

        for _ in 0..10 {
            heap.allocate_object(single_ref_class).unwrap();
        }

        for _ in 0..10 {
            heap.allocate_array(ClassId::JAVA_LANG_OBJECT_ARRAY, 0).unwrap();
        };

        for _ in 0..10 {
            heap.allocate_array(ClassId::JAVA_LANG_OBJECT_ARRAY, 1).unwrap();
        };

        unsafe {
            let objs = heap.objs.get().as_ref().unwrap();

            for i in 0..39 {
                let o1 = &objs[i];

                for j in (i + 1)..40 {
                    let o2 = &objs[j];

                    let o1_ptr = o1.bytes.as_ptr();
                    let o2_ptr = o2.bytes.as_ptr();

                    assert!(o1_ptr < o2_ptr || o1_ptr >= o2_ptr.offset(o2.size as isize));
                    assert!(o2_ptr < o1_ptr || o2_ptr >= o1_ptr.offset(o1.size as isize));
                };
            };
        };
    }

    #[test]
    fn test_class_objects() {
        let mut heap = unsafe { JavaStaticHeap::new(&TEST_ENV, !0) };
        heap.init_class_objects(TEST_ENV.class_ids()).unwrap();

        for class_id in TEST_ENV.class_ids() {
            let class_obj = heap.get_class_object(class_id);

            assert_eq!(class_obj.class_id(), ClassId::JAVA_LANG_CLASS);
            assert_eq!(class_obj.read_field(JAVA_LANG_CLASS_VTABLE_PTR_FIELD), Value::Int(class_id.0 as i32));
        };

        let single_ref_static_class = TEST_ENV.try_find("mocha/$test/SingleRefStatic").unwrap();
        let class_obj = heap.get_class_object(single_ref_static_class);
        assert_eq!(class_obj.read_field(FieldId(single_ref_static_class, 0)), Value::Ref(None));
    }

    #[test]
    fn test_oom() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 0) };

        assert!(heap.allocate_object(ClassId::JAVA_LANG_OBJECT).is_err());
    }

    #[test]
    #[should_panic(expected = "Attempt to drop JavaStaticHeap while tracked JavaStaticRefs exist")]
    fn test_panic_on_leaked_ref() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        std::mem::forget(heap.allocate_object(ClassId::JAVA_LANG_OBJECT).unwrap());
        std::mem::drop(heap);
    }

    #[test]
    fn test_clone_tracked_ref() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let r1 = heap.allocate_object(ClassId::JAVA_LANG_OBJECT).unwrap();
        let r2 = r1.clone();

        assert_eq!(r1.ptr, r2.ptr);
        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&r1.ptr) }, Some(&2));
    }

    #[test]
    fn test_clone_untracked_ref() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };

        let r1 = unsafe { heap.allocate_object_untracked(ClassId::JAVA_LANG_OBJECT).unwrap() };
        let r2 = r1.clone();

        assert_eq!(r1.ptr, r2.ptr);
        assert_eq!(unsafe { heap.ref_root.get().as_mut().unwrap().map.get(&r1.ptr) }, None);
    }

    #[test]
    fn test_rollback_modify() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };
        let single_ref_class = TEST_ENV.try_find("mocha/$test/SingleRef").unwrap();
        let ref_field = FieldId(single_ref_class, 0);

        let java_ref = heap.allocate_object(single_ref_class).unwrap();
        heap.commit();

        java_ref.write_field(ref_field, Value::Ref(Some(java_ref.clone())));
        assert_eq!(java_ref.read_field(ref_field), Value::Ref(Some(java_ref.clone())));

        heap.rollback();
        assert_eq!(java_ref.read_field(ref_field), Value::Ref(None));

        java_ref.write_field(ref_field, Value::Ref(Some(java_ref.clone())));
        heap.commit();
        java_ref.write_field(ref_field, Value::Ref(None));
        heap.rollback();
        assert_eq!(java_ref.read_field(ref_field), Value::Ref(Some(java_ref.clone())));
    }

    #[test]
    fn test_rollback_allocate() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };
        let single_ref_class = TEST_ENV.try_find("mocha/$test/SingleRef").unwrap();

        heap.commit();
        heap.allocate_object(single_ref_class).unwrap();
        heap.rollback();

        assert!(unsafe { heap.objs.get().as_ref().unwrap().is_empty() });
    }

    #[test]
    #[should_panic(expected = "Attempt to rollback with live references to non-checkpointed object")]
    fn test_panic_on_rollback_with_dead_tracked_ref() {
        let heap = unsafe { JavaStaticHeap::new(&TEST_ENV, 100) };
        heap.commit();

        let java_ref = heap.allocate_object(ClassId::JAVA_LANG_OBJECT);
        heap.rollback();
    }
}
