use std::collections::{HashMap, HashSet, VecDeque};
use std::collections::hash_map;
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::{BufReader, Read, Seek};
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use itertools::Itertools;
use lazy_static::lazy_static;
use smallvec::SmallVec;
use zip::ZipArchive;
use zip::result::{ZipError, ZipResult};

use crate::bytecode::{BytecodeInstruction, BytecodeIterator};
use crate::classfile::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ClassId(pub u32);

impl ClassId {
    pub const UNRESOLVED: ClassId = ClassId(!0);

    pub const PRIMITIVE_VOID: ClassId = ClassId(0);
    pub const PRIMITIVE_BYTE: ClassId = ClassId(1);
    pub const PRIMITIVE_CHAR: ClassId = ClassId(2);
    pub const PRIMITIVE_DOUBLE: ClassId = ClassId(3);
    pub const PRIMITIVE_FLOAT: ClassId = ClassId(4);
    pub const PRIMITIVE_INT: ClassId = ClassId(5);
    pub const PRIMITIVE_LONG: ClassId = ClassId(6);
    pub const PRIMITIVE_SHORT: ClassId = ClassId(7);
    pub const PRIMITIVE_BOOLEAN: ClassId = ClassId(8);

    pub const PRIMITIVE_BYTE_ARRAY: ClassId = ClassId(9);
    pub const PRIMITIVE_CHAR_ARRAY: ClassId = ClassId(10);
    pub const PRIMITIVE_DOUBLE_ARRAY: ClassId = ClassId(11);
    pub const PRIMITIVE_FLOAT_ARRAY: ClassId = ClassId(12);
    pub const PRIMITIVE_INT_ARRAY: ClassId = ClassId(13);
    pub const PRIMITIVE_LONG_ARRAY: ClassId = ClassId(14);
    pub const PRIMITIVE_SHORT_ARRAY: ClassId = ClassId(15);
    pub const PRIMITIVE_BOOLEAN_ARRAY: ClassId = ClassId(16);

    pub const JAVA_LANG_OBJECT: ClassId = ClassId(17);
    pub const JAVA_LANG_INVOKE_METHODHANDLE: ClassId = ClassId(18);
    pub const JAVA_LANG_STRING: ClassId = ClassId(19);
    pub const JAVA_LANG_CLASS: ClassId = ClassId(20);
    pub const JAVA_LANG_REFLECT_FIELD: ClassId = ClassId(21);

    pub const JAVA_LANG_OBJECT_ARRAY: ClassId = ClassId(22);
    pub const JAVA_LANG_REFLECT_FIELD_ARRAY: ClassId = ClassId(23);

    pub fn for_primitive_type(t: PrimitiveType) -> ClassId {
        match t {
            PrimitiveType::Byte => ClassId::PRIMITIVE_BYTE,
            PrimitiveType::Char => ClassId::PRIMITIVE_CHAR,
            PrimitiveType::Double => ClassId::PRIMITIVE_DOUBLE,
            PrimitiveType::Float => ClassId::PRIMITIVE_FLOAT,
            PrimitiveType::Int => ClassId::PRIMITIVE_INT,
            PrimitiveType::Long => ClassId::PRIMITIVE_LONG,
            PrimitiveType::Short => ClassId::PRIMITIVE_SHORT,
            PrimitiveType::Boolean => ClassId::PRIMITIVE_BOOLEAN
        }
    }

    pub fn for_primitive_type_array(t: PrimitiveType) -> ClassId {
        match t {
            PrimitiveType::Byte => ClassId::PRIMITIVE_BYTE_ARRAY,
            PrimitiveType::Char => ClassId::PRIMITIVE_CHAR_ARRAY,
            PrimitiveType::Double => ClassId::PRIMITIVE_DOUBLE_ARRAY,
            PrimitiveType::Float => ClassId::PRIMITIVE_FLOAT_ARRAY,
            PrimitiveType::Int => ClassId::PRIMITIVE_INT_ARRAY,
            PrimitiveType::Long => ClassId::PRIMITIVE_LONG_ARRAY,
            PrimitiveType::Short => ClassId::PRIMITIVE_SHORT_ARRAY,
            PrimitiveType::Boolean => ClassId::PRIMITIVE_BOOLEAN_ARRAY
        }
    }

    pub fn num_special_classes() -> u32 {
        24
    }

    pub fn special_classes() -> impl Iterator<Item=ClassId> {
        (0..ClassId::num_special_classes()).map(|i| ClassId(i))
    }

    pub fn needs_dual_slot(&self) -> bool {
        match *self {
            ClassId::PRIMITIVE_DOUBLE => true,
            ClassId::PRIMITIVE_LONG => true,
            _ => false
        }
    }

    pub fn is_primitive_type(&self) -> bool {
        match *self {
            ClassId::PRIMITIVE_VOID => true,
            ClassId::PRIMITIVE_BYTE => true,
            ClassId::PRIMITIVE_CHAR => true,
            ClassId::PRIMITIVE_DOUBLE => true,
            ClassId::PRIMITIVE_FLOAT => true,
            ClassId::PRIMITIVE_INT => true,
            ClassId::PRIMITIVE_LONG => true,
            ClassId::PRIMITIVE_SHORT => true,
            ClassId::PRIMITIVE_BOOLEAN => true,
            _ => false
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MethodId(pub ClassId, pub u16);

impl MethodId {
    pub const UNRESOLVED: MethodId = MethodId(ClassId::UNRESOLVED, !0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FieldId(pub ClassId, pub u16);

impl FieldId {
    pub const UNRESOLVED: FieldId = FieldId(ClassId::UNRESOLVED, !0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ConstantId(pub ClassId, pub u16);

pub trait ClassLoader: Debug + Send + Sync {
    fn try_load(
        &self,
        name: &str,
        resolve: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>
    ) -> Option<Result<ResolvedClass, ClassResolveError>>;
}

#[derive(Debug, Clone)]
pub struct ArrayClassLoader();

impl ClassLoader for ArrayClassLoader {
    fn try_load(
        &self,
        name: &str,
        resolve: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>
    ) -> Option<Result<ResolvedClass, ClassResolveError>> {
        if name.starts_with("[") {
            if let Some(descriptor) = TypeDescriptor::parse(name) {
                Some(try {
                    ResolvedClass::Array(if descriptor.array_dims > 1 {
                        resolve(&name[1..])?
                    } else {
                        match descriptor.flat {
                            FlatTypeDescriptor::Primitive(t) => ClassId::for_primitive_type(t),
                            FlatTypeDescriptor::Reference(ref elem_name) => resolve(elem_name)?
                        }
                    })
                })
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct FileClassLoader {
    path: PathBuf
}

impl FileClassLoader {
    pub fn new(path: PathBuf) -> FileClassLoader {
        FileClassLoader { path }
    }
}

impl ClassLoader for FileClassLoader {
    fn try_load(
        &self,
        name: &str,
        _: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>
    ) -> Option<Result<ResolvedClass, ClassResolveError>> {
        let path = self.path.join(Path::new(name)).with_extension("class");

        if path.is_file() {
            match File::open(path) {
                Result::Ok(file) => Some(
                    parse_class_file(&mut BufReader::new(file))
                        .map(|cls| ResolvedClass::User(cls))
                        .map_err(|err| ClassResolveError::ReadError(err))
                ),
                Result::Err(err) => Some(Result::Err(ClassResolveError::ReadError(ClassFileReadError::Io(err))))
            }
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct JarClassLoader<R: Read + Seek + Debug + Send + Sync>(Mutex<ZipArchive<R>>);

impl <R: Read + Seek + Debug + Send + Sync> JarClassLoader<R> {
    pub fn new(read: R) -> ZipResult<JarClassLoader<R>> {
        Result::Ok(JarClassLoader(Mutex::new(ZipArchive::new(read)?)))
    }
}

impl <R: Read + Seek + Debug + Send + Sync> ClassLoader for JarClassLoader<R> {
    fn try_load(
        &self,
        name: &str,
        _: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>
    ) -> Option<Result<ResolvedClass, ClassResolveError>> {
        let name = format!("{}.class", name);
        let mut archive = self.0.lock().unwrap();
        let file = archive.by_name(&name);

        match file {
            Result::Ok(file) if file.is_file() => Some(
                parse_class_file(&mut BufReader::new(file))
                    .map(|cls| ResolvedClass::User(cls))
                    .map_err(|err| ClassResolveError::ReadError(err))
            ),
            Result::Ok(_) => None,
            Result::Err(ZipError::FileNotFound) => None,
            Result::Err(ZipError::Io(err)) => Some(Result::Err(ClassResolveError::ReadError(ClassFileReadError::Io(err)))),
            Result::Err(ZipError::InvalidArchive(_)) => unreachable!(),
            Result::Err(ZipError::UnsupportedArchive(_)) => unreachable!()
        }
    }
}

#[derive(Debug)]
pub enum ClassResolveError {
    ReadError(ClassFileReadError),
    NoSuchClass(String),
    NoSuchField(ClassId, String, TypeDescriptor),
    NoSuchMethod(ClassId, String, MethodDescriptor),
    TooManyClasses,
    WhileResolvingClass(ClassId, Box<ClassResolveError>)
}

#[derive(Debug, Clone)]
pub enum ResolvedClass {
    User(Class),
    Primitive(Option<PrimitiveType>),
    Array(ClassId)
}

impl ResolvedClass {
    pub fn name(&self, env: &ClassEnvironment) -> String {
        let mut name = String::new();

        match *self {
            ResolvedClass::User(ref class) => {
                write!(name, "{}", class.meta.name).unwrap();
            },
            ResolvedClass::Primitive(None) => {
                write!(name, "void").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Byte)) => {
                write!(name, "byte").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Char)) => {
                write!(name, "char").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Double)) => {
                write!(name, "double").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Float)) => {
                write!(name, "float").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Int)) => {
                write!(name, "int").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Long)) => {
                write!(name, "long").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Short)) => {
                write!(name, "short").unwrap();
            },
            ResolvedClass::Primitive(Some(PrimitiveType::Boolean)) => {
                write!(name, "boolean").unwrap();
            },
            ResolvedClass::Array(mut inner_id) => {
                write!(name, "[").unwrap();
                loop {
                    match **env.get(inner_id) {
                        ResolvedClass::User(ref class) => {
                            write!(name, "L{};", class.meta.name).unwrap();
                            break;
                        },
                        ResolvedClass::Primitive(None) => {
                            write!(name, "void").unwrap();
                            break;
                        },
                        ResolvedClass::Primitive(Some(t)) => {
                            write!(name, "{}", t).unwrap();
                            break;
                        },
                        ResolvedClass::Array(elem) => {
                            write!(name, "[").unwrap();
                            inner_id = elem;
                        }
                    };
                };
            }
        };

        name
    }

    pub fn as_user_class(&self) -> &Class {
        if let ResolvedClass::User(ref class) = *self {
            class
        } else {
            unreachable!();
        }
    }

    pub fn as_user_class_mut(&mut self) -> &mut Class {
        if let ResolvedClass::User(ref mut class) = *self {
            class
        } else {
            unreachable!();
        }
    }
}

#[derive(Debug)]
struct ClassEnvironmentInternals {
    classes: Vec<Box<ResolvedClass>>,
    class_names: HashMap<String, ClassId>
}

impl ClassEnvironmentInternals {
    fn add_unnamed_class(&mut self, mut class: Box<ResolvedClass>) -> ClassId {
        assert!(self.classes.len() < (u32::max_value() - 1) as usize);

        let id = ClassId(self.classes.len() as u32);

        if let ResolvedClass::User(ref mut class) = *class {
            class.meta.this_id = id;

            if let ConstantPoolEntry::Class(ref mut cp_entry) = class.constant_pool[class.this_class_cp as usize] {
                cp_entry.class_id = id;
            } else {
                unreachable!();
            };
        };

        self.classes.push(class);
        id
    }

    fn add_class(&mut self, name: &str, mut class: Box<ResolvedClass>) -> ClassId {
        if let ResolvedClass::User(ref mut class) = *class {
            class.meta.name = Arc::from(name.to_owned().into_boxed_str());
        };

        let id = self.add_unnamed_class(class);
        self.class_names.insert(name.to_owned(), id);
        id
    }
}

#[derive(Debug)]
pub struct ClassEnvironment {
    class_loaders: Vec<Box<dyn ClassLoader>>,
    internals: ClassEnvironmentInternals
}

impl ClassEnvironment {
    pub fn new(class_loaders: Vec<Box<dyn ClassLoader>>) -> ClassEnvironment {
        ClassEnvironment {
            class_loaders,
            internals: ClassEnvironmentInternals {
                classes: vec![],
                class_names: HashMap::new()
            }
        }
    }

    pub fn load_bootstrap_classes(&mut self) -> Result<(), ClassResolveError> {
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(None))),
            ClassId::PRIMITIVE_VOID
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Byte)))),
            ClassId::PRIMITIVE_BYTE
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Char)))),
            ClassId::PRIMITIVE_CHAR
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Double)))),
            ClassId::PRIMITIVE_DOUBLE
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Float)))),
            ClassId::PRIMITIVE_FLOAT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Int)))),
            ClassId::PRIMITIVE_INT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Long)))),
            ClassId::PRIMITIVE_LONG
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Short)))),
            ClassId::PRIMITIVE_SHORT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(Some(PrimitiveType::Boolean)))),
            ClassId::PRIMITIVE_BOOLEAN
        );

        assert_eq!(
            self.load("[B")?,
            ClassId::PRIMITIVE_BYTE_ARRAY
        );
        assert_eq!(
            self.load("[C")?,
            ClassId::PRIMITIVE_CHAR_ARRAY
        );
        assert_eq!(
            self.load("[D")?,
            ClassId::PRIMITIVE_DOUBLE_ARRAY
        );
        assert_eq!(
            self.load("[F")?,
            ClassId::PRIMITIVE_FLOAT_ARRAY
        );
        assert_eq!(
            self.load("[I")?,
            ClassId::PRIMITIVE_INT_ARRAY
        );
        assert_eq!(
            self.load("[J")?,
            ClassId::PRIMITIVE_LONG_ARRAY
        );
        assert_eq!(
            self.load("[S")?,
            ClassId::PRIMITIVE_SHORT_ARRAY
        );
        assert_eq!(
            self.load("[Z")?,
            ClassId::PRIMITIVE_BOOLEAN_ARRAY
        );

        assert_eq!(
            self.load("java/lang/Object")?,
            ClassId::JAVA_LANG_OBJECT
        );
        assert_eq!(
            self.load("java/lang/invoke/MethodHandle")?,
            ClassId::JAVA_LANG_INVOKE_METHODHANDLE
        );
        assert_eq!(
            self.load("java/lang/String")?,
            ClassId::JAVA_LANG_STRING
        );
        assert_eq!(
            self.load("java/lang/Class")?,
            ClassId::JAVA_LANG_CLASS
        );
        assert_eq!(
            self.load("java/lang/reflect/Field")?,
            ClassId::JAVA_LANG_REFLECT_FIELD
        );

        assert_eq!(
            self.load("[Ljava/lang/Object;")?,
            ClassId::JAVA_LANG_OBJECT_ARRAY
        );
        assert_eq!(
            self.load("[Ljava/lang/reflect/Field;")?,
            ClassId::JAVA_LANG_REFLECT_FIELD_ARRAY
        );

        Result::Ok(())
    }

    fn load(&mut self, name: &str) -> Result<ClassId, ClassResolveError> {
        fn do_load(name: &str, class_loaders: &[Box<dyn ClassLoader>], internals: &mut ClassEnvironmentInternals) -> Result<ClassId, ClassResolveError> {
            if internals.classes.len() >= (u32::max_value() - 1) as usize {
                return Result::Err(ClassResolveError::TooManyClasses);
            };

            let class: ResolvedClass = class_loaders.iter().map(|cl| {
                cl.try_load(name, &mut |name| {
                    do_find_or_load(name, class_loaders, internals)
                })
            }).filter_map(|result| result).next().unwrap_or_else(
                || Result::Err(ClassResolveError::NoSuchClass(name.to_owned()))
            )?;

            Result::Ok(internals.add_class(name, Box::new(class)))
        }

        fn do_find_or_load(name: &str, class_loaders: &[Box<dyn ClassLoader>], internals: &mut ClassEnvironmentInternals) -> Result<ClassId, ClassResolveError> {
            if let Some(&existing_id) = internals.class_names.get(name) {
                Result::Ok(existing_id)
            } else {
                do_load(name, class_loaders, internals)
            }
        }

        do_load(name, &self.class_loaders, &mut self.internals)
    }

    pub fn get(&self, id: ClassId) -> &Box<ResolvedClass> {
        &self.internals.classes[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: ClassId) -> &mut Box<ResolvedClass> {
        &mut self.internals.classes[id.0 as usize]
    }

    pub fn get_method(&self, id: MethodId) -> (&Class, &Method) {
        let class = self.get(id.0).as_user_class();
        (class, &class.methods[id.1 as usize])
    }

    pub fn get_field(&self, id: FieldId) -> (&Class, &Field) {
        let class = self.get(id.0).as_user_class();
        (class, &class.fields[id.1 as usize])
    }

    pub fn can_convert(&self, from: ClassId, to: ClassId) -> bool {
        if from == to {
            return true;
        };

        match (&**self.get(from), &**self.get(to)) {
            (&ResolvedClass::Array(_), _) if to == ClassId::JAVA_LANG_OBJECT => true,
            // TODO Array interfaces?
            (&ResolvedClass::Array(from_component), &ResolvedClass::Array(to_component)) => self.can_convert(from_component, to_component),
            (&ResolvedClass::User(ref from_class), &ResolvedClass::User(_)) => {
                if from_class.meta.all_interface_ids.contains(&to) {
                    true
                } else if from_class.meta.super_id != ClassId::UNRESOLVED {
                    self.can_convert(from_class.meta.super_id, to)
                } else {
                    false
                }
            },
            _ => false
        }
    }

    pub fn try_find(&self, name: &str) -> Option<ClassId> {
        self.internals.class_names.get(name).map(|&id| id)
    }

    pub fn try_find_for_descriptor(&self, ty: &TypeDescriptor) -> Option<ClassId> {
        if ty.array_dims > 0 {
            self.try_find(&format!("{}", ty))
        } else {
            match ty.flat {
                FlatTypeDescriptor::Primitive(ty) => Some(ClassId::for_primitive_type(ty)),
                FlatTypeDescriptor::Reference(ref name) => self.try_find(name)
            }
        }
    }

    pub fn try_find_array(&self, elem_id: ClassId) -> Option<ClassId> {
        self.try_find(&self.array_name(elem_id))
    }

    pub fn find_or_load(&mut self, name: &str) -> Result<ClassId, ClassResolveError> {
        if let Some(id) = self.try_find(name) {
            Result::Ok(id)
        } else {
            self.load(name)
        }
    }

    fn array_name(&self, elem_id: ClassId) -> String {
        match **self.get(elem_id) {
            ResolvedClass::User(ref class) => {
                format!("[L{};", class.meta.name)
            },
            ResolvedClass::Array(_) => {
                format!("[{}", self.get(elem_id).name(self))
            },
            ResolvedClass::Primitive(None) => {
                format!("[void")
            },
            ResolvedClass::Primitive(Some(ty)) => {
                format!("[{}", ty)
            }
        }
    }

    pub fn find_or_load_array(&mut self, elem_id: ClassId) -> Result<ClassId, ClassResolveError> {
        self.find_or_load(&self.array_name(elem_id))
    }

    pub fn force_set_class(&mut self, name: &str, class: ResolvedClass) -> ClassId {
        assert!(!self.internals.class_names.contains_key(name));
        self.internals.add_class(name, Box::new(class))
    }

    pub fn num_classes(&self) -> usize {
        self.internals.classes.len()
    }

    pub fn num_user_classes(&self) -> usize {
        self.internals.classes.iter().filter(|&c| {
            match **c {
                ResolvedClass::User(_) => true,
                _ => false
            }
        }).count()
    }

    pub fn get_class_chain(&self, mut id: ClassId) -> SmallVec<[ClassId; 4]> {
        let mut chain = SmallVec::new();

        while id != ClassId::UNRESOLVED {
            chain.push(id);
            id = match **self.get(id) {
                ResolvedClass::Primitive(_) => ClassId::UNRESOLVED,
                ResolvedClass::User(ref class) => class.meta.super_id,
                ResolvedClass::Array(_) => ClassId::JAVA_LANG_OBJECT
            };
        };

        chain
    }

    pub fn class_ids(&self) -> impl Iterator<Item=ClassId> {
        (0..(self.num_classes())).map(|i| ClassId(i as u32))
    }
}

fn get_constant_pool_class(constant_pool: &[ConstantPoolEntry], i: u16) -> ClassId {
    if i == 0 {
        ClassId::UNRESOLVED
    } else if let ConstantPoolEntry::Class(ref cpe) = constant_pool[i as usize] {
        cpe.class_id
    } else {
        unreachable!()
    }
}

pub fn resolve_all_classes(env: &mut ClassEnvironment, verbose: bool) -> Result<(), ClassResolveError> {
    let mut resolving_class = Box::new(Class::dummy_class());
    let mut worklist = VecDeque::new();
    let mut not_found = HashSet::new();

    for id in env.class_ids() {
        worklist.push_back(id);
    };

    while let Some(resolving_id) = worklist.pop_front() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            // The name of the dummy class must be set correctly in case we try to load an array of
            // the current class due to an anewarray opcode.
            resolving_class.meta.name = class.meta.name.clone();

            mem::swap(class, &mut resolving_class);

            if verbose {
                eprintln!("Resolving classes from {}...", resolving_class.meta.name);
            };
        } else {
            continue;
        };

        for cpe in resolving_class.constant_pool.iter_mut() {
            if let ConstantPoolEntry::Class(ref mut cpe) = *cpe {
                if cpe.class_id == ClassId::UNRESOLVED && !not_found.contains(&cpe.name) {
                    cpe.class_id = if let Some(resolved_id) = env.try_find(&cpe.name) {
                        if verbose {
                            eprintln!("    Already loaded {}", cpe.name);
                        };
                        resolved_id
                    } else {
                        if verbose {
                            eprintln!("    Loading {}...", cpe.name);
                        };

                        let old_num_classes = env.num_classes();
                        match env.load(&cpe.name) {
                            Result::Ok(resolved_id) => {
                                for loaded_id in old_num_classes..env.num_classes() {
                                    worklist.push_back(ClassId(loaded_id as u32))
                                };
                                resolved_id
                            },
                            Result::Err(ClassResolveError::NoSuchClass(_)) => {
                                eprintln!("WARNING: Unable to find class {}", cpe.name);
                                not_found.insert(cpe.name.clone());
                                continue;
                            },
                            Result::Err(err) => {
                                if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
                                    mem::swap(class, &mut resolving_class);
                                } else {
                                    unreachable!();
                                };

                                return Result::Err(ClassResolveError::WhileResolvingClass(resolving_id, Box::new(err)));
                            }
                        }
                    };
                };
            };
        };

        resolving_class.meta.super_id = get_constant_pool_class(&resolving_class.constant_pool, resolving_class.super_class_cp);
        resolving_class.meta.interface_ids = resolving_class.interfaces.iter().map(|&i| {
            get_constant_pool_class(&resolving_class.constant_pool, i)
        }).collect();

        for m in resolving_class.methods.iter_mut() {
            if let Some(instrs) = BytecodeIterator::for_method(m) {
                for (_, instr) in instrs {
                    match instr.unwrap() {
                        BytecodeInstruction::ANewArray(cpe) => {
                            let cpe = match resolving_class.constant_pool[cpe as usize] {
                                ConstantPoolEntry::Class(ref mut cpe) => cpe,
                                _ => unreachable!()
                            };

                            if cpe.class_id != ClassId::UNRESOLVED && cpe.array_class_id == ClassId::UNRESOLVED {
                                cpe.array_class_id = env.find_or_load_array(cpe.class_id).unwrap();
                            };
                        },
                        _ => {}
                    };
                };
            };
        };

        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
        } else {
            unreachable!();
        };
    };

    Result::Ok(())
}

fn find_with_super<T, U>(
    env: &ClassEnvironment,
    class_id: ClassId,
    include_interfaces: bool,
    resolving_t: &[T],
    resolving_meta: &ClassMeta,
    get_class_t: &impl Fn (&Class) -> &[T],
    find_t: &impl Fn(ClassId, &[T]) -> Option<U>
) -> Option<U> {
    if class_id == ClassId::UNRESOLVED {
        return None;
    };

    let (t, meta) = if class_id == resolving_meta.this_id {
        (resolving_t, resolving_meta)
    } else {
        match **env.get(class_id) {
            ResolvedClass::User(ref class) => (get_class_t(class), &class.meta),
            ResolvedClass::Primitive(_) => {
                return None;
            },
            ResolvedClass::Array(_) => {
                return find_with_super(
                    env,
                    ClassId::JAVA_LANG_OBJECT,
                    include_interfaces,
                    resolving_t,
                    resolving_meta,
                    get_class_t,
                    find_t
                );
            }
        }
    };

    find_t(class_id, t).or_else(|| {
        let handle_super = |id| {
            if id != ClassId::UNRESOLVED {
                find_with_super(env, id, include_interfaces, resolving_t, resolving_meta, get_class_t, find_t)
            } else {
                None
            }
        };

        if include_interfaces {
            itertools::repeat_n(meta.super_id, 1)
                .chain(meta.interface_ids.iter().copied())
                .filter_map(handle_super)
                .next()
        } else {
            handle_super(meta.super_id)
        }
    })
}

fn find_field(
    env: &ClassEnvironment,
    class_id: ClassId,
    resolving_fields: &[Field],
    resolving_meta: &ClassMeta,
    fieldref: &ConstantFieldref
) -> FieldId {
    find_with_super(
        env,
        class_id,
        true,
        resolving_fields,
        resolving_meta,
        &|class| &class.fields,
        &|class_id, fields| {
            fields.iter().enumerate()
                .filter(|(_, f)| f.name == fieldref.name && f.descriptor == fieldref.descriptor)
                .next()
                .map(|(i, _)| FieldId(class_id, i as u16))
        }
    ).unwrap_or(FieldId::UNRESOLVED)
}

lazy_static! {
    static ref METHODHANDLE_INVOKE_DESCRIPTOR: MethodDescriptor = MethodDescriptor {
        return_type: Some(TypeDescriptor {
            array_dims: 0,
            flat: FlatTypeDescriptor::Reference(Arc::from(String::from("java/lang/Object").into_boxed_str()))
        }),
        param_types: vec![
            TypeDescriptor {
                array_dims: 1,
                flat: FlatTypeDescriptor::Reference(Arc::from(String::from("java/lang/Object").into_boxed_str()))
            }
        ]
    };
}

fn is_signature_polymorphic(class_id: ClassId, method: &Method) -> bool {
    class_id == ClassId::JAVA_LANG_INVOKE_METHODHANDLE
        && method.descriptor == *METHODHANDLE_INVOKE_DESCRIPTOR
        && method.flags.contains(MethodFlags::VARARGS | MethodFlags::NATIVE)
}

fn is_compatible_method(
    class_id: ClassId,
    method: &Method,
    methodref: &ConstantMethodref
) -> bool {
    method.name == methodref.name && (method.descriptor == methodref.descriptor || is_signature_polymorphic(class_id, method))
}

fn find_method(
    env: &ClassEnvironment,
    class_id: ClassId,
    include_interfaces: bool,
    resolving_methods: &[Method],
    resolving_meta: &ClassMeta,
    methodref: &ConstantMethodref
) -> MethodId {
    find_with_super(
        env,
        class_id,
        include_interfaces,
        resolving_methods,
        resolving_meta,
        &|class| &class.methods,
        &|class_id, methods| {
            methods.iter().enumerate()
                .filter(|(_, m)| is_compatible_method(class_id, m, methodref))
                .next()
                .map(|(i, _)| MethodId(class_id, i as u16))
        }
    ).unwrap_or(MethodId::UNRESOLVED)
}

pub fn resolve_all_subitem_references(env: &mut ClassEnvironment, verbose: bool) -> Result<(), ClassResolveError> {
    let mut resolving_class = Box::new(Class::dummy_class());
    let mut class_ids = vec![];

    for resolving_id in env.class_ids() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);

            if verbose {
                eprintln!("Resolving subitems from {}...", resolving_class.meta.name);
            };
        } else {
            continue;
        };

        class_ids.clear();
        class_ids.extend(resolving_class.constant_pool.iter().map(|cpe| {
            if let ConstantPoolEntry::Class(ref cpe) = *cpe {
                cpe.class_id
            } else {
                ClassId::UNRESOLVED
            }
        }));

        for cpe in resolving_class.constant_pool.iter_mut() {
            match *cpe {
                ConstantPoolEntry::Fieldref(ref mut cpe) => {
                    let class_id = class_ids[cpe.class as usize];

                    cpe.field_id = if class_id != ClassId::UNRESOLVED {
                        let field_id = find_field(env, class_id, &resolving_class.fields, &resolving_class.meta, cpe);

                        if field_id == FieldId::UNRESOLVED {
                            let err = ClassResolveError::NoSuchField(
                                class_id,
                                String::from(cpe.name.as_ref()),
                                cpe.descriptor.clone()
                            );

                            if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
                                mem::swap(class, &mut resolving_class);
                            } else {
                                unreachable!();
                            };

                            return Result::Err(ClassResolveError::WhileResolvingClass(resolving_id, Box::new(err)));
                        };

                        field_id
                    } else {
                        FieldId::UNRESOLVED
                    };
                    cpe.type_id = env.try_find_for_descriptor(&cpe.descriptor).unwrap_or(ClassId::UNRESOLVED);

                    if verbose {
                        eprintln!("    Resolved field {} {} as {:?}", cpe.name, cpe.descriptor, cpe.field_id);
                    };
                },
                ConstantPoolEntry::Methodref(ref mut cpe) | ConstantPoolEntry::InterfaceMethodref(ref mut cpe) => {
                    let class_id = class_ids[cpe.class as usize];

                    cpe.method_id = if class_id != ClassId::UNRESOLVED {
                        let method_id = find_method(env, class_id, true, &resolving_class.methods, &resolving_class.meta, cpe);

                        if method_id == MethodId::UNRESOLVED {
                            let err = ClassResolveError::NoSuchMethod(
                                class_id,
                                String::from(cpe.name.as_ref()),
                                cpe.descriptor.clone()
                            );

                            if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
                                mem::swap(class, &mut resolving_class);
                            } else {
                                unreachable!();
                            };

                            return Result::Err(ClassResolveError::WhileResolvingClass(resolving_id, Box::new(err)));
                        };

                        method_id
                    } else {
                        MethodId::UNRESOLVED
                    };

                    if verbose {
                        eprintln!("    Resolved method {}{} as {:?}", cpe.name, cpe.descriptor, cpe.method_id);
                    };
                }
                _ => {}
            };
        };

        for f in resolving_class.fields.iter_mut() {
            f.class_id = env.try_find_for_descriptor(&f.descriptor).unwrap_or(ClassId::UNRESOLVED);
        };

        for m in resolving_class.methods.iter_mut() {
            if !m.flags.contains(MethodFlags::STATIC) {
                m.param_types.push(resolving_class.meta.this_id);
            };

            m.param_types.extend(m.descriptor.param_types.iter().map(|d| {
                env.try_find_for_descriptor(d).unwrap_or(ClassId::UNRESOLVED)
            }));

            m.return_type = m.descriptor.return_type.as_ref().map_or(
                ClassId::PRIMITIVE_VOID,
                |d| env.try_find_for_descriptor(d).unwrap_or(ClassId::UNRESOLVED)
            );

            for attr in m.attributes.iter_mut() {
                match attr.data {
                    AttributeData::Code(ref mut code) => {
                        for attr in code.attributes.iter_mut() {
                            match attr.data {
                                AttributeData::LocalVariableTable(ref mut table) => {
                                    for entry in table.iter_mut() {
                                        entry.class_id = env.try_find_for_descriptor(&entry.signature).unwrap_or(ClassId::UNRESOLVED);
                                    };
                                },
                                _ => {}
                            };
                        };
                    },
                    _ => {}
                };
            };
        };

        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
        } else {
            unreachable!();
        };
    };

    Result::Ok(())
}

pub fn expand_interface(env: &ClassEnvironment, all_interface_ids: &mut Vec<ClassId>, interface_id: ClassId) {
    if interface_id == ClassId::UNRESOLVED || all_interface_ids.contains(&interface_id) {
        return;
    };

    all_interface_ids.push(interface_id);
    for interface_id in env.get(interface_id).as_user_class().meta.interface_ids.iter().cloned() {
        expand_interface(env, all_interface_ids, interface_id);
    };
}

pub fn expand_super_interfaces(env: &ClassEnvironment, all_interface_ids: &mut Vec<ClassId>, super_id: ClassId) {
    if super_id == ClassId::UNRESOLVED {
        return;
    };

    let super_class = match **env.get(super_id) {
        ResolvedClass::User(ref class) => class,
        _ => {
            return;
        }
    };

    for interface_id in super_class.meta.interface_ids.iter().cloned() {
        expand_interface(env, all_interface_ids, interface_id);
    };
    expand_super_interfaces(env, all_interface_ids, super_class.meta.super_id);
}

pub fn resolve_overriding(env: &mut ClassEnvironment, verbose: bool) -> Result<(), ClassResolveError> {
    let mut resolving_class = Box::new(Class::dummy_class());
    let mut overridden_by: HashMap<MethodId, Vec<MethodId>> = HashMap::new();

    // PASS 1: Resolve overriding based only on methods explicitly declared in each class
    for resolving_id in env.class_ids() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);

            if verbose {
                eprintln!("Resolving direct overriding from {}...", resolving_class.meta.name);
            };
        } else {
            continue;
        };

        for interface_id in resolving_class.meta.interface_ids.iter().cloned() {
            expand_interface(env, &mut resolving_class.meta.all_interface_ids, interface_id);
        };
        expand_super_interfaces(env, &mut resolving_class.meta.all_interface_ids, resolving_class.meta.super_id);

        let resolving_meta = &resolving_class.meta;

        for (i, m) in resolving_class.methods.iter_mut().enumerate() {
            if m.flags.intersects(MethodFlags::STATIC | MethodFlags::PRIVATE) || m.name.as_ref() == "<init>" {
                continue;
            };

            let method_id = MethodId(resolving_class.meta.this_id, i as u16);

            if verbose {
                eprintln!("    {}{}:", m.name, m.descriptor);
            };

            let methodref = ConstantMethodref {
                class: 0,
                name: m.name.clone(),
                descriptor: m.descriptor.clone(),
                method_id: MethodId::UNRESOLVED
            };

            m.overrides.overrides_virtual = find_method(
                env,
                resolving_class.meta.super_id,
                false,
                &[],
                resolving_meta,
                &methodref
            );

            if m.overrides.overrides_virtual != MethodId::UNRESOLVED && env.get(m.overrides.overrides_virtual.0).as_user_class().flags.contains(ClassFlags::INTERFACE) {
                m.overrides.overrides_virtual = MethodId::UNRESOLVED;
            };

            m.overrides.overrides_interface = resolving_class.meta.all_interface_ids.iter().cloned()
                .filter_map(|interface| {
                    let method = find_method(env, interface, true, &[], resolving_meta, &methodref);
                    if method != MethodId::UNRESOLVED {
                        Some(method)
                    } else {
                        None
                    }
                })
                .collect();

            if m.overrides.overrides_virtual != MethodId::UNRESOLVED {
                match overridden_by.entry(m.overrides.overrides_virtual) {
                    hash_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().push(method_id);
                    },
                    hash_map::Entry::Vacant(entry) => {
                        entry.insert(vec![method_id]);
                    }
                };
            };

            for &interface_method in m.overrides.overrides_interface.iter() {
                match overridden_by.entry(interface_method) {
                    hash_map::Entry::Occupied(mut entry) => {
                        entry.get_mut().push(method_id);
                    },
                    hash_map::Entry::Vacant(entry) => {
                        entry.insert(vec![method_id]);
                    }
                };
            };

            if verbose {
                if m.overrides.overrides_virtual != MethodId::UNRESOLVED {
                    eprintln!("        Virtual override for {}", env.get(m.overrides.overrides_virtual.0).name(env));
                } else {
                    eprintln!("        Not a virtual override");
                };

                if !m.overrides.overrides_interface.is_empty() {
                    eprint!("        Interface override for:");
                    for interface_method in m.overrides.overrides_interface.iter() {
                        eprint!(" {}", env.get(interface_method.0).name(env));
                    };
                    eprintln!();
                } else {
                    eprintln!("        Does not override any interfaces");
                };
            };
        };

        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
        } else {
            unreachable!();
        };
    };

    // PASS 2: Resolve overriding caused by implementing new interfaces in a subclass. e.g. Class A has a method a()V which does not
    //         override any interface methods, and subclass B implements an interface I having a method a()V using the method inherited
    //         from class A.
    for resolving_id in env.class_ids() {
        let class = if let ResolvedClass::User(ref class) = **env.get(resolving_id) {
            class
        } else {
            continue;
        };

        if class.meta.super_id == ClassId::UNRESOLVED {
            continue;
        };

        let super_class = env.get(class.meta.super_id).as_user_class();
        let new_interfaces = class.meta.all_interface_ids.iter().copied()
            .filter(|&iface_id| !super_class.meta.all_interface_ids.contains(&iface_id))
            .collect_vec();

        if new_interfaces.is_empty() {
            continue;
        };

        if verbose {
            eprintln!("Resolving indirect overriding from {}...", class.meta.name);
        };

        let mut super_chain = vec![super_class];
        while super_chain.last().unwrap().meta.super_id != ClassId::UNRESOLVED {
            super_chain.push(env.get(super_chain.last().unwrap().meta.super_id).as_user_class());
        };

        let mut extra_interface_overrides = vec![];

        for interface_id in new_interfaces {
            let interface_class = env.get(interface_id).as_user_class();

            for (i, m) in interface_class.methods.iter().enumerate() {
                if m.flags.intersects(MethodFlags::STATIC) {
                    continue;
                };

                let method_id = MethodId(interface_class.meta.this_id, i as u16);

                if overridden_by.get(&method_id).map_or(false, |overridden_by| overridden_by.iter().copied().any(|m| m.0 == resolving_id)) {
                    continue;
                };

                if verbose {
                    eprint!("    {}.{}{}: ", interface_class.meta.name, m.name, m.descriptor);
                };

                let mut overrider = None;

                for super_class in super_chain.iter().copied() {
                    for (i, sm) in super_class.methods.iter().enumerate() {
                        if sm.name == m.name && sm.descriptor == m.descriptor {
                            overrider = Some(MethodId(super_class.meta.this_id, i as u16));
                        };
                    };
                };

                if let Some(overrider) = overrider {
                    if verbose {
                        eprintln!("Overriden by {}", env.get(overrider.0).name(env));
                    };

                    extra_interface_overrides.push((method_id, overrider));

                    match overridden_by.entry(method_id) {
                        hash_map::Entry::Occupied(mut entry) => {
                            entry.get_mut().push(overrider);
                        },
                        hash_map::Entry::Vacant(entry) => {
                            entry.insert(vec![overrider]);
                        }
                    };
                } else {
                    if verbose {
                        eprintln!("Not overridden");
                    };

                    if m.flags.intersects(MethodFlags::ABSTRACT) && !class.flags.intersects(ClassFlags::ABSTRACT) {
                        eprintln!("WARNING: Class {} implements interface {} without overriding {}{}", class.meta.name, interface_class.meta.name, m.name, m.descriptor);
                    };
                };
            };
        };

        env.get_mut(resolving_id).as_user_class_mut().meta.extra_interface_overrides = extra_interface_overrides;
    };

    let mut worklist: VecDeque<_> = overridden_by.keys().cloned().collect();
    let mut inherited_overrides = vec![];

    // PASS 3: Propagate information about chained overrides. e.g. Class A has virtual method a()V, which is overridden by subclass B, which
    //         is in turn overridden by subclass C, which means that C.a()V should override A.a()V.
    while let Some(resolving_id) = worklist.pop_front() {
        for overrider_id in overridden_by[&resolving_id].iter() {
            if let Some(overridden_by) = overridden_by.get(overrider_id) {
                inherited_overrides.extend(overridden_by.iter().cloned());
            };
        };

        let overridden_by = overridden_by.get_mut(&resolving_id).unwrap();

        let mut changed = false;
        for overrider_id in inherited_overrides.drain(..) {
            if !overridden_by.contains(&overrider_id) {
                overridden_by.push(overrider_id);
                changed = true;
            };
        };

        if changed {
            let method = if let ResolvedClass::User(ref class) = **env.get(resolving_id.0) {
                &class.methods[resolving_id.1 as usize]
            } else {
                unreachable!();
            };

            if method.overrides.overrides_virtual != MethodId::UNRESOLVED {
                if !worklist.contains(&method.overrides.overrides_virtual) {
                    worklist.push_back(method.overrides.overrides_virtual);
                };
            };

            for interface_method in method.overrides.overrides_interface.iter().cloned() {
                if !worklist.contains(&interface_method) {
                    worklist.push_back(interface_method);
                };
            };
        };
    };

    // PASS 4: Write information about overriders of each method
    for resolving_id in env.class_ids() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);

            if verbose {
                eprintln!("Resolving overriding of {}...", resolving_class.meta.name);
            };
        } else {
            continue;
        };

        for (i, m) in resolving_class.methods.iter_mut().enumerate() {
            let method_id = MethodId(resolving_class.meta.this_id, i as u16);

            if let Some(overridden_by) = overridden_by.remove(&method_id) {
                if verbose {
                    eprint!("    {}{} is overridden by:", m.name, m.descriptor);
                    for overrider in overridden_by.iter() {
                        eprint!(" {}", env.get(overrider.0).name(env));
                    };
                    eprintln!();
                };

                m.overrides.overridden_by = overridden_by;
            };
        };

        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
        } else {
            unreachable!();
        };
    };

    Result::Ok(())
}
