use std::cell::RefCell;
use std::collections::{HashMap, VecDeque};
use std::fmt::Debug;
use std::fs::File;
use std::io::{BufReader, Read, Seek};
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use zip::ZipArchive;
use zip::result::{ZipError, ZipResult};

use crate::classfile::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ClassId(pub u32);

impl ClassId {
    pub const UNRESOLVED: ClassId = ClassId(!0);

    pub const PRIMITIVE_BYTE: ClassId = ClassId(0);
    pub const PRIMITIVE_CHAR: ClassId = ClassId(1);
    pub const PRIMITIVE_DOUBLE: ClassId = ClassId(2);
    pub const PRIMITIVE_FLOAT: ClassId = ClassId(3);
    pub const PRIMITIVE_INT: ClassId = ClassId(4);
    pub const PRIMITIVE_LONG: ClassId = ClassId(5);
    pub const PRIMITIVE_SHORT: ClassId = ClassId(6);
    pub const PRIMITIVE_BOOLEAN: ClassId = ClassId(7);

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
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct MethodId(pub ClassId, pub u16);

impl MethodId {
    pub const UNRESOLVED: MethodId = MethodId(ClassId::UNRESOLVED, !0);
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct FieldId(pub ClassId, pub u16);

pub trait ClassLoader: Debug {
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
                    let elem_id = match descriptor.flat {
                        FlatTypeDescriptor::Primitive(t) => ClassId::for_primitive_type(t),
                        FlatTypeDescriptor::Reference(ref elem_name) => resolve(elem_name)?
                    };
                    ResolvedClass::Array(descriptor.array_dims, elem_id)
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
pub struct JarClassLoader<R: Read + Seek + Debug>(RefCell<ZipArchive<R>>);

impl <R: Read + Seek + Debug> JarClassLoader<R> {
    pub fn new(read: R) -> ZipResult<JarClassLoader<R>> {
        Result::Ok(JarClassLoader(RefCell::new(ZipArchive::new(read)?)))
    }
}

impl <R: Read + Seek + Debug> ClassLoader for JarClassLoader<R> {
    fn try_load(
        &self,
        name: &str,
        _: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>
    ) -> Option<Result<ResolvedClass, ClassResolveError>> {
        let name = format!("{}.class", name);
        let mut archive = self.0.borrow_mut();
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
    TooManyClasses
}

#[derive(Debug, Clone)]
pub enum ResolvedClass {
    User(Class),
    Primitive(PrimitiveType),
    Array(u8, ClassId)
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
            class.id = id;

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
            class.name = Arc::from(name.to_owned().into_boxed_str());
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
        let mut env = ClassEnvironment {
            class_loaders,
            internals: ClassEnvironmentInternals {
                classes: vec![],
                class_names: HashMap::new()
            }
        };

        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Byte))),
            ClassId::PRIMITIVE_BYTE
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Char))),
            ClassId::PRIMITIVE_CHAR
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Double))),
            ClassId::PRIMITIVE_DOUBLE
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Float))),
            ClassId::PRIMITIVE_FLOAT
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Int))),
            ClassId::PRIMITIVE_INT
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Long))),
            ClassId::PRIMITIVE_LONG
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Short))),
            ClassId::PRIMITIVE_SHORT
        );
        assert_eq!(
            env.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Boolean))),
            ClassId::PRIMITIVE_BOOLEAN
        );

        env
    }

    fn load(&mut self, name: &str) -> Result<ClassId, ClassResolveError> {
        fn do_load(name: &str, class_loaders: &[Box<dyn ClassLoader>], internals: &mut ClassEnvironmentInternals) -> Result<ClassId, ClassResolveError> {
            if internals.classes.len() >= (u32::max_value() - 1) as usize {
                return Result::Err(ClassResolveError::TooManyClasses);
            };

            let class: ResolvedClass = class_loaders.iter().map(|cl| {
                cl.try_load(name, &mut |name| {
                    do_load(name, class_loaders, internals)
                })
            }).filter_map(|result| result).next().unwrap_or_else(
                || Result::Err(ClassResolveError::NoSuchClass(name.to_owned()))
            )?;

            Result::Ok(internals.add_class(name, Box::new(class)))
        }

        do_load(name, &self.class_loaders, &mut self.internals)
    }

    pub fn get(&self, id: ClassId) -> &Box<ResolvedClass> {
        &self.internals.classes[id.0 as usize]
    }

    pub fn get_mut(&mut self, id: ClassId) -> &mut Box<ResolvedClass> {
        &mut self.internals.classes[id.0 as usize]
    }

    pub fn try_find(&self, name: &str) -> Option<ClassId> {
        self.internals.class_names.get(name).map(|&id| id)
    }

    pub fn find_or_load(&mut self, name: &str) -> Result<ClassId, ClassResolveError> {
        if let Some(id) = self.try_find(name) {
            Result::Ok(id)
        } else {
            self.load(name)
        }
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

    pub fn class_ids(&self) -> impl Iterator<Item=ClassId> {
        (0..(self.num_classes())).map(|i| ClassId(i as u32))
    }
}

pub fn resolve_all_classes(env: &mut ClassEnvironment) -> Result<(), ClassResolveError> {
    let mut resolving_class = Box::new(Class {
        version: (0, 0),
        constant_pool: vec![],
        flags: ClassFlags::empty(),
        this_class_cp: 0,
        super_class_cp: 0,
        interfaces: vec![],
        fields: vec![],
        methods: vec![],
        attributes: vec![],

        id: ClassId::UNRESOLVED,
        name: Arc::from(String::from("").into_boxed_str())
    });
    let mut worklist = VecDeque::new();

    for id in env.class_ids() {
        worklist.push_back(id);
    };

    while let Some(resolving_id) = worklist.pop_front() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
            println!("Resolving {}...", resolving_class.name);
        } else {
            continue;
        };

        for cpe in resolving_class.constant_pool.iter_mut() {
            if let ConstantPoolEntry::Class(ref mut cpe) = *cpe {
                if cpe.class_id == ClassId::UNRESOLVED {
                    cpe.class_id = if let Some(resolved_id) = env.try_find(&cpe.name) {
                        println!("    Already loaded {}", cpe.name);
                        resolved_id
                    } else {
                        println!("    Loading {}...", cpe.name);
                        let resolved_id = env.load(&cpe.name)?;

                        worklist.push_back(resolved_id);
                        resolved_id
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
