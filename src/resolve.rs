use std::cell::RefCell;
use std::collections::{HashMap, HashSet, VecDeque};
use std::collections::hash_map;
use std::fmt::{Debug, Write};
use std::fs::File;
use std::io::{BufReader, Read, Seek};
use std::mem;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use lazy_static::lazy_static;
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
    pub const JAVA_LANG_OBJECT: ClassId = ClassId(8);
    pub const JAVA_LANG_INVOKE_METHODHANDLE: ClassId = ClassId(9);
    pub const JAVA_LANG_STRING: ClassId = ClassId(10);

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

impl FieldId {
    pub const UNRESOLVED: FieldId = FieldId(ClassId::UNRESOLVED, !0);
}

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
    NoSuchField(ClassId, String, TypeDescriptor),
    NoSuchMethod(ClassId, String, MethodDescriptor),
    TooManyClasses,
    WhileResolvingClass(ClassId, Box<ClassResolveError>)
}

#[derive(Debug, Clone)]
pub enum ResolvedClass {
    User(Class),
    Primitive(PrimitiveType),
    Array(u8, ClassId)
}

impl ResolvedClass {
    pub fn name(&self, env: &ClassEnvironment) -> String {
        let mut name = String::new();

        match *self {
            ResolvedClass::User(ref class) => {
                write!(name, "{}", class.meta.name).unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Byte) => {
                write!(name, "byte").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Char) => {
                write!(name, "char").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Double) => {
                write!(name, "double").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Float) => {
                write!(name, "float").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Int) => {
                write!(name, "int").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Long) => {
                write!(name, "long").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Short) => {
                write!(name, "short").unwrap();
            },
            ResolvedClass::Primitive(PrimitiveType::Boolean) => {
                write!(name, "boolean").unwrap();
            },
            ResolvedClass::Array(dims, inner_id) => {
                for _ in 0..dims {
                    write!(name, "[").unwrap();
                };
                match **env.get(inner_id) {
                    ResolvedClass::User(ref class) => {
                        write!(name, "L{};", class.meta.name).unwrap();
                    },
                    ResolvedClass::Primitive(t) => {
                        write!(name, "{}", t).unwrap();
                    },
                    ResolvedClass::Array(_, _) => unreachable!()
                };
            }
        };

        name
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
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Byte))),
            ClassId::PRIMITIVE_BYTE
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Char))),
            ClassId::PRIMITIVE_CHAR
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Double))),
            ClassId::PRIMITIVE_DOUBLE
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Float))),
            ClassId::PRIMITIVE_FLOAT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Int))),
            ClassId::PRIMITIVE_INT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Long))),
            ClassId::PRIMITIVE_LONG
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Short))),
            ClassId::PRIMITIVE_SHORT
        );
        assert_eq!(
            self.internals.add_unnamed_class(Box::new(ResolvedClass::Primitive(PrimitiveType::Boolean))),
            ClassId::PRIMITIVE_BOOLEAN
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
            ResolvedClass::Array(_, _) => {
                return find_with_super(
                    env,
                    ClassId::JAVA_LANG_OBJECT,
                    resolving_t,
                    resolving_meta,
                    get_class_t,
                    find_t
                );
            }
        }
    };

    find_t(class_id, t).or_else(|| {
        meta.interface_ids.iter()
            .cloned()
            .chain(itertools::repeat_n(meta.super_id, 1))
            .filter_map(|id| {
                if id != ClassId::UNRESOLVED {
                    find_with_super(env, id, resolving_t, resolving_meta, get_class_t, find_t)
                } else {
                    None
                }
            })
            .next()
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
    resolving_methods: &[Method],
    resolving_meta: &ClassMeta,
    methodref: &ConstantMethodref
) -> MethodId {
    find_with_super(
        env,
        class_id,
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

                    if verbose {
                        eprintln!("    Resolved field {} {} as {:?}", cpe.name, cpe.descriptor, cpe.field_id);
                    };
                },
                ConstantPoolEntry::Methodref(ref mut cpe) | ConstantPoolEntry::InterfaceMethodref(ref mut cpe) => {
                    let class_id = class_ids[cpe.class as usize];

                    cpe.method_id = if class_id != ClassId::UNRESOLVED {
                        let method_id = find_method(env, class_id, &resolving_class.methods, &resolving_class.meta, cpe);

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

        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);
        } else {
            unreachable!();
        };
    };

    Result::Ok(())
}

pub fn resolve_overriding(env: &mut ClassEnvironment, verbose: bool) -> Result<(), ClassResolveError> {
    let mut resolving_class = Box::new(Class::dummy_class());
    let mut overridden_by: HashMap<MethodId, Vec<MethodId>> = HashMap::new();

    for resolving_id in env.class_ids() {
        if let ResolvedClass::User(ref mut class) = **env.get_mut(resolving_id) {
            mem::swap(class, &mut resolving_class);

            if verbose {
                eprintln!("Resolving overriding from {}...", resolving_class.meta.name);
            };
        } else {
            continue;
        };

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
                &[],
                resolving_meta,
                &methodref
            );

            m.overrides.overrides_interface = resolving_class.meta.interface_ids.iter().cloned()
                .filter_map(|interface| {
                    let method = find_method(env, interface, &[], resolving_meta, &methodref);
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
