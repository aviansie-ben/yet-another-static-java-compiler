use std::sync::Arc;

use lazy_static::lazy_static;

use crate::classfile::*;
use crate::layout::{self, ObjectLayout};
use crate::liveness;
use crate::resolve::*;

fn create_mock_class(fields: &[(FieldFlags, ClassId)]) -> Class {
    let empty_string = Arc::from(String::new().into_boxed_str());
    Class {
        version: (0, 0),
        constant_pool: vec![
            ConstantPoolEntry::Class(ConstantClass {
                name: Arc::clone(&empty_string),
                class_id: ClassId::UNRESOLVED,
                array_class_id: ClassId::UNRESOLVED
            }),
            ConstantPoolEntry::Class(ConstantClass {
                name: Arc::clone(&empty_string),
                class_id: ClassId::JAVA_LANG_OBJECT,
                array_class_id: ClassId::UNRESOLVED
            })
        ],
        flags: ClassFlags::empty(),
        this_class_cp: 0,
        super_class_cp: 1,
        interfaces: vec![],
        fields: (
            fields.iter().map(|&(flags, type_id)| {
                Field {
                    flags,
                    name: Arc::clone(&empty_string),
                    descriptor: TypeDescriptor::parse("Z").unwrap(),
                    attributes: vec![],
                    class_id: type_id,
                    off: !0
                }
            }).collect()
        ),
        methods: vec![],
        attributes: vec![],
        layout: ObjectLayout::empty(),
        meta: ClassMeta {
            this_id: ClassId::UNRESOLVED,
            super_id: ClassId::UNRESOLVED,
            interface_ids: vec![],
            all_interface_ids: vec![],
            clinit_method: None,
            name: Arc::clone(&empty_string)
        }
    }
}

#[derive(Debug)]
struct FakeClassLoader;

impl ClassLoader for FakeClassLoader {
    fn try_load(&self, name: &str, _: &mut dyn FnMut (&str) -> Result<ClassId, ClassResolveError>) -> Option<Result<ResolvedClass, ClassResolveError>> {
        if name == "java/lang/Class" {
            Some(Result::Ok(ResolvedClass::User(create_mock_class(&[
                (FieldFlags::empty(), ClassId::JAVA_LANG_OBJECT),
                (FieldFlags::empty(), ClassId::PRIMITIVE_INT),
                (FieldFlags::empty(), ClassId::JAVA_LANG_OBJECT),
                (FieldFlags::empty(), ClassId::JAVA_LANG_OBJECT)
            ]))))
        } else if name == "java/lang/String" {
            Some(Result::Ok(ResolvedClass::User(create_mock_class(&[
                (FieldFlags::empty(), ClassId::PRIMITIVE_CHAR_ARRAY)
            ]))))
        } else if name == "java/lang/Object" || name == "java/lang/invoke/MethodHandle" || name == "java/lang/reflect/Field" {
            Some(Result::Ok(ResolvedClass::User(create_mock_class(&[]))))
        } else {
            None
        }
    }
}

fn create_fake_env() -> ClassEnvironment {
    let mut env = ClassEnvironment::new(vec![
        Box::new(ArrayClassLoader()),
        Box::new(FakeClassLoader)
    ]);

    env.load_bootstrap_classes().unwrap();
    env.force_set_class("mocha/$test/SingleRef", ResolvedClass::User(
        create_mock_class(&[(FieldFlags::empty(), ClassId::JAVA_LANG_OBJECT)])
    ));
    env.force_set_class("mocha/$test/SingleRefStatic", ResolvedClass::User(
        create_mock_class(&[(FieldFlags::STATIC, ClassId::JAVA_LANG_OBJECT)])
    ));

    let liveness = liveness::create_full_liveness(&env);
    layout::compute_all_layouts(&mut env, &liveness, false);

    env
}

lazy_static! {
    pub static ref TEST_ENV: ClassEnvironment = create_fake_env();
}
