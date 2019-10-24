#![feature(try_blocks)]

pub mod bytecode;
pub mod classfile;
pub mod liveness;
pub mod resolve;
pub mod static_interp;

use byteorder::ByteOrder;
use clap::{App, Arg, ArgMatches};
use lazy_static::lazy_static;

fn parse_args<'a>() -> ArgMatches<'a> {
    App::new("Mocha")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Statically compiles JVM bytecode")
        .arg(
            Arg::with_name("classpath")
                .value_name("CLASSPATH")
                .help("Sets the classpath for class resolution")
                .index(1)
                .required(true)
        )
        .arg(
            Arg::with_name("main")
                .value_name("MAIN CLASS")
                .help("The name of the class whose main function should be called on startup")
                .index(2)
                .required(true)
        )
        .arg(
            Arg::with_name("verbose")
                .short("v")
                .help("Enables verbose logging during resolution")
        )
        .get_matches()
}

fn print_resolve_error(env: &resolve::ClassEnvironment, err: &resolve::ClassResolveError) {
    match *err {
        resolve::ClassResolveError::ReadError(ref err) => {
            eprintln!("class read error: {:?}", err);
        },
        resolve::ClassResolveError::NoSuchClass(ref name) => {
            eprintln!("class {} was not found", name);
        },
        resolve::ClassResolveError::NoSuchField(class_id, ref name, ref descriptor) => {
            eprintln!("class {} has no field {} {}", env.get(class_id).name(env), name, descriptor);
        },
        resolve::ClassResolveError::NoSuchMethod(class_id, ref name, ref descriptor) => {
            eprintln!("class {} has no method {}{}", env.get(class_id).name(env), name, descriptor);
        },
        resolve::ClassResolveError::TooManyClasses => {
            eprintln!("too many classes were loaded");
        },
        resolve::ClassResolveError::WhileResolvingClass(class_id, ref err) => {
            eprint!("while resolving for class {}: ", env.get(class_id).name(env));
            print_resolve_error(env, err);
        }
    }
}

lazy_static! {
    static ref MAIN_DESCRIPTOR: classfile::MethodDescriptor = classfile::MethodDescriptor {
        return_type: None,
        param_types: vec![
            classfile::TypeDescriptor {
                array_dims: 1,
                flat: classfile::FlatTypeDescriptor::Reference(std::sync::Arc::from(String::from("java/lang/String").into_boxed_str()))
            }
        ]
    };
}

fn main() {
    let args = parse_args();
    let mut class_loaders: Vec<Box<dyn resolve::ClassLoader>> = vec![Box::new(resolve::ArrayClassLoader())];

    for cp in args.value_of("classpath").unwrap().split(":") {
        let path = ::std::path::PathBuf::from(cp);
        class_loaders.push(if path.extension() == Some(::std::ffi::OsStr::new("jar")) {
            Box::new(
                resolve::JarClassLoader::new(::std::io::BufReader::new(::std::fs::File::open(&path).unwrap())).unwrap()
            )
        } else {
            Box::new(resolve::FileClassLoader::new(path))
        })
    };

    let mut env = resolve::ClassEnvironment::new(class_loaders);

    if let Result::Err(err) = env.load_bootstrap_classes() {
        eprint!("error loading bootstrap classes: ");
        print_resolve_error(&env, &err);
        return;
    };

    let main_class = match env.find_or_load(args.value_of("main").unwrap()) {
        Result::Ok(main_class) => main_class,
        Result::Err(err) => {
            eprint!("error loading main class: ");
            print_resolve_error(&env, &err);
            return;
        }
    };
    let main_method = match **env.get(main_class) {
        resolve::ResolvedClass::User(ref main_class) => {
            main_class.methods.iter().enumerate()
                .filter(|(_, m)| m.name.as_ref() == "main" && m.descriptor == *MAIN_DESCRIPTOR && m.flags.contains(classfile::MethodFlags::STATIC | classfile::MethodFlags::PUBLIC))
                .map(|(i, _)| i)
                .next()
        },
        _ => None
    };
    let main_method = resolve::MethodId(main_class, if let Some(main_method) = main_method {
        main_method as u16
    } else {
        eprintln!("Class {} has no public static method main{}", args.value_of("main").unwrap(), *MAIN_DESCRIPTOR);
        return;
    });

    if let Result::Err(err) = resolve::resolve_all_classes(&mut env, args.is_present("verbose")) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };
    println!("Resolved {} classes ({} class files)", env.num_classes(), env.num_user_classes());

    if let Result::Err(err) = resolve::resolve_all_subitem_references(&mut env, args.is_present("verbose")) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };

    if let Result::Err(err) = resolve::resolve_overriding(&mut env, args.is_present("verbose")) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };

    for id in env.class_ids() {
        if let resolve::ResolvedClass::User(ref mut class) = **env.get_mut(id) {
            for m in class.methods.iter_mut() {
                for a in m.attributes.iter() {
                    if a.name.as_ref() == "Code" {
                        let len = byteorder::BigEndian::read_u32(&a.data[4..]) as usize;
                        let code = &a.data[8..(8 + len)];

                        m.summary = static_interp::summarize_bytecode(
                            bytecode::BytecodeIterator(code, 0),
                            &class.constant_pool
                        );
                    };
                };
            };
        };
    };

    let liveness = liveness::analyze_all(&env, main_method, args.is_present("verbose"));
    println!("Found {} classes requiring initialization ({} classes constructible)", liveness.needs_clinit.len(), liveness.may_construct.len());

    for m in liveness.may_call {
        let class = env.get(m.0).as_user_class();
        let method = &class.methods[m.1 as usize];

        if method.flags.contains(classfile::MethodFlags::NATIVE) {
            println!("NATIVE {}.{}{}", class.meta.name, method.name, method.descriptor);
        };
    };
}
