#![feature(allocator_api)]
#![feature(drain_filter)]
#![feature(try_blocks)]

pub mod bytecode;
pub mod classfile;
pub mod layout;
pub mod liveness;
pub mod resolve;
pub mod static_heap;
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

    let start_load_classes = std::time::Instant::now();
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
    println!("Loaded {} classes ({} class files) in {:.3}s", env.num_classes(), env.num_user_classes(), start_load_classes.elapsed().as_secs_f32());

    let start_resolve_subitems = std::time::Instant::now();
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
    println!("Resolved subitems in {:.3}s", start_resolve_subitems.elapsed().as_secs_f32());

    let start_summarize_methods = std::time::Instant::now();
    let mut num_methods_summarized = 0;
    for id in env.class_ids() {
        if let resolve::ResolvedClass::User(ref mut class) = **env.get_mut(id) {
            for m in class.methods.iter_mut() {
                if let Some(code) = bytecode::BytecodeIterator::for_method(m) {
                    m.summary = static_interp::summarize_bytecode(
                        code,
                        &class.constant_pool
                    );
                    num_methods_summarized += 1;
                };
            };
        };
    };
    println!("Summarized {} methods in {:.3}s", num_methods_summarized, start_summarize_methods.elapsed().as_secs_f32());

    let start_analyze_liveness = std::time::Instant::now();
    let liveness = liveness::analyze_all(&env, main_method, args.is_present("verbose"));
    println!("Found {} classes requiring initialization ({} classes constructible) in {:.3}s", liveness.needs_clinit.len(), liveness.may_construct.len(), start_analyze_liveness.elapsed().as_secs_f32());

    for m in liveness.may_call.iter().cloned() {
        let class = env.get(m.0).as_user_class();
        let method = &class.methods[m.1 as usize];

        if method.flags.contains(classfile::MethodFlags::NATIVE) {
            println!("NATIVE {}.{}{}", class.meta.name, method.name, method.descriptor);
        };
    };

    let start_layout = std::time::Instant::now();
    layout::compute_all_layouts(&mut env, &liveness, args.is_present("verbose"));
    println!("Computed object layouts in {:.3}s", start_layout.elapsed().as_secs_f32());

    let start_heap = std::time::Instant::now();
    let constant_strings = static_heap::collect_constant_strings(liveness.needs_clinit.iter().cloned(), &mut env);
    let mut heap = unsafe { static_heap::JavaStaticHeap::new(&env, 64 * 1024 * 1024) };
    if heap.init_class_objects(liveness.needs_clinit.iter().cloned()).is_err() {
        eprintln!("Failed to create class objects in static heap");
        return;
    };
    if heap.init_constant_strings(constant_strings.iter().map(|r| r.as_ref())).is_err() {
        eprintln!("Failed to create constant strings in static heap");
    };

    println!("Constructed initial static heap in {:.3}s", start_heap.elapsed().as_secs_f32());
}
