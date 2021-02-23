#![feature(allocator_api)]
#![feature(bindings_after_at)]
#![feature(btree_drain_filter)]
#![feature(drain_filter)]
#![feature(str_split_once)]
#![feature(try_blocks)]

pub mod log;

pub mod backend;
pub mod bytecode;
pub mod classfile;
pub mod layout;
pub mod liveness;
pub mod mil;
pub mod opt;
pub mod resolve;
pub mod static_heap;
pub mod static_interp;
pub mod util;

#[cfg(test)]
mod test_util;

use std::collections::HashSet;

use clap::{App, Arg, ArgMatches};
use itertools::Itertools;
use lazy_static::lazy_static;

fn parse_args<'a>() -> ArgMatches<'a> {
    App::new("Mocha")
        .version(env!("CARGO_PKG_VERSION"))
        .about("Statically compiles JVM bytecode")
        .author(env!("CARGO_PKG_AUTHORS"))
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
            Arg::with_name("optimize")
                .long("optimize")
                .help("Perform optimizations on Java code before emitting LLVM bitcode")
        )
        .arg(
            Arg::with_name("verbose")
                .long("verbose")
                .takes_value(true)
                .possible_values(&["load", "resolve", "liveness", "layout", "clinit", "ilgen", "opt", "codegen"])
                .use_delimiter(true)
                .help("Enables verbose logging for the provided phases")
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

    let mut verbose_load = false;
    let mut verbose_resolve = false;
    let mut verbose_liveness = false;
    let mut verbose_layout = false;
    let mut verbose_clinit = false;
    let mut verbose_ilgen = false;
    let mut verbose_opt = false;
    let mut verbose_codegen = false;

    if let Some(verbose_options) = args.values_of("verbose") {
        for verbose_option in verbose_options {
            match verbose_option {
                "load" => {
                    verbose_load = true;
                },
                "resolve" => {
                    verbose_resolve = true;
                },
                "liveness" => {
                    verbose_liveness = true;
                },
                "layout" => {
                    verbose_layout = true;
                },
                "clinit" => {
                    verbose_clinit = true;
                },
                "ilgen" => {
                    verbose_ilgen = true;
                },
                "opt" => {
                    verbose_opt = true;
                },
                "codegen" => {
                    verbose_codegen = true;
                },
                _ => unreachable!()
            };
        };
    };

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

    if let Result::Err(err) = resolve::resolve_all_classes(&mut env, verbose_load) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };
    println!("Loaded {} classes ({} class files) in {:.3}s", env.num_classes(), env.num_user_classes(), start_load_classes.elapsed().as_secs_f32());

    let start_resolve_subitems = std::time::Instant::now();
    if let Result::Err(err) = resolve::resolve_all_subitem_references(&mut env, verbose_resolve) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };

    if let Result::Err(err) = resolve::resolve_overriding(&mut env, verbose_resolve) {
        eprint!("error during resolution: ");
        print_resolve_error(&env, &err);
        return;
    };
    println!("Resolved subitems in {:.3}s", start_resolve_subitems.elapsed().as_secs_f32());

    let start_summarize_methods = std::time::Instant::now();
    let mut num_methods_summarized = 0;
    let mut summaries = vec![];
    for id in env.class_ids() {
        if let resolve::ResolvedClass::User(ref class) = **env.get(id) {
            for (i, m) in class.methods.iter().enumerate() {
                summaries.push(if let Some(classfile::MethodBody::Code(ref code)) = m.body {
                    num_methods_summarized += 1;
                    static_interp::summarize_bytecode(
                        bytecode::BytecodeIterator::for_code(code),
                        resolve::MethodId(id, i as u16),
                        &class.constant_pool
                    )
                } else {
                    classfile::MethodSummary::empty()
                });
            };

            for (method, summary) in env.get_mut(id).as_user_class_mut().methods.iter_mut().zip(summaries.drain(..)) {
                method.summary = summary;
            };
        };
    };
    println!("Summarized {} methods in {:.3}s", num_methods_summarized, start_summarize_methods.elapsed().as_secs_f32());

    let start_analyze_liveness = std::time::Instant::now();
    let mut liveness = liveness::analyze_all(&env, main_method, verbose_liveness);
    println!("Found {} classes requiring initialization ({} classes constructible) in {:.3}s", liveness.needs_clinit.len(), liveness.may_construct.len(), start_analyze_liveness.elapsed().as_secs_f32());

    let start_layout = std::time::Instant::now();
    layout::compute_all_layouts(&mut env, &liveness, verbose_layout);
    println!("Computed object layouts in {:.3}s", start_layout.elapsed().as_secs_f32());

    let start_heap = std::time::Instant::now();
    let constant_strings = static_heap::collect_constant_strings(liveness.may_use_strings.iter().cloned(), &mut env);
    let mut heap = unsafe { static_heap::JavaStaticHeap::new(&env, 64 * 1024 * 1024) };
    if heap.init_class_objects(liveness.needs_class_object.iter().cloned().sorted_by_key(|cls| cls.0)).is_err() {
        eprintln!("Failed to create class objects in static heap");
        return;
    };
    if heap.init_constant_strings(constant_strings.iter().map(|r| r.as_ref())).is_err() {
        eprintln!("Failed to create constant strings in static heap");
        return;
    };

    println!("Constructed initial static heap in {:.3}s", start_heap.elapsed().as_secs_f32());

    let mut good_clinit = 0usize;
    let mut bad_clinit = 0usize;
    let mut needs_runtime_clinit = HashSet::new();

    let start_clinit = std::time::Instant::now();
    for class_id in liveness.needs_clinit.iter().cloned().sorted_by_key(|cls| cls.0) {
        if static_interp::try_run_clinit(&env, &heap, class_id, verbose_clinit) {
            good_clinit += 1;
        } else {
            bad_clinit += 1;
            needs_runtime_clinit.insert(class_id);
        };
    };

    println!(
        "Ran static class initialization for {} classes ({} failed) in {:.3}s",
        good_clinit + bad_clinit,
        bad_clinit,
        start_clinit.elapsed().as_secs_f32()
    );

    let start_liveness_methods = std::time::Instant::now();
    liveness::analyze_post_clinit(&env, main_method, &mut liveness, HashSet::new(), verbose_liveness);
    println!("Found {} executable methods in {:.3}s", liveness.may_call.len(), start_liveness_methods.elapsed().as_secs_f32());

    let mut known_objects = mil::il::MilKnownObjectMap::new();
    for obj in heap.all_objs() {
        known_objects.add(obj);
    };
    for class_id in liveness.needs_class_object.iter().cloned() {
        let obj_id = known_objects.id_of(&heap.get_class_object(class_id));
        known_objects.refs.classes.insert(class_id, obj_id);
    };
    for i in 0..(constant_strings.len()) {
        let obj_id = known_objects.id_of(&heap.get_constant_string(i));
        known_objects.refs.strings.push(obj_id);
    };

    let start_ilgen = std::time::Instant::now();
    let mut program = mil::il::MilProgram::new(known_objects, main_method);
    for method_id in liveness.may_call.iter().cloned().sorted_by_key(|m| ((m.0).0, m.1)) {
        if let Some(func) = mil::ilgen::generate_il_for_method(&env, method_id, &program.known_objects.refs, &liveness, verbose_ilgen) {
            program.funcs.insert(method_id, func);
        };
    };
    println!("Generated MIL for {} functions in {:.3}s", program.funcs.len(), start_ilgen.elapsed().as_secs_f32());

    if args.is_present("optimize") {
        let mut stdout = std::io::stdout();
        let log = if verbose_opt {
            log::Log::new(&mut stdout)
        } else {
            log::Log::none()
        };

        let start_opt = std::time::Instant::now();
        opt::optimize_program(&mut program, &env, &heap, &log);
        println!("Optimized MIL in {:.3}s", start_opt.elapsed().as_secs_f32());
    };

    let start_codegen = std::time::Instant::now();
    let llvm_ctx = backend::llvm::LLVMContext::new();
    let llvm_module = backend::llvm::emit_llvm_ir(&env, &program, &liveness, &heap, &llvm_ctx, verbose_codegen);
    llvm_module.write_bitcode_to_file("test.bc").unwrap();
    println!("Generated LLVM bitcode in {:.3}s", start_codegen.elapsed().as_secs_f32());
}
