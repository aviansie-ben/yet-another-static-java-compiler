#![feature(try_blocks)]

pub mod classfile;
pub mod resolve;

use clap::{App, Arg, ArgMatches};

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

    let main_class = env.find_or_load(args.value_of("main").unwrap()).unwrap();
    resolve::resolve_all_classes(&mut env, args.is_present("verbose")).unwrap();

    println!("Resolved {} classes ({} class files)", env.num_classes(), env.num_user_classes());
}
