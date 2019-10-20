#![feature(try_blocks)]

pub mod classfile;
pub mod resolve;

fn main() {
    let mut class_loaders: Vec<Box<dyn resolve::ClassLoader>> = vec![Box::new(resolve::ArrayClassLoader())];

    for cp in ::std::env::args().nth(1).unwrap().split(":") {
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

    let main_class = env.find_or_load(&::std::env::args().nth(2).unwrap()).unwrap();
    resolve::resolve_all_classes(&mut env).unwrap();

    println!("Resolved {} classes ({} class files)", env.num_classes(), env.num_user_classes());
}
