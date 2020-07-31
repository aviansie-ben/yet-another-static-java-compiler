use std::io::Write;

#[derive(Debug)]
#[repr(C)]
pub struct MochaEnv {
    pub builtin_classes: *const BuiltinClassVTables
}

#[derive(Debug)]
#[repr(C)]
pub struct BuiltinClassVTables {
    pub void: u32,
    pub byte: u32,
    pub char: u32,
    pub double: u32,
    pub float: u32,
    pub int: u32,
    pub long: u32,
    pub short: u32,
    pub boolean: u32,

    pub byte_array: u32,
    pub char_array: u32,
    pub double_array: u32,
    pub float_array: u32,
    pub int_array: u32,
    pub long_array: u32,
    pub short_array: u32,
    pub boolean_array: u32,

    pub object: u32,
    pub method_handle: u32,
    pub string: u32,
    pub class: u32,
    pub reflect_field: u32,

    pub object_array: u32,
    pub reflect_field_array: u32
}

#[no_mangle]
pub unsafe fn mocha_init(builtin_classes: *const BuiltinClassVTables) -> *mut MochaEnv {
    Box::into_raw(Box::new(MochaEnv {
        builtin_classes
    }))
}

#[no_mangle]
pub unsafe fn mocha_shutdown(env: *mut MochaEnv) {
    std::mem::drop(Box::from_raw(env));

    std::io::stdout().flush().unwrap();
    std::io::stderr().flush().unwrap();
}
