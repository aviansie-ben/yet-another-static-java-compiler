use std::io::Write;

use crate::obj::{MochaByteArray, MochaObject};

#[repr(C)]
pub struct MochaFileDescriptor {
    obj: MochaObject,
    fd: u32
}

#[repr(C)]
pub struct MochaFileOutputStream {
    obj: MochaObject,
    fd: *mut MochaFileDescriptor
}

unsafe fn get_fd_stream(fd: *mut MochaFileDescriptor) -> Box<dyn Write> {
    assert!(!fd.is_null());
    match fd.as_ref().unwrap().fd {
        1 => Box::new(std::io::stdout()),
        2 => Box::new(std::io::stderr()),
        _ => unimplemented!()
    }
}

#[no_mangle]
pub unsafe extern fn java_io_FileOutputStream_write(stream: *mut MochaFileOutputStream, char: i32, _: i32) {
    let stream = stream.as_ref().unwrap();
    let mut stream = get_fd_stream(stream.fd);
    match stream.write_all(&[char as u8]) {
        Ok(_) => match stream.flush() {
            Ok(_) => {},
            Err(e) => {
                panic!("io error: {:?}", e);
            }
        },
        Err(e) => {
            panic!("io error: {:?}", e);
        }
    }
}

#[no_mangle]
pub unsafe extern fn java_io_FileOutputStream_writeBytes(stream: *mut MochaFileOutputStream, arr: *mut MochaByteArray, off: i32, len: i32, _: i32) {
    let stream = stream.as_ref().unwrap();

    assert!(!arr.is_null());
    let arr = arr.as_ref().unwrap();

    assert!(off >= 0);
    let off = off as usize;

    assert!(len >= 0);
    let len = len as usize;

    assert!(off < (arr.len as usize) && len <= ((arr.len as usize) - off));

    let mut stream = get_fd_stream(stream.fd);
    match stream.write_all(&arr.data()[off..(off + len)]) {
        Ok(_) => match stream.flush() {
            Ok(_) => {},
            Err(e) => {
                panic!("io error: {:?}", e);
            }
        },
        Err(e) => {
            panic!("io error: {:?}", e);
        }
    }
}
