use std::io::Write;
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Clone)]
pub struct Log<'a>(Option<Arc<Mutex<&'a mut dyn Write>>>);

impl <'a> Log<'a> {
    pub fn new(w: &'a mut dyn Write) -> Log<'a> {
        Log(Some(Arc::new(Mutex::new(w))))
    }

    pub fn none() -> Log<'a> {
        Log(None)
    }

    pub fn lock(&self) -> Option<MutexGuard<&'a mut dyn Write>> {
        if let Some(ref mutex) = self.0 {
            Some(mutex.lock().unwrap())
        } else {
            None
        }
    }
}

#[macro_export]
macro_rules! log_write {
    ($log:expr, $($arg:tt)*) => {
        if let Some(mut w) = $log.lock() {
            write!(w, $($arg)*).unwrap();
        };
    }
}

#[macro_export]
macro_rules! log_writeln {
    ($log:expr) => {
        if let Some(mut w) = $log.lock() {
            writeln!(w).unwrap();
        };
    };
    ($log:expr, $($arg:tt)*) => {
        if let Some(mut w) = $log.lock() {
            writeln!(w, $($arg)*).unwrap();
        };
    }
}

#[cfg(test)]
mod tests {
    use std::fmt;
    use super::Log;

    struct TestDisplay(&'static str);

    impl fmt::Display for TestDisplay {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            write!(f, "{}", self.0)
        }
    }

    #[test]
    fn test_write() {
        let mut log = vec![];

        log_write!(Log::new(&mut log), "{}", TestDisplay("123"));

        assert_eq!(b"123", &log[..]);
    }

    #[test]
    fn test_write_side_effects_for_some_log() {
        let mut val = false;

        log_write!(Log::new(&mut vec![]), "{}", {
            val = true;
            ""
        });

        assert_eq!(true, val);
    }

    #[test]
    fn test_write_no_side_effects_for_none_log() {
        let mut val = false;

        log_write!(Log::none(), "{}", {
            val = true;
            ""
        });

        assert_eq!(false, val);
    }

    #[test]
    fn test_writeln() {
        let mut log = vec![];

        log_writeln!(Log::new(&mut log), "{}", TestDisplay("123"));

        assert_eq!(b"123\n", &log[..]);
    }

    #[test]
    fn test_writeln_blank() {
        let mut log = vec![];

        log_writeln!(Log::new(&mut log));

        assert_eq!(b"\n", &log[..]);
    }

    #[test]
    fn test_writeln_side_effects_for_some_log() {
        let mut val = false;

        log_writeln!(Log::new(&mut vec![]), "{}", {
            val = true;
            ""
        });

        assert_eq!(true, val);
    }

    #[test]
    fn test_writeln_no_side_effects_for_none_log() {
        let mut val = false;

        log_writeln!(Log::none(), "{}", {
            val = true;
            ""
        });

        assert_eq!(false, val);
    }
}
