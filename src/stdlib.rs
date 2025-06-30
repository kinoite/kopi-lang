use crate::interpreter::KopiValue;
use std::collections::HashMap;
use std::rc::Rc;
use std::fs;

pub type KopiModule = HashMap<String, KopiValue>;
pub type KopiNativeFn = Rc<dyn Fn(Vec<KopiValue>) -> Result<KopiValue, String>>;

fn unescape_string(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            match chars.next() {
                Some('n') => result.push('\n'),
                Some('t') => result.push('\t'),
                Some('\\') => result.push('\\'),
                Some('"') => result.push('"'),
                Some(other) => {
                    result.push('\\');
                    result.push(other);
                }
                None => result.push('\\'),
            }
        } else {
            result.push(c);
        }
    }
    result
}

pub fn put(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.is_empty() {
        return Err("put() expects at least 1 argument.".to_string());
    }
    let output = match &args[0] {
        KopiValue::String(s) => unescape_string(s),
        other => other.to_string(),
    };
    println!("{}", output);
    Ok(KopiValue::Nil)
}

pub fn len(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 {
        return Err("len() expects exactly 1 argument.".to_string());
    }
    match &args[0] {
        KopiValue::String(s) => Ok(KopiValue::Number(s.len() as f64)),
        KopiValue::Array(arr) => Ok(KopiValue::Number(arr.borrow().len() as f64)),
        _ => Err("Argument to len() must be a string or an array.".to_string())
    }
}

pub fn create_os_module() -> KopiModule {
    let mut members = KopiModule::new();
    let read_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        Ok(KopiValue::String("file content placeholder".to_string()))
    };
    let sleep_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("sleep() expects 1 argument: seconds.".to_string()); }
        if let KopiValue::Number(seconds) = args[0] {
            std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
            Ok(KopiValue::Nil)
        } else {
            Err("sleep() argument must be a number.".to_string())
        }
    };
    members.insert("read".to_string(), KopiValue::new_native_fn("read", read_fn));
    members.insert("sleep".to_string(), KopiValue::new_native_fn("sleep", sleep_fn));
    members
}

pub fn create_io_module() -> KopiModule {
    let mut members = KopiModule::new();
    let write_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 {
            return Err("io.write() expects 2 arguments: a filepath and the content.".to_string());
        }
        let path = match &args[0] {
            KopiValue::String(s) => s,
            _ => return Err("First argument to io.write() must be a string filepath.".to_string()),
        };
        let content = match &args[1] {
            KopiValue::String(s) => unescape_string(s),
            other => other.to_string(),
        };

        match fs::write(path, content) {
            Ok(_) => Ok(KopiValue::Nil),
            Err(e) => Err(format!("Failed to write to file: {}", e)),
        }
    };
    members.insert("write".to_string(), KopiValue::new_native_fn("write", write_fn));
    members
}

pub fn create_color_module() -> KopiModule {
    let mut members = KopiModule::new();
    let apply_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("apply() expects 2 arguments: a value and a hex color string.".to_string()); }
        let s = args[0].to_string();
        if let KopiValue::String(color) = &args[1] {
            let (r, g, b) = if color.len() == 6 {
                (
                    u8::from_str_radix(&color[0..2], 16).unwrap_or(255),
                    u8::from_str_radix(&color[2..4], 16).unwrap_or(255),
                    u8::from_str_radix(&color[4..6], 16).unwrap_or(255),
                )
            } else { (255, 255, 255) };
            let colored_string = format!("\x1b[38;2;{};{};{}m{}\x1b[0m", r, g, b, s);
            return Ok(KopiValue::String(colored_string));
        }
        Err("Color argument must be a hex string like 'FF0000'.".to_string())
    };
    members.insert("apply".to_string(), KopiValue::new_native_fn("apply", apply_fn));
    members
}
