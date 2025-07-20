use crate::interpreter::KopiValue;
use std::collections::HashMap;
use std::rc::Rc;
use std::fs;
use std::cell::RefCell;
use crossterm::{
    cursor, event, execute, terminal, style,
    event::{Event, KeyCode, KeyEvent}
};
use std::io;

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

pub fn is_number(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 { return Err("is_number() expects 1 argument.".to_string()); }
    Ok(KopiValue::Boolean(matches!(args[0], KopiValue::Number(_))))
}

pub fn is_string(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 { return Err("is_string() expects 1 argument.".to_string()); }
    Ok(KopiValue::Boolean(matches!(args[0], KopiValue::String(_))))
}

pub fn is_boolean(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 { return Err("is_boolean() expects 1 argument.".to_string()); }
    Ok(KopiValue::Boolean(matches!(args[0], KopiValue::Boolean(_))))
}

pub fn is_array(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 { return Err("is_array() expects 1 argument.".to_string()); }
    Ok(KopiValue::Boolean(matches!(args[0], KopiValue::Array(_))))
}

pub fn is_nil(args: Vec<KopiValue>) -> Result<KopiValue, String> {
    if args.len() != 1 { return Err("is_nil() expects 1 argument.".to_string()); }
    Ok(KopiValue::Boolean(matches!(args[0], KopiValue::Nil)))
}

pub fn create_os_module() -> KopiModule {
    let mut members = KopiModule::new();

    let read_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 {
            return Err("io.read() expects 1 argument: a filepath.".to_string());
        }
        let path = match &args[0] {
            KopiValue::String(s) => s,
            _ => return Err("Argument to io.read() must be a string filepath.".to_string()),
        };
        match fs::read_to_string(path) {
            Ok(content) => Ok(KopiValue::String(content)),
            Err(e) => Err(format!("Failed to read file '{}': {}", path, e)),
        }
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

pub fn create_string_module() -> KopiModule {
    let mut members = KopiModule::new();

    let to_upper_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("string.to_upper() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::String(s) => Ok(KopiValue::String(s.to_uppercase())),
            _ => Err("string.to_upper() expects a string argument.".to_string()),
        }
    };

    let to_lower_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("string.to_lower() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::String(s) => Ok(KopiValue::String(s.to_lowercase())),
            _ => Err("string.to_lower() expects a string argument.".to_string()),
        }
    };

    let trim_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("string.trim() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::String(s) => Ok(KopiValue::String(s.trim().to_string())),
            _ => Err("string.trim() expects a string argument.".to_string()),
        }
    };

    let split_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("string.split() expects 2 arguments (string, delimiter).".to_string()); }
        let (KopiValue::String(s), KopiValue::String(delimiter)) = (&args[0], &args[1]) else {
            return Err("string.split() expects string arguments.".to_string());
        };
        let parts: Vec<KopiValue> = s.split(delimiter).map(|p| KopiValue::String(p.to_string())).collect();
        Ok(KopiValue::Array(Rc::new(RefCell::new(parts))))
    };

    let join_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("string.join() expects 2 arguments (array, separator).".to_string()); }
        let (KopiValue::Array(arr_rc), KopiValue::String(separator)) = (&args[0], &args[1]) else {
            return Err("string.join() expects an array and a string separator.".to_string());
        };
        let arr = arr_rc.borrow();
        let mut parts: Vec<String> = Vec::new();
        for item in arr.iter() {
            match item {
                KopiValue::String(s) => parts.push(s.clone()),
                _ => return Err("string.join() expects an array of strings.".to_string()),
            }
        }
        Ok(KopiValue::String(parts.join(separator)))
    };

    let replace_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 3 { return Err("string.replace() expects 3 arguments (string, from, to).".to_string()); }
        let (KopiValue::String(s), KopiValue::String(from), KopiValue::String(to)) = (&args[0], &args[1], &args[2]) else {
            return Err("string.replace() expects string arguments.".to_string());
        };
        Ok(KopiValue::String(s.replace(from, to)))
    };

    let contains_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("string.contains() expects 2 arguments (string, substring).".to_string()); }
        let (KopiValue::String(s), KopiValue::String(sub)) = (&args[0], &args[1]) else {
            return Err("string.contains() expects string arguments.".to_string());
        };
        Ok(KopiValue::Boolean(s.contains(sub)))
    };

    let starts_with_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("string.starts_with() expects 2 arguments (string, prefix).".to_string()); }
        let (KopiValue::String(s), KopiValue::String(prefix)) = (&args[0], &args[1]) else {
            return Err("string.starts_with() expects string arguments.".to_string());
        };
        Ok(KopiValue::Boolean(s.starts_with(prefix)))
    };

    let ends_with_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("string.ends_with() expects 2 arguments (string, suffix).".to_string()); }
        let (KopiValue::String(s), KopiValue::String(suffix)) = (&args[0], &args[1]) else {
            return Err("string.ends_with() expects string arguments.".to_string());
        };
        Ok(KopiValue::Boolean(s.ends_with(suffix)))
    };

    members.insert("to_upper".to_string(), KopiValue::new_native_fn("to_upper", to_upper_fn));
    members.insert("to_lower".to_string(), KopiValue::new_native_fn("to_lower", to_lower_fn));
    members.insert("trim".to_string(), KopiValue::new_native_fn("trim", trim_fn));
    members.insert("split".to_string(), KopiValue::new_native_fn("split", split_fn));
    members.insert("join".to_string(), KopiValue::new_native_fn("join", join_fn));
    members.insert("replace".to_string(), KopiValue::new_native_fn("replace", replace_fn));
    members.insert("contains".to_string(), KopiValue::new_native_fn("contains", contains_fn));
    members.insert("starts_with".to_string(), KopiValue::new_native_fn("starts_with", starts_with_fn));
    members.insert("ends_with".to_string(), KopiValue::new_native_fn("ends_with", ends_with_fn));

    members
}

pub fn create_math_module() -> KopiModule {
    let mut members = KopiModule::new();

    let abs_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.abs() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.abs())),
            _ => Err("math.abs() expects a number argument.".to_string()),
        }
    };

    let sqrt_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.sqrt() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => {
                if *n < 0.0 {
                    return Err("math.sqrt() cannot take negative numbers.".to_string());
                }
                Ok(KopiValue::Number(n.sqrt()))
            }
            _ => Err("math.sqrt() expects a number argument.".to_string()),
        }
    };

    let pow_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("math.pow() expects 2 arguments (base, exponent).".to_string()); }
        let (KopiValue::Number(base), KopiValue::Number(exponent)) = (&args[0], &args[1]) else {
            return Err("math.pow() expects number arguments.".to_string());
        };
        Ok(KopiValue::Number(base.powf(*exponent)))
    };

    let floor_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.floor() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.floor())),
            _ => Err("math.floor() expects a number argument.".to_string()),
        }
    };

    let ceil_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.ceil() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.ceil())),
            _ => Err("math.ceil() expects a number argument.".to_string()),
        }
    };

    let round_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.round() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.round())),
            _ => Err("math.round() expects a number argument.".to_string()),
        }
    };

    let sin_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.sin() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.sin())),
            _ => Err("math.sin() expects a number argument.".to_string()),
        }
    };

    let cos_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.cos() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.cos())),
            _ => Err("math.cos() expects a number argument.".to_string()),
        }
    };

    let tan_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("math.tan() expects 1 argument.".to_string()); }
        match &args[0] {
            KopiValue::Number(n) => Ok(KopiValue::Number(n.tan())),
            _ => Err("math.tan() expects a number argument.".to_string()),
        }
    };

    members.insert("abs".to_string(), KopiValue::new_native_fn("abs", abs_fn));
    members.insert("sqrt".to_string(), KopiValue::new_native_fn("sqrt", sqrt_fn));
    members.insert("pow".to_string(), KopiValue::new_native_fn("pow", pow_fn));
    members.insert("floor".to_string(), KopiValue::new_native_fn("floor", floor_fn));
    members.insert("ceil".to_string(), KopiValue::new_native_fn("ceil", ceil_fn));
    members.insert("round".to_string(), KopiValue::new_native_fn("round", round_fn));
    members.insert("sin".to_string(), KopiValue::new_native_fn("sin", sin_fn));
    members.insert("cos".to_string(), KopiValue::new_native_fn("cos", cos_fn));
    members.insert("tan".to_string(), KopiValue::new_native_fn("tan", tan_fn));

    members.insert("PI".to_string(), KopiValue::Number(std::f64::consts::PI));
    members.insert("E".to_string(), KopiValue::Number(std::f64::consts::E));

    members
}

pub fn create_array_module() -> KopiModule {
    let mut members = KopiModule::new();

    let push_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("array.push() expects 2 arguments (array, value).".to_string()); }
        if let KopiValue::Array(arr_rc) = &args[0] {
            arr_rc.borrow_mut().push(args[1].clone());
            Ok(KopiValue::Nil)
        } else {
            Err("array.push() expects an array as the first argument.".to_string())
        }
    };

    let pop_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 1 { return Err("array.pop() expects 1 argument (array).".to_string()); }
        if let KopiValue::Array(arr_rc) = &args[0] {
            match arr_rc.borrow_mut().pop() {
                Some(value) => Ok(value),
                None => Ok(KopiValue::Nil),
            }
        } else {
            Err("array.pop() expects an array as the first argument.".to_string())
        }
    };

    let remove_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("array.remove() expects 2 arguments (array, index).".to_string()); }
        let (KopiValue::Array(arr_rc), KopiValue::Number(index_f64)) = (&args[0], &args[1]) else {
            return Err("array.remove() expects an array and a number index.".to_string());
        };
        let index = *index_f64 as usize;
        let mut arr = arr_rc.borrow_mut();
        if index >= arr.len() {
            return Err(format!("Index {} out of bounds for array of length {}.", index, arr.len()));
        }
        let removed_value = arr.remove(index);
        Ok(removed_value)
    };

    let insert_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 3 { return Err("array.insert() expects 3 arguments (array, index, value).".to_string()); }
        let (KopiValue::Array(arr_rc), KopiValue::Number(index_f64), value) = (&args[0], &args[1], &args[2]) else {
            return Err("array.insert() expects an array, a number index, and a value.".to_string());
        };
        let index = *index_f64 as usize;
        let mut arr = arr_rc.borrow_mut();
        if index > arr.len() {
            return Err(format!("Index {} out of bounds for array of length {}.", index, arr.len()));
        }
        arr.insert(index, value.clone());
        Ok(KopiValue::Nil)
    };

    members.insert("push".to_string(), KopiValue::new_native_fn("push", push_fn));
    members.insert("pop".to_string(), KopiValue::new_native_fn("pop", pop_fn));
    members.insert("remove".to_string(), KopiValue::new_native_fn("remove", remove_fn));
    members.insert("insert".to_string(), KopiValue::new_native_fn("insert", insert_fn));

    members
}

fn parse_hex_color(hex_string: &str) -> Result<(u8, u8, u8), String> {
    if hex_string.len() != 6 {
        return Err("Hex color string must be 6 characters (e.g., 'FF0000').".to_string());
    }
    let r = u8::from_str_radix(&hex_string[0..2], 16)
        .map_err(|_| format!("Invalid red hex value: {}", &hex_string[0..2]))?;
    let g = u8::from_str_radix(&hex_string[2..4], 16)
        .map_err(|_| format!("Invalid green hex value: {}", &hex_string[2..4]))?;
    let b = u8::from_str_radix(&hex_string[4..6], 16)
        .map_err(|_| format!("Invalid blue hex value: {}", &hex_string[4..6]))?;
    Ok((r, g, b))
}

pub fn create_md_module() -> KopiModule {
    let mut members = KopiModule::new();

    let apply_sgr_code = |args: Vec<KopiValue>, code: u8| -> Result<KopiValue, String> {
        if args.len() != 1 {
            return Err(format!("md.<style>() expects 1 argument: a value to style.").to_string());
        }
        let text = args[0].to_string();
        Ok(KopiValue::String(format!("\x1b[{}m{}\x1b[0m", code, text)))
    };

    let bold_fn = move |args: Vec<KopiValue>| { apply_sgr_code(args, 1) };
    let italic_fn = move |args: Vec<KopiValue>| { apply_sgr_code(args, 3) };
    let underline_fn = move |args: Vec<KopiValue>| { apply_sgr_code(args, 4) };
    let strikethrough_fn = move |args: Vec<KopiValue>| { apply_sgr_code(args, 9) };


    let color_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 {
            return Err("md.color() expects 2 arguments: a value and a hex color string.".to_string());
        }
        let text = args[0].to_string();
        let hex_color = match &args[1] {
            KopiValue::String(s) => s,
            _ => return Err("Second argument to md.color() must be a hex color string.".to_string()),
        };

        let (r, g, b) = parse_hex_color(hex_color)?;
        Ok(KopiValue::String(format!("\x1b[38;2;{};{};{}m{}\x1b[0m", r, g, b, text)))
    };

    let bg_color_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 {
            return Err("md.bg_color() expects 2 arguments: a value and a hex color string.".to_string());
        }
        let text = args[0].to_string();
        let hex_color = match &args[1] {
            KopiValue::String(s) => s,
            _ => return Err("Second argument to md.bg_color() must be a hex color string.".to_string()),
        };

        let (r, g, b) = parse_hex_color(hex_color)?;
        Ok(KopiValue::String(format!("\x1b[48;2;{};{};{}m{}\x1b[0m", r, g, b, text)))
    };

    members.insert("bold".to_string(), KopiValue::new_native_fn("bold", bold_fn));
    members.insert("italic".to_string(), KopiValue::new_native_fn("italic", italic_fn));
    members.insert("underline".to_string(), KopiValue::new_native_fn("underline", underline_fn));
    members.insert("strikethrough".to_string(), KopiValue::new_native_fn("strikethrough", strikethrough_fn));
    members.insert("color".to_string(), KopiValue::new_native_fn("color", color_fn));
    members.insert("bg_color".to_string(), KopiValue::new_native_fn("bg_color", bg_color_fn));

    members
}

pub fn create_koptui_module() -> KopiModule {
    let mut members = KopiModule::new();

    let init_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        execute!(io::stdout(), terminal::EnterAlternateScreen, cursor::Hide)
            .map_err(|e| format!("Failed to enter alternate screen or hide cursor: {}", e))?;
        terminal::enable_raw_mode()
            .map_err(|e| format!("Failed to enable raw mode: {}", e))?;
        Ok(KopiValue::Nil)
    };

    let restore_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        execute!(io::stdout(), terminal::LeaveAlternateScreen, cursor::Show)
            .map_err(|e| format!("Failed to leave alternate screen or show cursor: {}", e))?;
        terminal::disable_raw_mode()
            .map_err(|e| format!("Failed to disable raw mode: {}", e))?;
        Ok(KopiValue::Nil)
    };

    let clear_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        execute!(io::stdout(), terminal::Clear(terminal::ClearType::All))
            .map_err(|e| format!("Failed to clear screen: {}", e))?;
        Ok(KopiValue::Nil)
    };

    let move_to_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 2 { return Err("koptui.move_to() expects 2 arguments (x, y).".to_string()); }
        let (KopiValue::Number(x_f64), KopiValue::Number(y_f64)) = (&args[0], &args[1]) else {
            return Err("koptui.move_to() expects number arguments for x and y.".to_string());
        };
        execute!(io::stdout(), cursor::MoveTo(*x_f64 as u16, *y_f64 as u16))
            .map_err(|e| format!("Failed to move cursor: {}", e))?;
        Ok(KopiValue::Nil)
    };

    let print_fn = |args: Vec<KopiValue>| -> Result<KopiValue, String> {
        if args.len() != 3 { return Err("koptui.print() expects 3 arguments (x, y, text).".to_string()); }
        let (KopiValue::Number(x_f64), KopiValue::Number(y_f64), text_val) = (&args[0], &args[1], &args[2]) else {
            return Err("koptui.print() expects number arguments for x and y, and a value for text.".to_string());
        };
        let text = text_val.to_string();

        execute!(io::stdout(),
            cursor::MoveTo(*x_f64 as u16, *y_f64 as u16),
            style::Print(text)
        ).map_err(|e| format!("Failed to print to screen: {}", e))?;
        Ok(KopiValue::Nil)
    };

    let read_key_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        match event::read() {
            Ok(Event::Key(KeyEvent { code, .. })) => {
                let key_str = match code {
                    KeyCode::Char(c) => c.to_string(),
                    KeyCode::Enter => "Enter".to_string(),
                    KeyCode::Left => "Left".to_string(),
                    KeyCode::Right => "Right".to_string(),
                    KeyCode::Up => "Up".to_string(),
                    KeyCode::Down => "Down".to_string(),
                    KeyCode::Esc => "Esc".to_string(),
                    KeyCode::Backspace => "Backspace".to_string(),
                    KeyCode::Delete => "Delete".to_string(),
                    KeyCode::Tab => "Tab".to_string(),
                    KeyCode::F(n) => format!("F{}", n),
                    _ => "Unknown".to_string(),
                };
                Ok(KopiValue::String(key_str))
            }
            Ok(_) => Ok(KopiValue::String("OtherEvent".to_string())),
            Err(e) => Err(format!("Failed to read event: {}", e)),
        }
    };

    let get_size_fn = |_args: Vec<KopiValue>| -> Result<KopiValue, String> {
        let (cols, rows) = terminal::size()
            .map_err(|e| format!("Failed to get terminal size: {}", e))?;
        let arr = vec![KopiValue::Number(cols as f64), KopiValue::Number(rows as f64)];
        Ok(KopiValue::Array(Rc::new(RefCell::new(arr))))
    };

    members.insert("init".to_string(), KopiValue::new_native_fn("init", init_fn));
    members.insert("restore".to_string(), KopiValue::new_native_fn("restore", restore_fn));
    members.insert("clear".to_string(), KopiValue::new_native_fn("clear", clear_fn));
    members.insert("move_to".to_string(), KopiValue::new_native_fn("move_to", move_to_fn));
    members.insert("print".to_string(), KopiValue::new_native_fn("print", print_fn));
    members.insert("read_key".to_string(), KopiValue::new_native_fn("read_key", read_key_fn));
    members.insert("get_size".to_string(), KopiValue::new_native_fn("get_size", get_size_fn));

    members
}
