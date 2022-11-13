use conduct_tk::AHashMap;

use crate::{rt::Runtime, value::Value};

pub fn stdio_println(_rt: &mut Runtime, args: AHashMap<String, Value>) -> Value {
    let value = args.get("string").unwrap();
    let string = match value {
        Value::String(str) => str,
        _ => return Value::Nil,
    };
    println!("{}", string.get().expect("Null Pointer").value);
    Value::Nil
}
