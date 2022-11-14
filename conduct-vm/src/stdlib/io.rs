use conduct_tk::AHashMap;

use crate::{rt::Runtime, value::Value};

pub fn println(_: &mut Runtime, args: AHashMap<String, Value>) -> Value {
    let value = args.get("string").unwrap();
    let string = match value {
        Value::String(str) => str,
        _ => return Value::Nil,
    };
    println!("{}", string.get().expect("Null Pointer").value);
    Value::Nil
}

pub fn readln(rt: &mut Runtime, _: AHashMap<String, Value>) -> Value {
    let mut out = String::new();
    std::io::stdin().read_line(&mut out).expect("IO Error");

    Value::String(rt.alloc(out.trim().to_owned()))
}
