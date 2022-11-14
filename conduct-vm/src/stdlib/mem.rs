use conduct_tk::AHashMap;

use crate::{rt::Runtime, value::Value};

pub fn ptr_of(_: &mut Runtime, args: AHashMap<String, Value>) -> Value {
    let ptr = match args.get("value") {
        Some(Value::Array(arr)) => arr.get_ptr() as usize,
        Some(Value::Compound(cmp)) => cmp.get_ptr() as usize,
        Some(Value::Function(fun)) => fun.get_ptr() as usize,
        Some(Value::String(str)) => str.get_ptr() as usize,
        _ => return Value::Nil,
    };
    Value::Number(ptr as f64)
}
