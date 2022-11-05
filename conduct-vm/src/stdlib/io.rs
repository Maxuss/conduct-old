use conduct_tk::AHashMap;

use crate::{
    reg::{HeapValue, StackValue},
    vm::Vm,
};

pub fn stdio_println(vm: &mut Vm, args: AHashMap<String, StackValue>) -> StackValue {
    let ptr = args.get("string").unwrap();
    let ptr = match ptr {
        StackValue::HeapPointer(ptr) => *ptr,
        _ => return StackValue::Nil,
    };
    let value = vm.read_value(ptr);
    let string = match value {
        Some(HeapValue::String(str)) => str,
        _ => return StackValue::Nil,
    };
    println!("{}", string.as_ref());
    StackValue::Nil
}
