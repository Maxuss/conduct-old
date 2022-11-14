use crate::rt::Runtime;

mod io;
mod mem;

pub fn inject_stdlib(rt: &mut Runtime) {
    inject_io(rt);

    inject_mem(rt);
}

fn inject_io(rt: &mut Runtime) {
    rt.enforce_native_function("std.io", "println", vec!["string"], io::println);
    rt.enforce_native_function("std.io", "readln", Vec::<String>::new(), io::readln);
}

fn inject_mem(rt: &mut Runtime) {
    rt.enforce_native_function("std.mem", "ptr", vec!["value"], mem::ptr_of)
}
