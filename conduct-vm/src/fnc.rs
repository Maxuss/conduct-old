use std::cell::UnsafeCell;

use conduct_tk::AHashMap;

use crate::{
    ffi::NativeFunctionDescriptor,
    heap::{Allocated, Gc, Trace},
    rt::Runtime,
    value::Value,
    vm::Variable,
};

#[derive(Debug, Clone)]
pub struct FunctionDescriptor {
    pub params: Vec<String>,
    pub module: Gc<Module>,
    pub ptr: usize,
}

#[derive(Debug, Clone)]
pub struct CallFrame {
    pub base_counter: usize,
    pub callable: Gc<FunctionDescriptor>,
    pub bytecode: *const Chunk,
    pub ip: *const u8,
}

impl CallFrame {
    pub fn new(callable: Gc<FunctionDescriptor>, base_counter: usize) -> Self {
        let chunk: *const Chunk = callable.module.chunk(callable.ptr);
        let ip = callable.module.chunk(callable.ptr).as_ptr();
        Self {
            base_counter,
            callable,
            bytecode: chunk,
            ip,
        }
    }

    fn chunk(&self) -> &Chunk {
        // We use unsafe here because it's way faster
        // This is safe, because we have `Root<Closure>` which eventually has a `Gc<Import>`.
        unsafe { &*self.bytecode }
    }

    pub fn load_ip(&self) -> *const u8 {
        self.ip
    }

    pub fn store_ip(&mut self, ip: *const u8) {
        self.ip = ip;
    }

    pub fn set_pc(&mut self, value: usize) {
        unsafe {
            self.ip = self.chunk().as_slice().as_ptr().add(value);
        }
    }
}

impl Trace for CallFrame {
    fn trace(&self) {
        self.callable.trace()
    }
}

macro_rules! unmanaged_ptr {
    ($v:expr) => {
        Gc::from_ptr(Box::into_raw(Box::new(Allocated::new($v))))
    };
}

#[derive(Debug, Default)]
pub struct Module {
    chunks: Vec<Chunk>,
    constants: Vec<Constant>,
    globals: UnsafeCell<AHashMap<String, Variable>>,
    native_variables: UnsafeCell<AHashMap<String, Value>>,
    native_functions: UnsafeCell<AHashMap<String, NativeFunctionDescriptor>>,
}

impl Module {
    pub fn new() -> Self {
        Self::default()
    }

    #[inline]
    pub fn chunk(&self, index: usize) -> &Chunk {
        unsafe { self.chunks.get_unchecked(index) }
    }

    pub fn chunk_mut(&mut self, index: usize) -> &mut Chunk {
        &mut self.chunks[index]
    }

    pub fn add_chunk(&mut self) -> usize {
        self.chunks.push(Chunk::new());
        self.chunks.len() - 1
    }

    pub fn add_constant(&mut self, constant: Constant) -> usize {
        self.constants.push(constant);
        self.constants.len() - 1
    }

    pub fn add_global(&self, name: String, mutable: bool, value: Value) {
        let globals = unsafe { &mut *self.globals.get() };
        globals.insert(
            name,
            Variable {
                name: unmanaged_ptr!("<global>".to_owned()),
                mutable,
                value,
            },
        );
    }

    pub fn set_global(&self, name: String, value: Value) {
        let globals = unsafe { &mut *self.globals.get() };
        match globals.get_mut(&name) {
            Some(v) => {
                if v.mutable {
                    v.value = value
                } else {
                    panic!("Constant Assignment") // TODO: runtime error here
                }
            }
            _ => {
                panic!("Undefined Assignment") // TODO: runtime error here too
            }
        }
    }

    pub fn get_global(&self, name: String) -> Value {
        let globals = unsafe { &*self.globals.get() };
        match globals.get(&name) {
            Some(v) => v.value,
            _ => Value::Nil,
        }
    }

    pub fn get_native_constant(&self, name: String) -> Value {
        let natives = unsafe { &*self.native_variables.get() };
        match natives.get(&name) {
            Some(v) => *v,
            _ => Value::Nil,
        }
    }

    pub fn native_argc(&self, name: &String) -> Option<usize> {
        let natives = unsafe { &*self.native_functions.get() };
        match natives.get(name) {
            Some(v) => Some(v.arity.len()),
            _ => None,
        }
    }

    pub fn call_native_function(
        &self,
        name: String,
        rt: &mut Runtime,
        args: Vec<Value>,
    ) -> Option<Value> {
        let natives = unsafe { &*self.native_functions.get() };
        match natives.get(&name) {
            Some(v) => {
                if v.arity.len() != args.len() {
                    return None;
                } else {
                    let mut out = AHashMap::with_capacity(args.len());
                    let mut index = 0;
                    for each in &v.arity {
                        out.insert(each.to_owned(), args[index]);
                        index += 1;
                    }
                    let callable = v.callable;
                    Some(callable(rt, out))
                }
            }
            _ => Some(Value::Nil),
        }
    }

    pub fn add_native_function(
        &self,
        module: &String,
        name: String,
        params: Vec<String>,
        callable: fn(&mut Runtime, AHashMap<String, Value>) -> Value,
    ) {
        let natives = unsafe { &mut *self.native_functions.get() };
        natives.insert(
            name,
            NativeFunctionDescriptor {
                callable: callable,
                arity: params,
                module: module.clone(),
            },
        );
    }

    pub fn add_native_constant(&self, name: String, value: Value) {
        let natives = unsafe { &mut *self.native_variables.get() };
        natives.insert(name, value);
    }

    #[inline]
    pub fn constant(&self, index: usize) -> &Constant {
        &self.constants[index]
    }

    pub fn chunks(&self) -> &[Chunk] {
        &self.chunks
    }

    pub fn constants(&self) -> &[Constant] {
        &self.constants
    }
}

impl Trace for Module {
    fn trace(&self) {
        for value in &self.constants {
            value.value.trace()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Chunk {
    pub bytecode: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { bytecode: vec![] }
    }

    pub fn as_slice(&self) -> &[u8] {
        &self.bytecode
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.bytecode.as_ptr()
    }
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
    pub value: Value,
}
