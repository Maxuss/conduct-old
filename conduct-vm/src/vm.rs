pub const DEFAULT_HEAP_CAPACITY: usize = 1024 * 4;
pub const DEFAULT_STACK_SIZE: usize = 256;
pub const DEFAULT_CALLFRAME_LEN: usize = 64;

use crate::{
    fnc::{FunctionDescriptor, Module},
    heap::{Gc, Trace},
    rt::Runtime,
    value::Value,
};

pub type HeapPtr = usize;

pub struct Vm {
    pub rt: Runtime,
}

impl Vm {
    pub fn new_bc(bytecode: &[u8]) -> Self {
        let mut rt = Runtime::new();
        let mut module = Module::new();
        module.add_chunk();
        module.chunk_mut(0).bytecode = bytecode.to_vec();

        let desc = FunctionDescriptor {
            params: vec![],
            module: rt.alloc(module),
            ptr: 0,
        };

        let desc = rt.alloc(desc);
        rt.bytecode.begin_frame(desc, 1);

        Self { rt }
    }

    pub fn new() -> Self {
        let mut rt = Runtime::new();
        let mut module = Module::new();
        module.add_chunk();

        let desc = FunctionDescriptor {
            params: vec![],
            module: rt.alloc(module),
            ptr: 0,
        };

        let desc = rt.alloc(desc);
        rt.bytecode.begin_frame(desc, 1);

        Self { rt }
    }

    pub fn with_bc(&mut self, bytecode: Vec<u8>) {
        let last_frame = self.rt.bytecode.last_frame_mut();
        let index = last_frame.callable.module.chunks().len() - 1;
        last_frame.callable.module.chunk_mut(index).bytecode = bytecode;
        last_frame.ip = last_frame
            .callable
            .module
            .chunk(last_frame.callable.ptr)
            .as_ptr();
        last_frame.bytecode = last_frame.callable.module.chunk(last_frame.callable.ptr);
        self.rt.load_ip();
    }

    pub fn run(mut self) -> Option<()> {
        self.rt.prepare();
        self.rt.load_ip();
        self.rt.run()
    }
}

#[derive(Debug, Clone)]
pub struct LocalVariable {
    pub name: Gc<String>,
    pub mutable: bool,
    pub value: Value,
}

#[derive(Debug, Clone)]
pub struct Variable {
    pub name: Gc<String>,
    pub mutable: bool,
    pub value: Value,
}

impl Trace for Variable {
    fn trace(&self) {
        self.name.trace();
        self.value.trace();
    }
}
