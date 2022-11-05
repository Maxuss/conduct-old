use conduct_tk::AHashMap;

use crate::reg::{HeapValue, Registry, StackValue};

pub type HeapPtr = usize;

#[derive(Clone)]
pub struct Vm<'c> {
    pub heap: Vec<u8>,
    pub registry: Registry<'c>,
    native_values: AHashMap<String, fn(&mut Vm) -> Variable>,
    global_variables: AHashMap<String, Variable>,
}

impl<'c> Vm<'c> {
    pub fn new() -> Self {
        Self {
            heap: Vec::with_capacity(1024 * 8),
            registry: Registry::new(&[0]),
            native_values: AHashMap::new(),
            global_variables: AHashMap::new(),
        }
    }

    pub fn alloc(&mut self, value: HeapValue) -> HeapPtr {
        let ptr = self.heap.len();
        value.store_to(&mut self.heap);
        ptr
    }

    pub fn read_value(&mut self, ptr: HeapPtr) -> Option<HeapValue> {
        let begin = ptr;
        let mut end = begin;
        while self.heap[end] != 0x00 {
            end += 1
        }
        let slice = &self.heap[begin..end + 1];

        HeapValue::read_from(slice)
    }

    pub fn add_native_value<S: Into<String>>(
        &mut self,
        named: S,
        provider: fn(&mut Vm) -> Variable,
    ) {
        self.native_values.insert(named.into(), provider);
    }

    pub fn add_global_variable<S: Into<String>>(
        &mut self,
        named: S,
        mutable: bool,
        value: StackValue,
    ) {
        self.global_variables
            .insert(named.into(), Variable { mutable, value });
    }

    pub fn set_global_variable<S: Into<String>>(&mut self, named: S, value: StackValue) {
        let name = named.into();
        if self.global_variables.contains_key(&name) && self.global_variables[&name].mutable {
            self.global_variables.insert(
                name,
                Variable {
                    mutable: true,
                    value,
                },
            );
        }
    }

    pub fn get_global_variable<S: Into<String>>(&mut self, named: S) -> Option<StackValue> {
        match self.global_variables.entry(named.into()) {
            std::collections::hash_map::Entry::Occupied(occupied) => Some(occupied.get().value),
            std::collections::hash_map::Entry::Vacant(_) => None,
        }
    }

    pub fn run(mut self, bytecode: &'c [u8]) {
        let stack = self.registry.stack.clone();
        let mut c = Registry {
            bytecode,
            ip: 0,
            size: bytecode.len(),
            stack: stack,
        };
        c.run(&mut self);
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub value: StackValue,
}
