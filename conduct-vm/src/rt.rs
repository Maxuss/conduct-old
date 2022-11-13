use std::{cell::RefCell, iter::repeat_with, vec};

use conduct_tk::AHashMap;

use crate::fnc::Constant;
use crate::fnc::FunctionDescriptor;
use crate::{
    bc::BytecodeManager,
    fnc::Module,
    heap::{Gc, Heap, Trace},
    op::Opcode,
    stdlib,
    value::Value,
    vm::{Variable, DEFAULT_STACK_SIZE},
};

#[derive(Debug, Clone)]
pub struct Stack {
    top: *mut Value,
    bottom: *mut Value,
    _stack: Box<[Value]>,
}

impl Stack {
    pub fn new(size: usize) -> Self {
        let mut stack = vec![Value::Nil; size].into_boxed_slice();
        Self {
            top: stack.as_mut_ptr(),
            bottom: stack.as_mut_ptr(),
            _stack: stack,
        }
    }

    pub fn len(&self) -> usize {
        unsafe { self.top.offset_from(self.bottom) as usize }
    }

    pub fn truncate(&mut self, top: usize) {
        unsafe {
            self.top = self.bottom.add(top);
        }
    }

    pub fn push(&mut self, value: Value) {
        unsafe {
            std::ptr::write(self.top, value);
            self.top = self.top.add(1);
        }
    }

    pub fn pop(&mut self) -> Value {
        unsafe {
            self.top = self.top.sub(1);
            std::ptr::read(self.top)
        }
    }

    pub fn set(&mut self, index: usize, value: Value) {
        unsafe {
            let ptr = self.bottom.add(index);
            std::ptr::write(ptr, value);
        }
    }

    pub fn get(&self, index: usize) -> &Value {
        unsafe { &*self.bottom.add(index) }
    }

    pub fn peek_n(&self, n: usize) -> &Value {
        unsafe { &*self.top.sub(n + 1) }
    }

    pub fn pop_n(&mut self, n: usize) -> Vec<Value> {
        unsafe {
            self.top = self.top.sub(n);
            let slice = std::ptr::slice_from_raw_parts(self.top, n);
            (*slice).into()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub variables: AHashMap<String, Variable>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            variables: AHashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ScopeStack {
    pub inner: Vec<Scope>,
    pub depth: usize,
}

impl ScopeStack {
    pub fn new() -> Self {
        Self {
            inner: vec![],
            depth: 0,
        }
    }

    pub fn push(&mut self) {
        self.depth += 1;
        let scope = Scope::new();
        self.inner.push(scope);
    }

    pub fn pop(&mut self) -> Scope {
        self.depth -= 1;
        self.inner.pop().unwrap()
    }

    pub fn define<S: Into<String>>(
        &mut self,
        rt: &mut Runtime,
        name: S,
        mutable: bool,
        value: Value,
    ) {
        let scope = &mut self.inner[self.depth - 1];
        let name = name.into();
        let gc_name = rt.alloc(name.clone());
        scope.variables.insert(
            name,
            Variable {
                name: gc_name,
                mutable,
                value,
            },
        );
    }

    pub fn set<S: Into<String>>(&mut self, rt: &mut Runtime, name: S, value: Value) -> bool {
        let key = name.into();
        let scope = self
            .inner
            .iter_mut()
            .find(|scope| scope.variables.contains_key(&key));
        if let Some(scope) = scope {
            let var = scope.variables.get(&key).unwrap();
            if var.mutable {
                scope.variables.insert(
                    key.clone(),
                    Variable {
                        name: rt.alloc(key),
                        mutable: true,
                        value,
                    },
                );
            }
            true
        } else {
            false
        }
    }

    pub fn get<S: Into<String>>(&mut self, name: S) -> Option<Value> {
        let key = name.into();
        let scope = self
            .inner
            .iter()
            .find(|scope| scope.variables.contains_key(&key));
        if let Some(scope) = scope {
            let var = scope.variables.get(&key);
            if let Some(var) = var {
                Some(var.value.clone())
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug)]
pub struct Runtime {
    pub bytecode: BytecodeManager,
    pub imports: AHashMap<String, Gc<Module>>,
    pub stack: Stack,
    pub module: String,
    pub heap: RefCell<Heap>,
    pub ip: *const u8,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            bytecode: BytecodeManager::new(),
            imports: AHashMap::new(),
            stack: Stack::new(DEFAULT_STACK_SIZE),
            module: "nil".to_owned(),
            heap: RefCell::new(Heap::new()),
            ip: std::ptr::null(),
        }
    }

    pub fn prepare(&mut self) {
        // native functions
        self.enforce_native_function(
            "std.io",
            "println",
            vec!["string".to_owned()],
            stdlib::io::stdio_println,
        );
    }

    pub fn alloc<T: Trace>(&mut self, data: T) -> Gc<T> {
        let mut heap = self.heap.borrow_mut();
        // TODO: call gc here
        heap.allocate(data)
    }

    pub fn with_module(&mut self, module: Module) {
        let closure = self.prepare_interpret(module);
        self.stack.push(Value::Function(closure.clone()));
        self.bytecode
            .begin_frame(closure.to_owned(), self.stack.len());
        self.load_ip();
    }

    fn prepare_interpret(&mut self, module: Module) -> Gc<FunctionDescriptor> {
        let import = self.alloc(module);
        self.imports.insert("self".into(), import);

        let function = FunctionDescriptor {
            params: vec![],
            module: import,
            ptr: 0,
        };

        self.alloc(function)
    }

    pub fn store_ip(&mut self) {
        self.bytecode.last_frame_mut().store_ip(self.ip);
    }

    pub fn load_ip(&mut self) {
        self.ip = self.bytecode.last_frame().load_ip();
    }

    pub fn set_ip(&mut self, to: usize) {
        self.bytecode.last_frame_mut().set_pc(to);
        self.load_ip();
    }

    pub fn inc_ip(&mut self, by: usize) {
        let last_frame = self.bytecode.last_frame_mut();
        last_frame.set_pc(self.ip as usize + by);
        self.load_ip();
    }

    pub fn current_module(&self) -> Gc<Module> {
        self.bytecode.last_frame().callable.module
    }

    pub fn next_opcode(&mut self) -> Opcode {
        self.next_byte().into()
    }

    pub fn push_string(&mut self, string: impl Into<String>) {
        let root = self.alloc(string.into());
        self.stack.push(Value::String(root));
    }

    pub fn next_u32(&mut self) -> u32 {
        unsafe {
            let slice = &*std::ptr::slice_from_raw_parts(self.ip, 4);
            let value = u32::from_le_bytes(slice.try_into().unwrap());
            self.ip = self.ip.add(4);
            value
        }
    }

    pub fn next_byte(&mut self) -> u8 {
        unsafe {
            let value = std::ptr::read(self.ip);
            self.ip = self.ip.add(1);
            value
        }
    }

    /// reads a string from bytecode **without adding it to heap**
    fn next_bc_str(&mut self) -> Option<String> {
        // a string, parse it
        let handle = repeat_with(|| self.next_byte())
            .take_while(|v| *v != 0x00)
            .collect::<Vec<u8>>();
        String::from_utf8(handle).ok()
    }

    pub fn define_global_constant(&mut self, value: Constant) {
        let mut module = self.current_module();
        module.add_constant(value);
    }

    pub fn define_global<S: Into<String>>(&mut self, name: S, value: Variable) {
        let module = self.current_module();
        module.add_global(name.into(), value.mutable, value.value);
    }

    pub fn set_global<S: Into<String>>(&mut self, name: S, value: Value) {
        let module = self.current_module();
        module.set_global(name.into(), value)
    }

    pub fn get_global<S: Into<String>>(&self, name: S) -> Value {
        let module = self.current_module();
        module.get_global(name.into())
    }

    pub fn add_native_const<S: Into<String>>(&self, name: S, value: Value) {
        let module = self.current_module();
        module.add_native_constant(name.into(), value)
    }

    pub fn add_native_function<S: Into<String>>(
        &self,
        name: S,
        params: Vec<impl Into<String>>,
        callable: fn(&mut Runtime, AHashMap<String, Value>) -> Value,
    ) {
        let module = self.current_module();
        module.add_native_function(
            &self.module,
            name.into(),
            params.into_iter().map(|each| each.into()).collect(),
            callable,
        )
    }

    pub fn enforce_native_function<S: Into<String>>(
        &self,
        module_name: S,
        name: S,
        params: Vec<impl Into<String>>,
        callable: fn(&mut Runtime, AHashMap<String, Value>) -> Value,
    ) {
        let module = self.current_module();
        module.add_native_function(
            &module_name.into(),
            name.into(),
            params.into_iter().map(|each| each.into()).collect(),
            callable,
        )
    }

    pub fn get_native_const<S: Into<String>>(&self, name: S) -> Value {
        let module = self.current_module();
        module.get_native_constant(name.into())
    }

    pub fn call_native_function<S: Into<String>>(
        &mut self,
        name: S,
        args: Vec<Value>,
    ) -> Option<Value> {
        let module = self.current_module();
        module.call_native_function(name.into(), self, args)
    }

    pub fn define_local<S: Into<String>>(&mut self, name: S, mutable: bool, value: Value) -> usize {
        let index = self.next_u32() as usize;
        let index = self.bytecode.last_frame().base_counter + index;
        let name = self.alloc(name.into());
        let alloc = self.alloc(Variable {
            name,
            mutable,
            value,
        });
        self.stack.set(index, Value::Variable(alloc));
        index
    }

    pub fn set_local(&mut self) {
        let index = self.next_u32() as usize;
        let index = self.bytecode.last_frame().base_counter + index;
        let value = self.stack.peek_n(0);
        match self.stack.get(index) {
            Value::Variable(var) => {
                if !var.mutable {
                    panic!("Constant Assignment") // TODO: runtime error
                } else {
                    let alloc = self.alloc(Variable {
                        name: var.name.clone(),
                        mutable: var.mutable,
                        value: value.to_owned(),
                    });
                    self.stack.set(index, Value::Variable(alloc))
                }
            }
            _ => panic!("Invalid Assignment"),
        }
    }

    pub fn get_local(&mut self) -> Value {
        let index = self.next_u32() as usize;

        let index = self.bytecode.last_frame().base_counter + index;
        match self.stack.get(index) {
            Value::Variable(var) => var.value.to_owned(),
            other => {
                // assume that we are just in a function, and this is an arg
                *other
            }
        }
    }

    pub fn add_function<S: Into<String>>(
        &mut self,
        name: S,
        desc: Gc<FunctionDescriptor>,
    ) -> usize {
        self.define_local(name.into(), false, Value::Function(desc))
    }

    pub fn preload_function_bytecode(&mut self, bc: Vec<u8>) -> usize {
        let mut module = self.current_module();
        module.add_chunk();
        let ptr = module.chunks().len();
        module.chunk_mut(ptr - 1).bytecode = bc;
        ptr
    }
}
