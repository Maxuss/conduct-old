use std::{fmt::Display, io::Read, iter::repeat_with, vec};

use conduct_tk::AHashMap;
use internment::Intern;

use crate::{
    asm,
    fnc::FunctionDescriptor,
    op::Opcode,
    vm::{HeapPtr, Variable, Vm},
};

#[derive(Debug, Copy, Clone)]
pub enum StackValue {
    Nil,
    Number(f64),
    Boolean(bool),
    HeapPointer(usize),
}

impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::HeapPointer(ptr) => write!(f, "0x{:02x}", *ptr),
            Self::Boolean(bool) => write!(f, "{}", bool),
            StackValue::Nil => write!(f, "nil"),
        }
    }
}

impl StackValue {
    fn number(&self) -> Option<f64> {
        match self {
            StackValue::Number(num) => Some(*num),
            StackValue::Nil => Some(0.),
            _ => None,
        }
    }

    fn ptr(&self) -> Option<usize> {
        match self {
            StackValue::HeapPointer(ptr) => Some(*ptr),
            _ => None,
        }
    }

    fn bool(&self) -> Option<bool> {
        match self {
            StackValue::Boolean(bool) => Some(*bool),
            StackValue::Nil => Some(false),
            _ => None,
        }
    }

    fn read_from(vm: &mut Vm, reg: &mut Registry) -> Option<StackValue> {
        let type_index = reg.next_byte()?;
        Some(match type_index {
            0x00 => {
                // a number, parse a float
                let mut buffer = [0; 8];
                let handle = repeat_with(|| reg.next_byte())
                    .take(8)
                    .collect::<Option<Vec<u8>>>()?;
                let mut handle = handle.take(8);
                handle.read(&mut buffer).unwrap();
                StackValue::Number(f64::from_be_bytes(buffer))
            }
            0x01 => {
                // a string, parse it
                let handle = repeat_with(|| reg.next_byte())
                    .take_while(|v| !matches!(v, Some(0x00)))
                    .collect::<Option<Vec<u8>>>()?;
                let ptr = vm.heap.len();
                vm.heap.push(0x01); // string id
                vm.heap.extend(handle);
                vm.heap.push(0x00);
                StackValue::HeapPointer(ptr)
            }
            0x02 => {
                // a pointer, parse a float
                let mut buffer = [0; 8];
                let handle = repeat_with(|| reg.next_byte())
                    .take(8)
                    .collect::<Option<Vec<u8>>>()?;
                let mut handle = handle.take(8);
                handle.read(&mut buffer).unwrap();
                StackValue::HeapPointer(u64::from_be_bytes(buffer) as HeapPtr)
            }
            0x03 => {
                // nil value
                StackValue::Nil
            }
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HeapValue {
    String(Intern<String>),
    Function(FunctionDescriptor),
}

impl HeapValue {
    pub fn string(self) -> Option<Intern<String>> {
        match self {
            Self::String(str) => Some(str),
            _ => None,
        }
    }

    pub fn type_id(&self) -> u8 {
        match self {
            HeapValue::String(_) => 0x01,
            HeapValue::Function(_) => 0x02,
        }
    }

    pub fn read_from(vec: &[u8]) -> Option<Self> {
        match vec[0] {
            0x01 => Some(HeapValue::String(Intern::new(
                String::from_utf8(vec[1..vec.len() - 1].to_vec()).ok()?,
            ))),
            0x02 => {
                let slice = vec[1..vec.len() - 1].to_vec();
                let len = slice[0] as usize;
                let module = String::from_utf8(slice[1..len + 1].to_vec()).ok()?;

                let slice = &slice[len + 1..];
                let param_count = slice[0] as usize;
                let mut params: Vec<String> = Vec::with_capacity(param_count);
                let mut index = 1;
                for _ in 0..param_count {
                    let begin = index + 1;
                    let string =
                        String::from_utf8(slice[begin..begin + slice[index] as usize].to_vec())
                            .ok()?;
                    index += slice[index] as usize + 1;
                    params.push(string);
                }
                let body = slice[index..].to_vec();
                Some(HeapValue::Function(FunctionDescriptor {
                    params,
                    bytecode_chunk: body,
                    module,
                }))
            }
            _ => None,
        }
    }

    pub fn store_to(&self, heap: &mut Vec<u8>) {
        match self {
            HeapValue::String(str) => {
                heap.push(0x01);
                heap.extend_from_slice(str.as_bytes());
                heap.push(0x00);
            }
            HeapValue::Function(func) => {
                heap.push(0x02);
                heap.push(func.module.len() as u8);
                heap.extend_from_slice(func.module.as_bytes());
                heap.push(func.params.len() as u8);
                for param in &func.params {
                    heap.push(param.len() as u8);
                    heap.extend_from_slice(param.as_bytes());
                }
                heap.push(0x00)
            }
        }
    }
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::String(str) => write!(f, "{}", str.as_ref()),
            HeapValue::Function(fun) => write!(f, "<fn {:?}>", fun.params),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Stack {
    inner: Vec<StackValue>,
    top: usize,
    size: usize,
}

impl Stack {
    pub fn new(size: usize) -> Self {
        Self {
            inner: Vec::with_capacity(size),
            top: 0,
            size,
        }
    }

    pub fn push(&mut self, v: StackValue) {
        self.top += 1;
        if self.top > self.size {
            panic!("Stack overflow") // TODO: replace panic with runtime error
        } else {
            self.inner.push(v)
        }
    }

    #[inline(always)]
    pub fn push_bool(&mut self, bool: bool) {
        self.push(StackValue::Boolean(bool))
    }

    #[inline(always)]
    pub fn push_nil(&mut self) {
        self.push(StackValue::Nil)
    }

    #[inline(always)]
    pub fn push_num(&mut self, value: f64) {
        self.push(StackValue::Number(value))
    }

    #[inline(always)]
    pub fn push_ptr(&mut self, value: usize) {
        self.push(StackValue::HeapPointer(value))
    }

    pub fn pop(&mut self) -> StackValue {
        if self.top == 0 {
            panic!("Stack underflow") // TODO: replace panic with runtime error
        } else {
            self.top -= 1;
            self.inner.pop().unwrap()
        }
    }

    pub fn peek(&mut self) -> StackValue {
        if self.top <= 1 {
            panic!("Stack underflow")
        } else {
            self.inner[self.top - 1]
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

    pub fn define<S: Into<String>>(&mut self, name: S, mutable: bool, value: StackValue) {
        let scope = &mut self.inner[self.depth - 1];
        scope
            .variables
            .insert(name.into(), Variable { mutable, value });
    }

    pub fn set<S: Into<String>>(&mut self, name: S, value: StackValue) -> bool {
        let key = name.into();
        let scope = self
            .inner
            .iter_mut()
            .find(|scope| scope.variables.contains_key(&key));
        if let Some(scope) = scope {
            let var = scope.variables[&key];
            if var.mutable {
                scope.variables.insert(
                    key,
                    Variable {
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

    pub fn get<S: Into<String>>(&mut self, name: S) -> Option<StackValue> {
        let key = name.into();
        let scope = self
            .inner
            .iter()
            .find(|scope| scope.variables.contains_key(&key));
        if let Some(scope) = scope {
            let var = scope.variables.get(&key);
            if let Some(var) = var {
                Some(var.value)
            } else {
                None
            }
        } else {
            None
        }
    }
}

#[derive(Debug, Clone)]
pub struct Registry {
    pub bytecode: Vec<u8>,
    pub ip: usize,
    pub size: usize,
    pub scopes: ScopeStack,
    pub stack: Stack,
    pub module: String,
}

macro_rules! read_buffer {
    ($vm:ident.$buffer:ident, $ptr:expr) => {{
        let begin = $ptr;
        let mut end = begin;
        while $vm.$buffer[end] != 0x00 {
            end += 1
        }
        &$vm.$buffer[begin..end + 1]
    }};
}

macro_rules! read_heap {
    ($vm:ident, $ptr:expr) => {{
        HeapValue::read_from(read_buffer!($vm.heap, $ptr))
    }};
}

impl Registry {
    pub fn new(bytecode: Vec<u8>) -> Self {
        let len = bytecode.len();
        Self {
            bytecode,
            ip: 0,
            size: len,
            stack: Stack::new(256),
            scopes: ScopeStack::new(),
            module: "<unknown>".to_owned(),
        }
    }

    fn next_opcode(&mut self) -> Option<Opcode> {
        self.ip += 1;
        if self.ip >= self.size {
            None
        } else {
            Some(self.bytecode[self.ip].into())
        }
    }

    fn next_byte(&mut self) -> Option<u8> {
        self.ip += 1;
        if self.ip >= self.size {
            None
        } else {
            Some(self.bytecode[self.ip].into())
        }
    }

    /// reads a string from bytecode **without adding it to heap**
    fn next_bc_str(&mut self) -> Option<String> {
        // a string, parse it
        let handle = repeat_with(|| self.next_byte())
            .take_while(|v| !matches!(v, Some(0x00)))
            .collect::<Option<Vec<u8>>>()?;
        String::from_utf8(handle).ok()
    }
    pub fn run(&mut self, vm: &mut Vm) -> Option<()> {
        macro_rules! debug {
            ($msg:expr) => {
                if cfg!(debug_assertions) {
                    println!("{}", $msg);
                }
            };
        }

        macro_rules! bin_op {
            ($kind:ident $op:tt) => {
                {
                    {
                        let _unused = Opcode::$kind;
                        std::mem::drop(_unused)
                    }
                    let rh = self.stack.pop().number().unwrap();
                    let lh = self.stack.pop().number().unwrap();
                    let out = lh $op rh;
                    debug!(format!("{} {} {}", stringify!($kind), lh, rh));
                    self.stack.push_num(out);
                }
            };
        }

        self.ip = 0;
        while self.ip < self.size {
            let next = self.next_opcode().unwrap_or(Opcode::NOP);
            match next {
                Opcode::NOP => { /* noop */ }
                Opcode::HLT => break,
                Opcode::PUSH => {
                    let value = StackValue::read_from(vm, self)?;
                    debug!(format!("PUSH {}", value));
                    self.stack.push(value);
                }
                Opcode::POP => {
                    let popped = self.stack.pop();
                    debug!(format!("POP => {popped}"));
                }
                Opcode::ADD => bin_op!(ADD+),
                Opcode::SUB => bin_op!(SUB -),
                Opcode::MUL => bin_op!(MUL *),
                Opcode::DIV => bin_op!(DIV /),
                Opcode::NEG => match self.stack.pop() {
                    StackValue::Boolean(bool) => {
                        debug!(format!("NEG {} {}", bool, !bool));
                        self.stack.push_bool(!bool)
                    }
                    StackValue::Number(v) => {
                        debug!(format!("NEG {} {}", v, -v));
                        self.stack.push_num(-v);
                    }
                    _ => return None,
                },
                Opcode::INC => {
                    let v = self.stack.pop().number()?;
                    debug!(format!("INC {} {}", v, v + 1.));
                    self.stack.push_num(v + 1.)
                }
                Opcode::DEC => {
                    let v = self.stack.pop().number()?;
                    debug!(format!("DEC {} {}", v, v - 1.));
                    self.stack.push_num(v - 1.)
                }
                Opcode::HDEBUG => {
                    let pos = self.stack.pop().ptr()?;
                    debug!(format!("HDEBUG 0x{:02x}", pos));
                    let str = unsafe {
                        String::from_utf8_unchecked(read_buffer!(vm.heap, pos).to_vec()).to_owned()
                    };
                    println!("{str}")
                }
                Opcode::CONCAT => {
                    let second = read_buffer!(vm.heap, self.stack.pop().ptr()?);
                    let first = read_buffer!(vm.heap, self.stack.pop().ptr()?);
                    let mut out: Vec<u8> = Vec::with_capacity(first.len() + second.len());
                    out.push(0x01);
                    out.extend_from_slice(&first[1..]);
                    out.pop();
                    out.extend_from_slice(&second[1..]);
                    let ptr = vm.heap.len();
                    vm.heap.extend(out);
                    self.stack.push_ptr(ptr);
                }
                Opcode::EQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("EQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            val_a == val_b || ptr_a == ptr_b
                        }
                        (StackValue::Boolean(a), StackValue::Boolean(b)) => a == b,
                        (StackValue::Number(a), StackValue::Number(b)) => a == b,
                        (any, StackValue::Nil) => matches!(any, StackValue::Nil),
                        (StackValue::Nil, any) => matches!(any, StackValue::Nil),
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::NEQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("NEQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            val_a != val_b
                        }
                        (StackValue::Boolean(a), StackValue::Boolean(b)) => a != b,
                        (StackValue::Number(a), StackValue::Number(b)) => a != b,
                        (any, StackValue::Nil) => !matches!(any, StackValue::Nil),
                        (StackValue::Nil, any) => !matches!(any, StackValue::Nil),
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::LT => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("LT {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            match (val_a, val_b) {
                                (HeapValue::String(a), HeapValue::String(b)) => a.len() < b.len(),
                                _ => false,
                            }
                        }
                        (StackValue::Number(a), StackValue::Number(b)) => a < b,
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::GT => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("GT {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            match (val_a, val_b) {
                                (HeapValue::String(a), HeapValue::String(b)) => a.len() > b.len(),
                                _ => false,
                            }
                        }
                        (StackValue::Number(a), StackValue::Number(b)) => a > b,
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::LTQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("LTQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            match (val_a, val_b) {
                                (HeapValue::String(a), HeapValue::String(b)) => a.len() <= b.len(),
                                _ => false,
                            }
                        }
                        (StackValue::Number(a), StackValue::Number(b)) => a <= b,
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::GTQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("GTQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (StackValue::HeapPointer(ptr_a), StackValue::HeapPointer(ptr_b)) => {
                            let val_a = read_heap!(vm, ptr_a)?;
                            let val_b = read_heap!(vm, ptr_b)?;
                            match (val_a, val_b) {
                                (HeapValue::String(a), HeapValue::String(b)) => a.len() >= b.len(),
                                _ => false,
                            }
                        }
                        (StackValue::Number(a), StackValue::Number(b)) => a >= b,
                        _ => false,
                    };
                    self.stack.push_bool(tag)
                }
                Opcode::DEBUG => match self.stack.pop() {
                    StackValue::HeapPointer(ptr) => {
                        println!("{}", read_heap!(vm, ptr)?);
                    }
                    other => println!("{}", other),
                },
                Opcode::DEF_GLOBAL_CONST => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("DEF_GLOBAL_CONST {} {}", name, value));
                    vm.add_global_variable(name.as_ref(), false, value)
                }
                Opcode::DEF_GLOBAL_MUT => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("DEF_GLOBAL_MUT {} {}", name, value));
                    vm.add_global_variable(name.as_ref(), true, value)
                }
                Opcode::SET_GLOBAL => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("SET_GLOBAL {} {}", name, value));
                    vm.set_global_variable(name.as_ref(), value);
                }
                Opcode::LOAD_GLOBAL => {
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let value = match vm.get_global_variable(name.as_ref()) {
                        Some(v) => v,
                        None => StackValue::Nil,
                    };
                    debug!(format!("LOAD_GLOBAL {} => {}", name, value));
                    self.stack.push(value);
                }
                Opcode::LOAD_NATIVE => {
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let value = match vm.get_native_variable(name.as_ref()) {
                        Some(v) => v,
                        None => StackValue::Nil,
                    };
                    debug!(format!("LOAD_NATIVE {} => {}", name, value));
                    self.stack.push(value);
                }
                Opcode::DEF_LOCAL_CONST => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("DEF_LOCAL_CONST {} {}", name, value));
                    self.scopes.define(name.as_ref(), false, value)
                }
                Opcode::DEF_LOCAL_MUT => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("DEF_LOCAL_MUT {} {}", name, value));
                    self.scopes.define(name.as_ref(), true, value)
                }
                Opcode::SET_LOCAL => {
                    let value = self.stack.pop();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("SET_LOCAL {} {}", name, value));
                    self.scopes.set(name.as_ref(), value);
                }
                Opcode::LOAD_LOCAL => {
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let value = match self.scopes.get(name.as_ref()) {
                        Some(value) => value,
                        None => StackValue::Nil,
                    };
                    debug!(format!("LOAD_LOCAL {} => {}", name, value));
                    self.stack.push(value);
                }
                Opcode::PUSH_SCOPE => {
                    debug!("PUSH_SCOPE");
                    self.scopes.push();
                }
                Opcode::POP_SCOPE => {
                    debug!("POP_SCOPE");
                    self.scopes.pop();
                }
                Opcode::ASSERT => {
                    let flag = self.stack.pop().bool()?;
                    assert!(flag, "{}", "Assertion Failed")
                }
                Opcode::JMP_IF => {
                    let bool_flag = self.stack.pop().bool()?;
                    let value = u16::from_be_bytes([self.next_byte()?, self.next_byte()?]);
                    debug!(format!("JMP_IF {bool_flag} {value}"));
                    if bool_flag {
                        self.ip += value as usize
                    }
                }
                Opcode::JMPF => {
                    let value = u16::from_be_bytes([self.next_byte()?, self.next_byte()?]);
                    debug!(format!("JMPF {value}"));
                    self.ip += value as usize;
                }
                Opcode::JMPB => {
                    let value = u16::from_be_bytes([self.next_byte()?, self.next_byte()?]);
                    debug!(format!("JMPB {value}"));
                    self.ip -= value as usize;
                }
                Opcode::CALL_NATIVE => {
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let argc = vm.get_native_argc(name.as_ref().to_owned()).len();
                    let mut args = repeat_with(|| self.stack.pop())
                        .take(argc)
                        .collect::<Vec<StackValue>>();
                    args.reverse();
                    debug!(format!("CALL_NATIVE {} {:?}", name, args));
                    self.stack
                        .push(vm.call_native_function(name.as_ref(), args)?);
                }
                Opcode::IMPORT => {
                    let handle = repeat_with(|| self.next_byte())
                        .take_while(|v| !matches!(v, Some(0x00)))
                        .collect::<Option<Vec<u8>>>()?;
                    let path = String::from_utf8(handle[1..].to_vec()).ok()?;
                    debug!(format!("IMPORT {}", path));
                    vm.import(path);
                }
                Opcode::FUNCTION => {
                    let arg_size = self.next_byte()?;
                    let mut params = repeat_with(|| {
                        let heap = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                        Some(heap)
                    })
                    .take(arg_size as usize)
                    .map(|each| each.map(|it| it.as_ref().to_owned()))
                    .collect::<Option<Vec<String>>>()?;
                    params.reverse();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let body_size =
                        u16::from_be_bytes([self.next_byte()?, self.next_byte()?]) as usize;
                    let body = self.bytecode[self.ip + 1..self.ip + body_size].to_vec();
                    self.ip += body_size - 1;

                    debug!(format!(
                        "FUNCTION {} {:?} <body {}>",
                        name, params, body_size
                    ));

                    let desc = FunctionDescriptor {
                        params,
                        bytecode_chunk: body,
                        module: self.module.clone(),
                    };
                    vm.add_function(name.as_ref(), desc);
                }
                Opcode::MODULE => {
                    let module = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    debug!(format!("MODULE {}", module));
                    vm.import(module.as_ref());
                    self.module = module.as_ref().to_owned();
                }
                Opcode::CALL => {
                    let arg_size = self.next_byte()?;
                    let mut args = repeat_with(|| self.stack.pop())
                        .take(arg_size as usize)
                        .collect::<Vec<StackValue>>();
                    args.reverse();
                    let name = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                    let (descriptor, args) =
                        if let Some((descriptor, args)) = vm.get_function(name.as_ref(), args) {
                            (descriptor, args)
                        } else {
                            self.stack.push(StackValue::Nil);
                            continue;
                        };

                    debug!(format!("CALL {} {:?}", name, args));
                    descriptor.prepare_call_site(self, args);

                    let new_position = self.bytecode.len() + 2;
                    let previous_position = self.ip as u16;
                    let prev_bytes: [u8; 2] = previous_position.to_be_bytes();
                    let new_bytes: [u8; 2] = previous_position.to_be_bytes();
                    self.size += descriptor.bytecode_chunk.len() + 7;
                    self.bytecode.extend(asm! {
                        SPLIT [new_bytes.to_vec()]
                    });
                    self.bytecode.extend(descriptor.bytecode_chunk);
                    self.bytecode.extend(asm! {
                        POP_SCOPE
                        JMPA [prev_bytes.to_vec()]
                    });
                    self.ip = new_position;
                }
                Opcode::JMPA => {
                    let position =
                        u16::from_be_bytes([self.next_byte()?, self.next_byte()?]) as usize;
                    debug!(format!("JMPA {position}"));
                    self.ip = position;
                }
                Opcode::SPLIT => {
                    let position =
                        u16::from_be_bytes([self.next_byte()?, self.next_byte()?]) as usize;
                    debug!(format!("SPLIT {position}"));
                    let split = self.bytecode.split_off(position);
                    self.size -= split.len();
                    drop(split);
                }
                Opcode::INLINE_FUNCTION => {
                    let arg_size = self.next_byte()?;
                    let mut params = repeat_with(|| {
                        let heap = read_heap!(vm, self.stack.pop().ptr()?)?.string()?;
                        Some(heap)
                    })
                    .take(arg_size as usize)
                    .map(|each| each.map(|it| it.as_ref().to_owned()))
                    .collect::<Option<Vec<String>>>()?;
                    params.reverse();
                    let body_size =
                        u16::from_be_bytes([self.next_byte()?, self.next_byte()?]) as usize;
                    let body = self.bytecode[self.ip + 1..self.ip + body_size].to_vec();
                    self.ip += body_size - 1;

                    debug!(format!("INLINE_FUNCTION {:?} <body {}>", params, body_size));

                    let desc = FunctionDescriptor {
                        params,
                        bytecode_chunk: body,
                        module: self.module.clone(),
                    };
                    let heap_value = HeapValue::Function(desc);
                    let ptr = vm.heap.len();
                    println!("STORING AT {ptr}");
                    heap_value.store_to(&mut vm.heap);
                    self.stack.push(StackValue::HeapPointer(ptr))
                }
            }
        }
        Some(())
    }
}
