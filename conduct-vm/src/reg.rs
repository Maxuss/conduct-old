use std::{fmt::Display, io::Read, iter::repeat_with, vec};

use conduct_tk::AHashMap;
use internment::Intern;

use crate::{
    asm,
    fnc::FunctionDescriptor,
    op::Opcode,
    vm::{HeapPtr, Variable, Vm},
};

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum StackValue {
    Nil,
    Number(f64),
    Boolean(bool),
    Range(f64, f64),
    HeapPointer(usize),
}

impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Number(num) => write!(f, "{}", num),
            Self::HeapPointer(ptr) => write!(f, "0x{:02x}", *ptr),
            Self::Boolean(bool) => write!(f, "{}", bool),
            Self::Nil => write!(f, "nil"),
            Self::Range(begin, end) => write!(f, "{begin}..{end}"),
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

    fn slice(&self) -> Option<(f64, f64)> {
        match self {
            StackValue::Range(begin, end) => Some((*begin, *end)),
            _ => None,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            StackValue::Nil => 0x00,
            StackValue::Number(_) => 8,
            StackValue::Boolean(_) => 1,
            StackValue::Range(_, _) => 16,
            StackValue::HeapPointer(_) => 8,
        }
    }

    pub fn read_from_heap(slice: &[u8]) -> Option<StackValue> {
        Some(match slice[0] {
            0x01 => {
                let mut val_bytes: [u8; 8] = Default::default();
                let bytes = &slice[1..];
                val_bytes.copy_from_slice(bytes);
                StackValue::Number(f64::from_be_bytes(val_bytes))
            }
            0x02 => StackValue::Boolean(match slice[1] {
                0x00 => false,
                _ => true,
            }),
            0x03 => {
                let mut val_bytes: [u8; 8] = Default::default();
                let bytes = &slice[1..=8];
                val_bytes.copy_from_slice(bytes);
                let begin = u64::from_be_bytes(val_bytes) as f64;
                let bytes = &slice[8..=16];
                val_bytes.copy_from_slice(bytes);
                let end = u64::from_be_bytes(val_bytes) as f64;
                StackValue::Range(begin, end)
            }
            0x04 => {
                let mut val_bytes: [u8; 8] = Default::default();
                let bytes = &slice[1..];
                val_bytes.copy_from_slice(bytes);
                StackValue::HeapPointer(u64::from_be_bytes(val_bytes) as usize)
            }
            _ => StackValue::Nil,
        })
    }

    pub fn store_to_heap(&self, heap: &mut Vec<u8>) {
        let size_bytes = (self.size() as i32 + 1).to_be_bytes();
        heap.extend_from_slice(&size_bytes);

        let type_index: u8 = match self {
            StackValue::Nil => 0x00,
            StackValue::Number(_) => 0x01,
            StackValue::Boolean(_) => 0x02,
            StackValue::Range(_, _) => 0x03,
            StackValue::HeapPointer(_) => 0x04,
        };
        heap.push(type_index);
        match self {
            StackValue::Nil => { /* nop */ }
            StackValue::Number(num) => heap.extend_from_slice(&num.to_be_bytes()),
            StackValue::Boolean(bool) => heap.push(*bool as u8),
            StackValue::Range(begin, end) => {
                heap.extend_from_slice(&begin.to_be_bytes());
                heap.extend_from_slice(&end.to_be_bytes());
            }
            StackValue::HeapPointer(ptr) => heap.extend_from_slice(&(*ptr as u64).to_be_bytes()),
        }
    }

    pub fn read_from_bc(vm: &mut Vm, reg: &mut Registry) -> Option<StackValue> {
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
                let heapval = HeapValue::String(Intern::new(String::from_utf8(handle).ok()?));
                let ptr = vm.alloc(heapval);
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
            0x04 => {
                // a slice
                let mut begin_buffer = [0; 8];
                let handle = repeat_with(|| reg.next_byte())
                    .take(8)
                    .collect::<Option<Vec<u8>>>()?;
                let mut handle = handle.take(8);
                handle.read(&mut begin_buffer).unwrap();
                let begin = u64::from_be_bytes(begin_buffer) as f64;

                let mut end_buffer = [0; 8];
                let handle = repeat_with(|| reg.next_byte())
                    .take(8)
                    .collect::<Option<Vec<u8>>>()?;
                let mut handle = handle.take(8);
                handle.read(&mut end_buffer).unwrap();
                let end = u64::from_be_bytes(end_buffer) as f64;

                StackValue::Range(begin, end)
            }
            _ => return None,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum HeapValue {
    String(Intern<String>),
    Function(FunctionDescriptor),
    Array(Vec<StackValue>),
    Compound(AHashMap<Intern<String>, StackValue>),
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
            HeapValue::Array(_) => 0x03,
            HeapValue::Compound(_) => 0x04,
        }
    }

    pub fn read_from(vec: &[u8]) -> Option<Self> {
        match vec[0] {
            0x01 => Some(HeapValue::String(Intern::new(
                String::from_utf8(vec[1..].to_vec()).ok()?,
            ))),
            0x02 => {
                let slice = vec[1..].to_vec();
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
            0x03 => {
                let slice = vec[1..].to_vec();
                let mut index = 0;
                let mut size_bytes: [u8; 8] = Default::default();
                let size_slice = &slice[index..index + 8];
                size_bytes.copy_from_slice(size_slice);
                let size = u64::from_be_bytes(size_bytes) as usize;
                index += 8;
                let elements = repeat_with(|| {
                    let mut size_bytes: [u8; 4] = Default::default();
                    let size_slice = &slice[index..index + 4];
                    size_bytes.copy_from_slice(size_slice);
                    index += 4;
                    let size = u32::from_be_bytes(size_bytes) as usize;
                    let data = &slice.get(index..index + size)?;
                    index += size;
                    StackValue::read_from_heap(data)
                })
                .take(size)
                .collect::<Option<Vec<StackValue>>>()?;
                Some(HeapValue::Array(elements))
            }
            0x04 => {
                let slice = vec[1..].to_vec();
                let mut index = 0;
                let mut size_bytes: [u8; 8] = Default::default();
                let size_slice = &slice[index..index + 8];
                size_bytes.copy_from_slice(size_slice);
                let size = u64::from_be_bytes(size_bytes) as usize;
                index += 8;
                let elements = repeat_with(|| {
                    let mut size_bytes: [u8; 4] = Default::default();
                    let size_slice = &slice[index..index + 4];
                    size_bytes.copy_from_slice(size_slice);
                    index += 4;
                    let size = u32::from_be_bytes(size_bytes) as usize;
                    let key = String::from_utf8(slice.get(index..index + size)?.to_vec()).ok()?;
                    index += size;

                    let mut size_bytes: [u8; 4] = Default::default();
                    let size_slice = &slice[index..index + 4];
                    size_bytes.copy_from_slice(size_slice);
                    index += 4;
                    let size = u32::from_be_bytes(size_bytes) as usize;
                    let value = StackValue::read_from_heap(&slice[index..index + size].to_vec())?;
                    index += size;
                    Some((Intern::new(key), value))
                })
                .take(size)
                .collect::<Option<Vec<(Intern<String>, StackValue)>>>()?;
                let map = AHashMap::from_iter(elements);
                Some(HeapValue::Compound(map))
            }
            _ => None,
        }
    }

    pub fn size(&self) -> usize {
        match self {
            HeapValue::String(str) => str.len(),
            HeapValue::Function(func) => {
                let params_len: usize = func.params.iter().map(|each| each.len() + 1).sum();
                let total_len = 2usize + func.module.len() + params_len + func.bytecode_chunk.len();
                total_len
            }
            HeapValue::Array(array) => {
                8usize + array.iter().map(|each| each.size() + 5).sum::<usize>()
            }
            HeapValue::Compound(cmp) => {
                8usize
                    + cmp
                        .iter()
                        .map(|(key, value)| key.len() + value.size() + 8)
                        .sum::<usize>()
            }
        }
    }

    pub fn store_to(&self, heap: &mut Vec<u8>) {
        match self {
            HeapValue::String(str) => {
                let size_bytes = ((str.len() + 1usize) as u64).to_be_bytes();
                heap.extend(size_bytes);
                heap.push(0x01);
                heap.extend_from_slice(str.as_bytes());
            }
            HeapValue::Function(func) => {
                let params_len: usize = func.params.iter().map(|each| each.len() + 1).sum();
                let total_len = 3usize + func.module.len() + params_len + func.bytecode_chunk.len();
                let size_bytes = (total_len as u64).to_be_bytes();
                heap.extend_from_slice(&size_bytes);
                heap.push(0x02);
                heap.push(func.module.len() as u8);
                heap.extend_from_slice(func.module.as_bytes());
                heap.push(func.params.len() as u8);
                for param in &func.params {
                    heap.push(param.len() as u8);
                    heap.extend_from_slice(param.as_bytes());
                }
                heap.extend_from_slice(&func.bytecode_chunk);
            }
            HeapValue::Array(arr) => {
                let len = 9usize + arr.iter().map(|each| each.size() + 5).sum::<usize>();
                let size_bytes = (len as u64).to_be_bytes();
                heap.extend_from_slice(&size_bytes);
                heap.push(0x03);
                let size_bytes = (arr.len() as u64).to_be_bytes();
                heap.extend_from_slice(&size_bytes);
                for element in arr {
                    element.store_to_heap(heap)
                }
            }
            HeapValue::Compound(cmp) => {
                let len = 9usize
                    + cmp
                        .iter()
                        .map(|(key, value)| key.len() + value.size() + 9)
                        .sum::<usize>();
                let size_bytes = (len as u64).to_be_bytes();
                heap.extend_from_slice(&size_bytes);
                heap.push(0x04);
                let size_bytes = (cmp.len() as u64).to_be_bytes();
                heap.extend_from_slice(&size_bytes);
                for (key, value) in cmp {
                    let size_bytes = (key.len() as u32).to_be_bytes();
                    heap.extend_from_slice(&size_bytes);
                    heap.extend_from_slice(key.as_bytes());
                    value.store_to_heap(heap);
                }
            }
        }
    }
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::String(str) => write!(f, "{}", str.as_ref()),
            HeapValue::Function(fun) => write!(f, "<fn {:?}>", fun.params),
            HeapValue::Array(arr) => write!(f, "{arr:?}"),
            HeapValue::Compound(cmp) => write!(f, "{cmp:?}"),
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
        let mut size_bytes: [u8; 8] = Default::default();
        let mem_slice = &$vm.$buffer[begin..begin + 8];
        size_bytes.copy_from_slice(mem_slice);
        let size = u64::from_be_bytes(size_bytes) as usize;
        let slice = &$vm.$buffer[begin + 8..begin + 8 + size];
        slice
    }};
}

macro_rules! read_heap {
    ($vm:ident, $ptr:expr) => {{
        $vm.read_value($ptr)
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
                    let value = StackValue::read_from_bc(vm, self)?;
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
                    out.extend_from_slice(&first[1..]);
                    out.extend_from_slice(&second[1..]);
                    let handle = String::from_utf8(out).ok()?;
                    let ptr = vm.heap.len();
                    HeapValue::String(Intern::new(handle)).store_to(&mut vm.heap);
                    debug!(format!("CONCAT => 0x{ptr:2x}"));
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
                        println!("{}", vm.read_value(ptr)?);
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
                    match vm.read_value(self.stack.pop().ptr()?)? {
                        HeapValue::String(name) => {
                            let (descriptor, args) = if let Some((descriptor, args)) =
                                vm.get_function(name.as_ref(), args)
                            {
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
                        HeapValue::Function(inline) => {
                            let mut argv = AHashMap::new();
                            let mut index = 0;
                            for param in &inline.params {
                                argv.insert(param.to_owned(), args[index]);
                                index += 1;
                            }
                            debug!(format!("CALL <inlined> {:?}", args));
                            inline.prepare_call_site(self, argv);

                            let new_position = self.bytecode.len() + 2;
                            let previous_position = self.ip as u16;
                            let prev_bytes: [u8; 2] = previous_position.to_be_bytes();
                            let new_bytes: [u8; 2] = previous_position.to_be_bytes();
                            self.size += inline.bytecode_chunk.len() + 7;
                            self.bytecode.extend(asm! {
                                SPLIT [new_bytes.to_vec()]
                            });
                            self.bytecode.extend(inline.bytecode_chunk);
                            self.bytecode.extend(asm! {
                                POP_SCOPE
                                JMPA [prev_bytes.to_vec()]
                            });
                            self.ip = new_position;
                        }
                        _ => {
                            // TODO: throw compile error here
                        }
                    }
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
                Opcode::CLOSURE => {
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
                Opcode::RANGE => {
                    let end = self.stack.pop().number()?;
                    let begin = self.stack.pop().number()?;
                    debug!(format!("RANGE {begin} {end}"));
                    self.stack.push(StackValue::Range(begin, end))
                }
                Opcode::ARRAY => {
                    let size = self.stack.pop().number()? as usize;
                    let mut array = repeat_with(|| self.stack.pop())
                        .take(size)
                        .collect::<Vec<StackValue>>();
                    array.reverse();
                    let value = HeapValue::Array(array);
                    debug!(format!("LIST {value:?}"));
                    let ptr = vm.alloc(value);
                    self.stack.push(StackValue::HeapPointer(ptr))
                }
                Opcode::INDEX => {
                    let value = self.stack.pop();
                    let indexee = vm.read_value(self.stack.pop().ptr()?)?;
                    debug!(format!("INDEX {value} {indexee}"));
                    match (value, indexee) {
                        (StackValue::Number(index), HeapValue::String(str)) => {
                            let char = str.chars().nth(index as usize)?.to_string();
                            let ptr = vm.alloc(HeapValue::String(Intern::new(char)));
                            self.stack.push(StackValue::HeapPointer(ptr));
                        }
                        (StackValue::Number(index), HeapValue::Array(arr)) => {
                            self.stack.push(arr[index as usize]);
                        }
                        (StackValue::Number(index), HeapValue::Compound(cmp)) => {
                            let index = index as usize;
                            self.stack.push(
                                cmp.iter()
                                    .nth(index)
                                    .map(|it| *it.1)
                                    .unwrap_or(StackValue::Nil),
                            )
                        }
                        (StackValue::Range(begin, end), HeapValue::String(str)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let char = str[begin..end].to_string();
                            let ptr = vm.alloc(HeapValue::String(Intern::new(char)));
                            self.stack.push(StackValue::HeapPointer(ptr));
                        }
                        (StackValue::Range(begin, end), HeapValue::Array(arr)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let slice = arr[begin..end].to_vec();
                            let ptr = vm.alloc(HeapValue::Array(slice));
                            self.stack.push(StackValue::HeapPointer(ptr));
                        }
                        (StackValue::Range(begin, end), HeapValue::Compound(cmp)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let slice = cmp.iter().map(|each| (*each.0, *each.1)).collect::<Vec<(
                                Intern<String>,
                                StackValue,
                            )>>(
                            )[begin..end]
                                .to_vec();
                            let ptr = vm.alloc(HeapValue::Compound(AHashMap::from_iter(slice)));
                            self.stack.push(StackValue::HeapPointer(ptr))
                        }
                        (StackValue::HeapPointer(sptr), HeapValue::Compound(cmp)) => {
                            let str = vm.read_value(sptr)?.string()?;
                            let value = *cmp.get(&str).unwrap_or(&StackValue::Nil);
                            self.stack.push(value);
                        }
                        (indexer, indexee) => {
                            // TODO: change to runtime error later
                            panic!("Invalid indexation: ({indexer} {indexee})")
                        }
                    }
                }
                Opcode::COMPOUND => {
                    let size = self.stack.pop().number()? as usize;
                    let mut elements = repeat_with(|| {
                        let value = self.stack.pop();
                        let ptr = self.stack.pop().ptr()?;
                        let key = vm.read_value(ptr)?.string()?;
                        Some((key, value))
                    })
                    .take(size)
                    .collect::<Option<Vec<(Intern<String>, StackValue)>>>()?;
                    elements.reverse();
                    let map = AHashMap::from_iter(elements);
                    let ptr = vm.alloc(HeapValue::Compound(map));
                    self.stack.push(StackValue::HeapPointer(ptr))
                }
            }
        }
        Some(())
    }
}
