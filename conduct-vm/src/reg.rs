use std::{fmt::Display, io::Read, iter::repeat_with};

use internment::Intern;

use crate::{
    op::Opcode,
    vm::{HeapPtr, Vm},
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
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum HeapValue {
    String(Intern<String>),
}

impl HeapValue {
    pub fn string(self) -> Option<Intern<String>> {
        match self {
            Self::String(str) => Some(str),
        }
    }

    pub fn type_id(&self) -> u8 {
        match self {
            HeapValue::String(_) => 0x01,
        }
    }

    pub fn read_from(vec: &[u8]) -> Option<Self> {
        match vec[0] {
            0x01 => Some(HeapValue::String(Intern::new(
                String::from_utf8(vec[1..vec.len() - 1].to_vec()).ok()?,
            ))),
            _ => None,
        }
    }

    pub fn store_to(&self, heap: &mut Vec<u8>) {
        match self {
            HeapValue::String(str) => {
                heap.push(0x01);
                heap.extend(str.as_bytes());
                heap.push(0x00);
            }
        }
    }
}

impl Display for HeapValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HeapValue::String(str) => write!(f, "{}", str.as_ref()),
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
            panic!("Stack underflow") // TODO: replacae panic with runtime error
        } else {
            self.top -= 1;
            self.inner.pop().unwrap()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Registry<'c> {
    pub bytecode: &'c [u8],
    pub ip: usize,
    pub size: usize,
    pub stack: Stack,
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

impl<'c> Registry<'c> {
    pub fn new(bytecode: &'c [u8]) -> Self {
        Self {
            bytecode,
            ip: 0,
            size: bytecode.len(),
            stack: Stack::new(256),
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
            match self.next_opcode().unwrap_or(Opcode::NOP) {
                Opcode::NOP => { /* noop */ }
                Opcode::HLT => break,
                Opcode::PUSH => {
                    let type_index = self.next_byte()?;
                    let value = match type_index {
                        0x00 => {
                            // a number, parse a float
                            let mut buffer = [0; 8];
                            let handle = repeat_with(|| self.next_byte())
                                .take(8)
                                .collect::<Option<Vec<u8>>>()?;
                            let mut handle = handle.take(8);
                            handle.read(&mut buffer).unwrap();
                            StackValue::Number(f64::from_be_bytes(buffer))
                        }
                        0x01 => {
                            // a string, parse it
                            let handle = repeat_with(|| self.next_byte())
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
                            let handle = repeat_with(|| self.next_byte())
                                .take(8)
                                .collect::<Option<Vec<u8>>>()?;
                            let mut handle = handle.take(8);
                            handle.read(&mut buffer).unwrap();
                            StackValue::HeapPointer(u64::from_be_bytes(buffer) as HeapPtr)
                        }
                        0x03 => {
                            // nil value
                            let _ = self.next_byte();
                            StackValue::Nil
                        }
                        _ => {
                            panic!("Unknown value type: 0x{:2x}", type_index)
                        }
                    };
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
                            val_a == val_b
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
            }
        }
        Some(())
    }
}
