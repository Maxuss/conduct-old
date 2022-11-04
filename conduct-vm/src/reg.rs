use std::iter::repeat_with;

use crate::{op::Opcode, vm::Vm};

#[derive(Debug, Clone)]
pub struct Stack {
    inner: Vec<f64>,
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

    pub fn push(&mut self, value: f64) {
        self.top += 1;
        if self.top > self.size {
            panic!("Stack overflow") // TODO: replace panic with runtime error
        } else {
            self.inner.push(value)
        }
    }

    pub fn pop(&mut self) -> f64 {
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
    bytecode: &'c [u8],
    ip: usize,
    size: usize,
    stack: Stack,
}

macro_rules! read_buffer {
    ($vm:ident . $buffer:ident $from:expr) => {{
        let begin = $from;
        let mut end = begin;
        while $vm.$buffer[end] != 0x00 {
            end += 1
        }
        &$vm.$buffer[begin..end + 1]
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

    fn next_value<T>(&mut self) -> Option<T>
    where
        T: BytecodeStore,
    {
        T::retrieve(self)
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
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    let out = lh $op rh;
                    debug!(format!("{} {} {}", stringify!($kind), lh, rh));
                    self.stack.push(out);
                }
            };
        }

        self.ip = 0;
        while self.ip < self.size {
            match self.next_opcode().unwrap_or(Opcode::NOP) {
                Opcode::NOP => { /* noop */ }
                Opcode::HLT => break,
                Opcode::PUSH => {
                    let num: f64 = self.next_value()?;
                    debug!(format!("PUSH {}", num));
                    self.stack.push(num);
                }
                Opcode::POP => {
                    let popped = self.stack.pop();
                    debug!(format!("POP => {popped}"));
                }
                Opcode::ADD => bin_op!(ADD+),
                Opcode::SUB => bin_op!(SUB -),
                Opcode::MUL => bin_op!(MUL *),
                Opcode::DIV => bin_op!(DIV /),
                Opcode::NEG => {
                    let v = self.stack.pop();
                    debug!(format!("NEG {} {}", v, -v));
                    self.stack.push(-v)
                }
                Opcode::INC => {
                    let v = self.stack.pop();
                    debug!(format!("INC {} {}", v, v + 1.));
                    self.stack.push(v + 1.)
                }
                Opcode::DEC => {
                    let v = self.stack.pop();
                    debug!(format!("DEC {} {}", v, v - 1.));
                    self.stack.push(v - 1.)
                }
                Opcode::RDLOAD => {
                    let pos = self.stack.pop() as usize;
                    debug!(format!("RDLOAD {}", pos));
                    let index = vm.heap.len();
                    vm.heap.extend_from_slice(read_buffer!(vm.rodata pos)); // TODO: GC store ptr
                    self.stack.push(index as f64)
                }
                Opcode::HDEBUG => {
                    let pos = self.stack.pop() as usize;
                    debug!(format!("HDEBUG {}", pos));
                    let str = unsafe {
                        String::from_utf8_unchecked(read_buffer!(vm.heap pos).to_vec()).to_owned()
                    };
                    println!("{str}")
                }
                Opcode::CONCAT => {
                    let second = read_buffer!(vm.heap self.stack.pop() as usize);
                    let first = read_buffer!(vm.heap self.stack.pop() as usize);
                    let mut out: Vec<u8> = Vec::with_capacity(first.len() + second.len());
                    out.extend_from_slice(first);
                    out.pop();
                    out.extend_from_slice(second);
                    let ptr = vm.heap.len();
                    vm.heap.extend(out);
                    self.stack.push(ptr as f64);
                }
            }
        }
        Some(())
    }
}

pub trait BytecodeStore {
    fn retrieve<'c>(registry: &mut Registry<'c>) -> Option<Self>
    where
        Self: Sized;
}

impl BytecodeStore for u32 {
    fn retrieve<'c>(registry: &mut Registry<'c>) -> Option<Self>
    where
        Self: Sized,
    {
        let bytes = repeat_with(|| registry.next_byte())
            .take(4)
            .collect::<Option<Vec<u8>>>()?;
        Some(u32::from_be_bytes([bytes[0], bytes[1], bytes[2], bytes[3]]))
    }
}

impl BytecodeStore for f64 {
    fn retrieve<'c>(registry: &mut Registry<'c>) -> Option<Self>
    where
        Self: Sized,
    {
        let bytes = repeat_with(|| registry.next_byte())
            .take(8)
            .collect::<Option<Vec<u8>>>()?;
        Some(f64::from_be_bytes([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]))
    }
}
