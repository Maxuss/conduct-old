use std::iter::repeat_with;

use conduct_tk::AHashMap;

use crate::{fnc::FunctionDescriptor, rt::Runtime, value::Value, vm::Variable};

macro_rules! opcodes {
    (
        $(
            $bc:literal $name:ident
        )*
    ) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
        #[repr(u8)]
        pub enum Opcode {
            $(
                $name = $bc
            ),*
        }

        impl Opcode {
            pub fn bytecode(&self) -> u8 {
                match self {
                    $(
                        Self::$name => $bc
                    ),*
                }
            }
        }

        impl From<u8> for Opcode {
            fn from(v: u8) -> Self {
                match v {
                    $(
                        $bc => Self::$name,
                    )*
                    _ => Self::HLT
                }
            }
        }

        impl Into<u8> for Opcode {
            fn into(self) -> u8 {
                self as u8
            }
        }
    };
}

opcodes! {
    0x00 NOP // noop
    0x01 HLT // halts the execution
    0x02 PUSH // pushes value
    0x03 POP // pops value
    0x04 ADD // adds values
    0x05 SUB // subtracts values
    0x06 MUL // multiplies values
    0x07 DIV // divides values
    0x08 NEG // negates values
    0x09 INC // increments a value
    0x0A DEC // decrements a value
    0x0B CONCAT // concatenates two strings from heap and pushes new string to heap
    0x0C EQ // equal
    0x0D NEQ // not equal
    0x0E LT // less than
    0x0F GT // greater than
    0x10 LTQ // less than or equal
    0x11 GTQ // greater than or equal
    0x12 DEF_GLOBAL_CONST // defines a global constant
    0x13 DEF_GLOBAL_MUT // defines a global mutable variable
    0x14 SET_GLOBAL // sets a global variable
    0x15 LOAD_GLOBAL // loads a global variable
    0x16 LOAD_NATIVE // loads a native variable
    0x17 DEF_LOCAL_CONST // defines a local constant
    0x18 DEF_LOCAL_MUT // defines a local mutable variable
    0x19 SET_LOCAL // sets a local variable
    0x1A LOAD_LOCAL // loads value of a local variable
    0x1B LEGACY_RESERVED_1 // pushes current scope value forward
    0x1C LEGACY_RESERVED_2 // pops current scope value
    0x1D JMP_IF // jumps if bool flag is true
    0x1E LEGACY_RESERVED_3 // jumps forward
    0x1F LEGACY_RESERVED_4 // jumps backward
    0x20 CALL_NATIVE // calls a native function
    0x21 IMPORT // imports a module
    0x22 FUNCTION // declares a function
    0x23 MODULE // declares current module
    0x24 CALL // calls a function
    0x25 JMPA // absolute jump
    0x26 LEGACY_RESERVED_5 // splits off part of the bytecode at provided position
    0x27 CLOSURE // constructs a function literal value
    0x28 RANGE // constructs a range from two floats
    0x29 ARRAY // allocates a new list to the heap
    0x2A INDEX // indexes into the provided array/compound
    0x2B COMPOUND // allocates a new compound to the heap
    0x2C RETURN // returns from the scope
    0xFD ASSERT // asserts that the boolean flag is true
    0xFE DEBUG // debug prints current stack value
}

#[macro_export]
macro_rules! asm {
    (
        $($op:ident $([$value:expr])? $(*$ptr:literal)?)+
    ) => {{
        #[allow(unused_imports)]
        use $crate::op::IntoAsm;
        let mut buf: Vec<u8> = Vec::new();
        $(
            $crate::asm!(@buf => $op $([$value])? $(*$ptr)?);
        )+
        buf
    }};
    (
        @$buf:ident => $op:ident [$value:expr]
    ) => {
        $buf.push(Opcode::$op.into());
        $buf.extend_from_slice(&$value.into_asm());
    };
    (
        @$buf:ident => $op:ident *$value:literal
    ) => {
        $buf.push(Opcode::$op.into());
        $buf.extend(&($value as usize).into_asm());
    };
    (
        @$buf:ident => $op:ident
    ) => {
        $buf.push(Opcode::$op.into());
    }
}
pub trait IntoAsm {
    fn into_asm(self) -> Vec<u8>;
}

impl IntoAsm for Vec<u8> {
    fn into_asm(self) -> Vec<u8> {
        self
    }
}

impl IntoAsm for u8 {
    fn into_asm(self) -> Vec<u8> {
        vec![0x00, self]
    }
}

impl IntoAsm for i32 {
    fn into_asm(self) -> Vec<u8> {
        (self as f64).into_asm()
    }
}

impl IntoAsm for u64 {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x00];
        out.extend(self.to_be_bytes());
        out
    }
}

impl IntoAsm for usize {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x02];
        out.extend(self.to_be_bytes());
        out
    }
}

impl IntoAsm for core::ops::Range<i64> {
    fn into_asm(self) -> Vec<u8> {
        let mut buf = vec![0x04];
        buf.extend_from_slice(&(self.start as u64).to_be_bytes());
        buf.extend_from_slice(&(self.end as u64).to_be_bytes());
        buf
    }
}

impl IntoAsm for core::ops::Range<i32> {
    fn into_asm(self) -> Vec<u8> {
        let mut buf = vec![0x04];
        buf.extend_from_slice(&(self.start as u64).to_be_bytes());
        buf.extend_from_slice(&(self.end as u64).to_be_bytes());
        buf
    }
}

impl IntoAsm for f64 {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x00];
        out.extend(self.to_be_bytes());
        out
    }
}

impl<'s> IntoAsm for &'s str {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x01];
        out.extend(self.as_bytes());
        out.push(0);
        out
    }
}

impl IntoAsm for String {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x01];
        out.extend(self.into_bytes());
        out.push(0);
        out
    }
}

impl Runtime {
    fn get_native_argc(&mut self, name: &String) -> Option<usize> {
        self.current_module().native_argc(name)
    }

    pub fn run(&mut self) -> Option<()> {
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
                    self.stack.push(Value::Number(out));
                }
            };
        }

        macro_rules! legacy {
            () => {
                panic!("Legacy Opcode encountered!")
            };
        }

        loop {
            let next = self.next_opcode();
            match next {
                Opcode::NOP => { /* noop */ }
                Opcode::HLT => break,
                Opcode::PUSH => {
                    let value = Value::read_from_bc(self)?;
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
                    Value::Boolean(bool) => {
                        debug!(format!("NEG {} {}", bool, !bool));
                        self.stack.push(Value::Boolean(!bool))
                    }
                    Value::Number(v) => {
                        debug!(format!("NEG {} {}", v, -v));
                        self.stack.push(Value::Number(-v));
                    }
                    _ => return None,
                },
                Opcode::INC => {
                    let v = self.stack.pop().number()?;
                    debug!(format!("INC {} {}", v, v + 1.));
                    self.stack.push(Value::Number(v + 1.))
                }
                Opcode::DEC => {
                    let v = self.stack.pop().number()?;
                    debug!(format!("DEC {} {}", v, v - 1.));
                    self.stack.push(Value::Number(v - 1.))
                }
                Opcode::CONCAT => {
                    let second = &*self.stack.pop().string()?;
                    let mut first = (&*self.stack.pop().string()?).clone();
                    first += &second;
                    let ptr = self.alloc(first);
                    debug!(format!("CONCAT => {ptr:?}"));
                    self.stack.push(Value::String(ptr))
                }
                Opcode::EQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("EQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::Boolean(a), Value::Boolean(b)) => a == b,
                        (Value::Number(a), Value::Number(b)) => a == b,
                        (any, Value::Nil) | (Value::Nil, any) => matches!(any, Value::Nil),
                        (Value::String(a), Value::String(b)) => {
                            a.get_ptr() == b.get_ptr() || *a == *b
                        }
                        (Value::Array(a), Value::Array(b)) => a.get_ptr() == b.get_ptr(),
                        (Value::Compound(a), Value::Compound(b)) => a.get_ptr() == b.get_ptr(),
                        (Value::Range(begin_a, end_a), Value::Range(begin_b, end_b)) => {
                            begin_a == begin_b && end_a == end_b
                        }
                        (Value::Function(a), Value::Function(b)) => a.get_ptr() == b.get_ptr(),
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::NEQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("NEQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::Boolean(a), Value::Boolean(b)) => a != b,
                        (Value::Number(a), Value::Number(b)) => a != b,
                        (any, Value::Nil) | (Value::Nil, any) => !matches!(any, Value::Nil),
                        (Value::String(a), Value::String(b)) => {
                            a.get_ptr() != b.get_ptr() || *a != *b
                        }
                        (Value::Array(a), Value::Array(b)) => a.get_ptr() != b.get_ptr(),
                        (Value::Compound(a), Value::Compound(b)) => a.get_ptr() != b.get_ptr(),
                        (Value::Range(begin_a, end_a), Value::Range(begin_b, end_b)) => {
                            begin_a != begin_b || end_a != end_b
                        }
                        (Value::Function(a), Value::Function(b)) => a.get_ptr() != b.get_ptr(),
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::LT => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("LT {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::String(a), Value::String(b)) => a.len() < b.len(),
                        (Value::Number(a), Value::Number(b)) => a < b,
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::GT => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("GT {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::String(a), Value::String(b)) => a.len() > b.len(),
                        (Value::Number(a), Value::Number(b)) => a > b,
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::LTQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("LTQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::String(a), Value::String(b)) => a.len() <= b.len(),
                        (Value::Number(a), Value::Number(b)) => a <= b,
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::GTQ => {
                    let rh = self.stack.pop();
                    let lh = self.stack.pop();
                    debug!(format!("GTQ {} {}", lh, rh));
                    let tag = match (lh, rh) {
                        (Value::String(a), Value::String(b)) => a.len() >= b.len(),
                        (Value::Number(a), Value::Number(b)) => a >= b,
                        _ => false,
                    };
                    self.stack.push(Value::Boolean(tag))
                }
                Opcode::DEBUG => println!("{}", self.stack.pop()),
                Opcode::DEF_GLOBAL_CONST => {
                    let value = self.stack.pop();
                    let name = self.stack.pop().string()?;
                    debug!(format!("DEF_GLOBAL_CONST {} {}", &*name, value));
                    self.define_global(
                        &*name,
                        Variable {
                            name: name.clone(),
                            mutable: false,
                            value,
                        },
                    )
                }
                Opcode::DEF_GLOBAL_MUT => {
                    let value = self.stack.pop();
                    let name = self.stack.pop().string()?;
                    debug!(format!("DEF_GLOBAL_MUT {} {}", &*name, value));
                    self.define_global(
                        &*name,
                        Variable {
                            name: name.clone(),
                            mutable: true,
                            value,
                        },
                    )
                }
                Opcode::SET_GLOBAL => {
                    let value = self.stack.pop();
                    let name = self.stack.pop().string()?;
                    debug!(format!("SET_GLOBAL {} {}", *name, value));
                    self.set_global(&*name, value)
                }
                Opcode::LOAD_GLOBAL => {
                    let name = &*self.stack.pop().string()?;
                    let value = self.get_global(name);
                    debug!(format!("LOAD_GLOBAL {} => {}", name, value));
                    self.stack.push(value);
                }
                Opcode::LOAD_NATIVE => {
                    let name = &*self.stack.pop().string()?;
                    let value = self.get_native_const(name);
                    debug!(format!("LOAD_NATIVE {} => {}", name, value));
                    self.stack.push(value);
                }
                Opcode::DEF_LOCAL_CONST => {
                    let value = self.stack.pop();
                    let name = &*self.stack.pop().string()?;
                    debug!(format!("DEF_LOCAL_CONST {} {}", name, value));
                    self.define_local(name, false, value);
                }
                Opcode::DEF_LOCAL_MUT => {
                    let value = self.stack.pop();
                    let name = &*self.stack.pop().string()?;
                    debug!(format!("DEF_LOCAL_MUT {} {}", name, value));
                    self.define_local(name, true, value);
                }
                Opcode::SET_LOCAL => {
                    let value = self.stack.pop();
                    let name = &*self.stack.pop().string()?;
                    self.set_local();
                    debug!(format!("SET_LOCAL {} {}", name, value));
                }
                Opcode::LOAD_LOCAL => {
                    let value = self.get_local();
                    debug!(format!("LOAD_LOCAL => {}", value));
                    self.stack.push(value);
                }
                Opcode::LEGACY_RESERVED_1 => {
                    debug!("PUSH_SCOPE <legacy>");
                    legacy!()
                }
                Opcode::LEGACY_RESERVED_2 => {
                    debug!("POP_SCOPE <legacy>");
                    legacy!()
                }
                Opcode::ASSERT => {
                    let flag = self.stack.pop().bool()?;
                    assert!(flag, "{}", "Assertion Failed")
                }
                Opcode::JMP_IF => {
                    let bool_flag = self.stack.pop().bool()?;
                    let value = u16::from_be_bytes([self.next_byte(), self.next_byte()]);
                    debug!(format!("JMP_IF {bool_flag} {value}"));
                    self.set_ip(value as usize);
                }
                Opcode::LEGACY_RESERVED_3 => {
                    debug!("JMPF <legacy>");
                    legacy!()
                }
                Opcode::LEGACY_RESERVED_4 => {
                    debug!("JMPB <legacy>");
                    legacy!()
                }
                Opcode::CALL_NATIVE => {
                    let name = &*self.stack.pop().string()?;
                    let argc = self.get_native_argc(name)?;
                    let mut args = repeat_with(|| self.stack.pop())
                        .take(argc)
                        .collect::<Vec<Value>>();
                    args.reverse();
                    debug!(format!("CALL_NATIVE {} {:?}", name, args));
                    let res = self.call_native_function(name, args)?;
                    self.stack.push(res);
                }
                Opcode::IMPORT => {
                    let handle = repeat_with(|| self.next_byte())
                        .take_while(|v| *v != 0x00)
                        .collect::<Vec<u8>>();
                    let path = String::from_utf8(handle[1..].to_vec()).ok()?;
                    debug!(format!("IMPORT {}", path));
                    // TODO: import
                }
                Opcode::FUNCTION => {
                    let arg_size = self.next_byte();
                    let mut params = repeat_with(|| {
                        let heap = &*self.stack.pop().string()?;
                        Some(heap.to_owned())
                    })
                    .take(arg_size as usize)
                    .collect::<Option<Vec<String>>>()?;
                    params.reverse();
                    let name = &*self.stack.pop().string()?;
                    let ptr = u16::from_be_bytes([self.next_byte(), self.next_byte()]) as usize;

                    debug!(format!("FUNCTION {} {:?} <body at {}>", name, params, ptr));

                    let desc = self.alloc(FunctionDescriptor {
                        params,
                        module: self.current_module(),
                        ptr,
                    });
                    self.add_function(name, desc);
                    // self.inc_ip(size);
                }
                Opcode::MODULE => {
                    let module = (&*self.stack.pop().string()?).to_owned();
                    debug!(format!("MODULE {}", module));
                    self.module = module;
                }
                Opcode::CALL => {
                    let arity = self.next_byte();
                    match self.stack.pop() {
                        Value::Function(closure) => {
                            debug!(format!("CALL arity {arity:?}"));
                            self.store_ip();
                            self.bytecode.begin_frame(closure, self.stack.len() + 1);
                            self.load_ip()
                        }
                        _ => {
                            // TODO: throw compile error here
                        }
                    }
                }
                Opcode::JMPA => {
                    let position =
                        u16::from_be_bytes([self.next_byte(), self.next_byte()]) as usize;
                    debug!(format!("JMPA {position}"));
                    self.set_ip(position)
                }
                Opcode::LEGACY_RESERVED_5 => {
                    debug!(format!("SPLIT <legacy>"));
                    legacy!()
                }
                Opcode::CLOSURE => {
                    let arg_size = self.next_byte();
                    let mut params = repeat_with(|| {
                        let heap = &*self.stack.pop().string()?;
                        Some(heap.to_owned())
                    })
                    .take(arg_size as usize)
                    .collect::<Option<Vec<String>>>()?;
                    params.reverse();
                    let size = u16::from_be_bytes([self.next_byte(), self.next_byte()]) as usize;

                    debug!(format!("CLOSURE {:?} <body {}>", params, size));

                    self.inc_ip(size);

                    let desc = self.alloc(FunctionDescriptor {
                        params,
                        module: self.current_module(),
                        ptr: self.ip as usize,
                    });
                    self.stack.push(Value::Function(desc))
                }
                Opcode::RANGE => {
                    let end = self.stack.pop().number()?;
                    let begin = self.stack.pop().number()?;
                    debug!(format!("RANGE {begin} {end}"));
                    self.stack.push(Value::Range(begin, end))
                }
                Opcode::ARRAY => {
                    let size = self.stack.pop().number()? as usize;
                    let mut array = repeat_with(|| self.stack.pop())
                        .take(size)
                        .collect::<Vec<Value>>();
                    array.reverse();
                    debug!(format!("LIST {array:?}"));
                    let ptr = self.alloc(array);
                    self.stack.push(Value::Array(ptr))
                }
                Opcode::INDEX => {
                    let value = self.stack.pop();
                    let indexee = self.stack.pop();
                    debug!(format!("INDEX {value} {indexee}"));
                    match (value, indexee) {
                        (Value::Number(index), Value::String(str)) => {
                            let char = str.chars().nth(index as usize)?.to_string();
                            let ptr = self.alloc(char);
                            self.stack.push(Value::String(ptr));
                        }
                        (Value::Number(index), Value::Array(arr)) => {
                            self.stack
                                .push((*arr).get(index as usize).unwrap().to_owned());
                        }
                        (Value::Number(index), Value::Compound(cmp)) => {
                            let index = index as usize;
                            self.stack.push(
                                cmp.iter()
                                    .nth(index)
                                    .map(|it| (&*it.1).to_owned())
                                    .unwrap_or(Value::Nil),
                            )
                        }
                        (Value::Range(begin, end), Value::String(str)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let char = str[begin..end].to_string();
                            let ptr = self.alloc(char);
                            self.stack.push(Value::String(ptr));
                        }
                        (Value::Range(begin, end), Value::Array(arr)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let slice = arr[begin..end].to_vec();
                            let ptr = self.alloc(slice);
                            self.stack.push(Value::Array(ptr));
                        }
                        (Value::Range(begin, end), Value::Compound(cmp)) => {
                            let (begin, end) = (begin as usize, end as usize);
                            let slice = cmp
                                .iter()
                                .map(|each| ((&*each.0).to_owned(), (&*each.1).to_owned()))
                                .collect::<Vec<(String, Value)>>()[begin..end]
                                .to_vec();
                            let ptr = self.alloc(AHashMap::from_iter(slice));
                            self.stack.push(Value::Compound(ptr))
                        }
                        (Value::String(key), Value::Compound(cmp)) => {
                            let value = (*cmp).get(&*key).unwrap_or(&Value::Nil);
                            self.stack.push(value.to_owned());
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
                        let key = (&*self.stack.pop().string()?).to_owned();
                        Some((key, value))
                    })
                    .take(size)
                    .collect::<Option<Vec<(String, Value)>>>()?;
                    elements.reverse();
                    let map = AHashMap::from_iter(elements);
                    let ptr = self.alloc(map);
                    self.stack.push(Value::Compound(ptr))
                }
                Opcode::RETURN => {
                    let result = self.stack.pop();

                    let base_counter = self.bytecode.last_frame().base_counter;
                    self.stack.truncate(base_counter);

                    if let None = self.bytecode.end_frame() {
                        panic!("Frame Underflow");
                    }
                    self.load_ip();
                    self.stack.push(result);
                }
            }
        }
        Some(())
    }
}
