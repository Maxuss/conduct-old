use std::{fmt::Display, io::Read, iter::repeat_with};

use conduct_tk::AHashMap;

use crate::{
    fnc::FunctionDescriptor,
    heap::{Gc, Trace},
    rt::Runtime,
    vm::Variable,
};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Nil,
    Number(f64),
    Boolean(bool),
    Range(f64, f64),
    Function(Gc<FunctionDescriptor>),
    String(Gc<String>),
    Array(Gc<Vec<Self>>),
    Compound(Gc<AHashMap<String, Value>>),
    Variable(Gc<Variable>),
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Value::Nil => "nil".to_string(),
                Value::Number(num) => format!("{num}"),
                Value::Boolean(bool) => format!("{bool}"),
                Value::Range(start, end) => format!("{start}..{end}"),
                Value::Function(fun) => format!("<fn 0x{:2x?}>", fun.get().map(|it| it.value.ptr)),
                Value::String(str) => format!("{:?}", str.get_unchecked().value),
                Value::Array(arr) => format!("{:?}", arr.get_unchecked().value),
                Value::Compound(cmp) => format!("{:?}", cmp.get_unchecked().value),
                Value::Variable(var) => format!(
                    "<var {} {} = {}>",
                    if var.mutable { "mut" } else { "const" },
                    *var.name,
                    var.value
                ),
            }
        )
    }
}

impl Value {
    pub fn number(&self) -> Option<f64> {
        match self {
            Self::Number(v) => Some(*v),
            _ => None,
        }
    }

    pub fn bool(&self) -> Option<bool> {
        match self {
            Self::Boolean(v) => Some(*v),
            _ => None,
        }
    }

    pub fn range(&self) -> Option<(f64, f64)> {
        match self {
            Self::Range(a, b) => Some((*a, *b)),
            _ => None,
        }
    }

    pub fn function(&self) -> Option<Gc<FunctionDescriptor>> {
        match self {
            Self::Function(v) => Some(*v),
            _ => None,
        }
    }

    pub fn string(&self) -> Option<Gc<String>> {
        match self {
            Self::String(v) => Some(*v),
            _ => None,
        }
    }

    pub fn array(&self) -> Option<Gc<Vec<Self>>> {
        match self {
            Self::Array(v) => Some(*v),
            _ => None,
        }
    }

    pub fn compound(&self) -> Option<Gc<AHashMap<String, Self>>> {
        match self {
            Self::Compound(v) => Some(*v),
            _ => None,
        }
    }

    pub fn read_from_bc(rt: &mut Runtime) -> Option<Value> {
        let type_index = rt.next_byte();
        Some(match type_index {
            0x00 => {
                // a number, parse a float
                let mut buffer = [0; 8];
                let handle = repeat_with(|| rt.next_byte()).take(8).collect::<Vec<u8>>();
                let mut handle = handle.take(8);
                if 8 != handle.read(&mut buffer).unwrap() {
                    panic!("Invalid Bytecode")
                }
                Value::Number(f64::from_be_bytes(buffer))
            }
            0x01 => {
                // a string, parse it
                let handle = repeat_with(|| rt.next_byte())
                    .take_while(|v| *v != 0x00)
                    .collect::<Vec<u8>>();
                let ptr = rt.alloc(String::from_utf8(handle).ok()?);
                Value::String(ptr)
            }
            0x02 => {
                // legacy reserved value
                Value::Nil
            }
            0x03 => {
                // nil value
                Value::Nil
            }
            0x04 => {
                // a slice
                let mut begin_buffer = [0; 8];
                let handle = repeat_with(|| rt.next_byte()).take(8).collect::<Vec<u8>>();
                let mut handle = handle.take(8);
                if 8 != handle.read(&mut begin_buffer).unwrap() {
                    panic!("Invalid Bytecode")
                }
                let begin = u64::from_be_bytes(begin_buffer) as f64;

                let mut end_buffer = [0; 8];
                let handle = repeat_with(|| rt.next_byte()).take(8).collect::<Vec<u8>>();
                let mut handle = handle.take(8);
                if 8 != handle.read(&mut end_buffer).unwrap() {
                    panic!("Invalid Bytecode")
                }
                let end = u64::from_be_bytes(end_buffer) as f64;

                Value::Range(begin, end)
            }
            _ => return None,
        })
    }
}

impl Trace for Value {
    fn trace(&self) {
        match self {
            Value::Nil => {}
            Value::Number(_) => {}
            Value::Boolean(_) => {}
            Value::Range(_, _) => {}
            Value::Function(f) => f.trace(),
            Value::String(s) => s.trace(),
            Value::Array(arr) => arr.trace(),
            Value::Compound(cmp) => cmp.trace(),
            Value::Variable(var) => var.trace(),
        }
    }
}
