use std::sync::{Arc, Mutex};

use conduct_tk::AHashMap;

use crate::op::Opcode;

#[derive(Debug, Clone)]
pub enum StackValue {
    Variable {
        name: Arc<str>,
        mutable: bool,
        value: Arc<Mutex<StackValue>>,
    },
    String(Arc<str>),
    StringPtr(usize),
    Bool(bool),
    Float(f64),
    Object(AHashMap<String, StackValue>),
    Array(Vec<StackValue>),
    // TODO: JIT caching
    Lambda(usize),
    NativeFnPtr(String),
    NativeConstPtr(String),
    Nil,
}

impl StackValue {
    pub fn as_value(self) -> Self {
        match self {
            Self::Variable {
                name: _,
                mutable: _,
                value,
            } => value.lock().unwrap().clone(),
            other => other,
        }
    }

    pub fn as_bool(self) -> bool {
        match self {
            Self::Bool(b) => b,
            Self::String(s) => !s.is_empty(),
            Self::Object(obj) => !obj.is_empty(),
            Self::Array(arr) => !arr.is_empty(),
            Self::Float(f) => f > 0.,
            Self::Lambda(_) => true,
            Self::Nil => false,
            _ => true,
        }
    }

    pub fn as_str(self) -> Arc<str> {
        match self {
            Self::Bool(b) => Arc::from(format!("{b}").as_str()),
            Self::String(s) => Arc::from(s.as_ref()),
            Self::Object(_) => Arc::from("{object}"),
            Self::Array(_) => Arc::from("[array]"),
            Self::Float(f) => Arc::from(f.to_string().as_str()),
            Self::Lambda(_) => Arc::from("<lambda>"),
            Self::Nil => Arc::from("nil"),
            _ => Arc::from("undefined"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub parameters: Vec<String>,
    pub body: Vec<Opcode>,
}

#[derive(Debug, Clone)]
pub struct NativeFunctionData {
    pub parameters: Vec<String>,
    pub executor: fn(AHashMap<String, StackValue>) -> StackValue,
}
