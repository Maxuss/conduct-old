use std::sync::{Arc, Mutex};

use conduct_tk::AHashMap;

use crate::op::Opcode;

#[derive(Debug, Clone, PartialEq)]
pub enum StackValue {
    Variable {
        mutable: bool,
        value: Arc<StackValue>,
    },
    String(Arc<str>),
    Float(f64),
    Object(AHashMap<String, StackValue>),
    Array(Vec<StackValue>),
    // TODO: JIT caching
    Lambda(usize),
    LazyAccess(Arc<StackValue>, Arc<str>),
    LazyInvoke(Arc<StackValue>, Vec<StackValue>),
    LazyNullAssert(Arc<StackValue>),
    Nil,
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    pub parameters: Vec<String>,
    pub body: Vec<Opcode>,
    pub preinit_stack: Vec<StackValue>,
}
