use std::sync::Arc;

use conduct_tk::{ast::Expression, AHashMap};

use crate::mem::{FunctionData, StackValue};

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    FloatConst(f64),
    StringConst(Arc<str>),
    BoolConst(bool),
    Lambda(usize),
    Add,
    Sub,
    Div,
    Mul,
    Mod,
    Invoke(usize),
    Nil,
    DefVar(Arc<str>),
    DefConst(Arc<str>),
    LoadVar(Arc<str>),
    Debug,
    Halt,
    Return,
    InitObj,
    InitArray,
    ObjPush(Arc<str>),
    ArrayPush,
    Negate,
    Pow,
    Dec,
    Inc,
    GetProp(Arc<str>),
    Index,
    NullAssert,
    Function(Arc<str>, Vec<String>),
    Jump(usize),
    JumpIf(usize),
    JumpIfNot(usize),
    Pop,
    Assign,
    AddAssign,
    SubAssign,
    DivAssign,
    MulAssign,
    ModAssign,
}
