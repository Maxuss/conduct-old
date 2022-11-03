use std::fmt::Display;

use ahash::AHashMap;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValueBody {
    pub value: Literal,
    pub operator: Option<UnaryOperator>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Literal {
    Nil,
    String(String),
    Number(f64),
    Boolean(bool),
    Reference(String),
    Array(Vec<Expression>),
    Compound(AHashMap<String, Expression>),
    TypeDefinition(AHashMap<String, String>),
}

impl Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Literal::Nil => "nil literal",
            Literal::String(_) => "string literal",
            Literal::Number(_) => "number literal",
            Literal::Boolean(_) => "boolean literal",
            Literal::Reference(_) => "identifier literal",
            Literal::Array(_) => "array literal",
            Literal::Compound(_) => "compound literal",
            Literal::TypeDefinition(_) => "type",
        };
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Expression {
    Literal(ValueBody),
    Path(Path),
    BinaryOperation(BinaryOperation),
    Function(Vec<String>, Vec<Statement>),
    Ternary(Ternary),
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Literal(_) => "literal",
            Self::Path(_) => "path literal",
            Self::BinaryOperation(_) => "binary operation",
            Self::Function(_, _) => "arrow function",
            Self::Ternary(_) => "ternary operation",
        };
        write!(f, "{str}")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Ternary {
    pub condition: Box<Expression>,
    pub if_clause: Box<Expression>,
    pub else_clause: Box<Expression>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryOperation {
    pub values: Vec<Expression>,
    pub operators: Vec<BinaryOperator>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Range,
    In,
    Multiply,
    Divide,
    Modulo,
    Power,
    Or,
    And,
    Equals,
    LessThan,
    Is,
    GreaterThan,
    LessThanOrEqual,
    GreaterThanOrEqual,
    NotEquals,
    BitwiseOr,
    BitwiseAnd,
    BitwiseXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug, Copy, Clone, Serialize, Deserialize)]
pub enum UnaryOperator {
    Bang,
    Minus,
    Increment,
    Decrement,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Path {
    pub base: Box<Expression>,
    pub elements: Vec<PathElement>,
}

impl Display for Path {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut buf = "<>".to_owned();
        for element in &self.elements {
            match element {
                PathElement::AccessProperty(prop) => buf += &format!(".{prop}"),
                PathElement::Index(_) => buf += "[<index>]",
                PathElement::Invoke(_) => buf += "(<invoke>)",
                PathElement::NullAssert => buf += "!!",
            }
        }
        write!(f, "{}", buf)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum PathElement {
    // .prop
    AccessProperty(String),
    // [indexed]
    Index(Expression),
    // (args...)
    Invoke(Vec<Expression>),
    // !!
    NullAssert,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Assignment {
    Assign,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
    ModuloAssign,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum Statement {
    // import core.prelude;
    Import(String),
    // module std
    Module(String),
    // fun pow(a, b) { import core.math; return pow(a, b) }
    Function(String, Vec<String>, Vec<Statement>),
    // let a = value
    Variable(String, Expression),
    // const b = value
    Constant(String, Expression),
    // native const c = value
    NativeConstant(String),
    // native fun pow(a, b)
    NativeFunction(String, Vec<String>),
    // a.b += c.d
    AssignValue(Expression, Assignment, Expression),
    // return a.b[c]
    Return(Expression),
    // if <cond> { } else if <other> { } else { }
    If(IfStatement),
    // while <cond> { <code> }
    WhileLoop(Expression, Vec<Statement>),
    // for <iterable> in <iterator> { <code> }
    ForLoop(ForLoopStatement),
    // break
    Break,
    // continue
    Continue,
    // simple expression (does nothing, or may be a function call)
    Expression(Expression),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForLoopStatement {
    pub iterable: String,
    pub iterator: Expression,
    pub code: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
    pub else_ifs: Vec<ElseIfStatement>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ElseIfStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}
