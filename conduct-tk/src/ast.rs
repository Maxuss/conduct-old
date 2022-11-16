use std::fmt::Display;

use ahash::AHashMap;
use serde::{Deserialize, Serialize};

pub type Span = (usize, usize);
pub type Spanned<T> = (T, Span);

pub trait Visitor {
    fn visit_stmt(&mut self, tree: &Statement);

    fn consume(&mut self, stream: &Vec<Statement>) {
        for stmt in stream {
            self.visit_stmt(stmt);
        }
    }
}

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
    Array(Vec<Spanned<Expression>>),
    Compound(AHashMap<String, Spanned<Expression>>),
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
    Function(Vec<Spanned<String>>, Vec<Statement>),
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
    pub condition: Box<Spanned<Expression>>,
    pub if_clause: Box<Spanned<Expression>>,
    pub else_clause: Box<Spanned<Expression>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BinaryOperation {
    pub values: Vec<Spanned<Expression>>,
    pub operators: Vec<Spanned<BinaryOperator>>,
}

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
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
    Index(Spanned<Expression>),
    // (args...)
    Invoke(Vec<Spanned<Expression>>),
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
    Import(Spanned<String>),
    // export std.proccess
    Export(Spanned<String>),
    // module std
    Module(Spanned<String>),
    // fun pow(a, b) { import core.math; return pow(a, b) }
    Function(Spanned<String>, Vec<Spanned<String>>, Vec<Statement>),
    // let a = value
    Variable(Spanned<String>, Spanned<Expression>),
    // const b = value
    Constant(Spanned<String>, Spanned<Expression>),
    // native const c = value
    NativeConstant(Spanned<String>),
    // native fun pow(a, b)
    NativeFunction(Spanned<String>, Vec<Spanned<String>>),
    // a.b += c.d
    AssignValue(
        Spanned<Expression>,
        Spanned<Assignment>,
        Spanned<Expression>,
    ),
    // return a.b[c]
    Return(Spanned<Expression>),
    // if <cond> { } else if <other> { } else { }
    If(IfStatement),
    // while <cond> { <code> }
    WhileLoop(Spanned<Expression>, Vec<Statement>),
    // for <iterable> in <iterator> { <code> }
    ForLoop(ForLoopStatement),
    // throw <err>
    Throw(Spanned<Expression>),
    // try { <code> } catch error as err { <code> } catch OtherError as err { <code> } catch * as other { <code> }
    TryCatch(TryCatchStatement),
    // break
    Break(Span),
    // continue
    Continue(Span),
    // simple expression (does nothing, or may be a function call)
    Expression(Spanned<Expression>),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TryCatchStatement {
    pub try_clause: Vec<Statement>,
    pub catch_clauses: Vec<CatchClause>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CatchClause {
    pub catches: Spanned<TypeReference>,
    pub name: String,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ForLoopStatement {
    pub iterable: String,
    pub iterator: Spanned<Expression>,
    pub code: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IfStatement {
    pub condition: Spanned<Expression>,
    pub body: Vec<Statement>,
    pub else_ifs: Vec<ElseIfStatement>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ElseIfStatement {
    pub condition: Spanned<Expression>,
    pub body: Vec<Statement>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TypeReference {
    pub name: String,
    pub nullable: bool,
}
