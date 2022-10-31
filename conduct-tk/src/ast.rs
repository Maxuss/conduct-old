use ahash::AHashMap;

#[derive(Debug, Clone)]
pub struct ValueBody {
    pub value: Literal,
    pub operator: Option<UnaryOperator>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Nil,
    String(String),
    Number(f64),
    Boolean(bool),
    Reference(String),
    Array(Vec<Expression>),
    Compound(AHashMap<String, Expression>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(ValueBody),
    Path(Path),
    BinaryOperation(BinaryOperation),
    Function(Vec<Statement>),
}

#[derive(Debug, Clone)]
pub struct BinaryOperation {
    pub values: Vec<Expression>,
    pub operators: Vec<BinaryOperator>,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Range,
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

#[derive(Debug, Copy, Clone)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, Clone)]
pub struct Path {
    pub base: Box<Expression>,
    pub elements: Vec<PathElement>,
}

#[derive(Debug, Clone)]
pub enum PathElement {
    // .prop
    AccessProperty(String),
    // [indexed]
    Index(Expression),
    // (args...)
    Invoke(Vec<Expression>),
}

#[derive(Debug, Clone)]
pub enum Statement {}
