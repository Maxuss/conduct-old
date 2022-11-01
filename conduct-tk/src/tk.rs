use std::fmt::Display;

use logos::Logos;

#[derive(Debug, Clone, Logos, PartialEq, PartialOrd)]
pub enum Token {
    // basic tokens
    #[token("=>")]
    ThickArrow,

    #[token("|")]
    BitwiseOr,

    #[token("||")]
    Or,

    #[token("&")]
    BitwiseAnd,

    #[token("&&")]
    And,

    #[token("==")]
    Equal,

    #[token("is")]
    Is,

    #[token("in")]
    In,

    #[token("!=")]
    NotEqual,

    #[token(">=")]
    MoreOrEqual,

    #[token("<=")]
    LessOrEqual,

    #[token(">")]
    MoreThan,

    #[token("<")]
    LessThan,

    #[token("*")]
    Star,

    #[token("%")]
    Modulo,

    #[token("^")]
    BitwiseXor,

    #[token("**")]
    DoubleStar,

    #[token("+")]
    Plus,

    #[token("-")]
    Minus,

    #[token("/")]
    Slash,

    #[token("<<")]
    ShiftLeft,

    #[token(">>")]
    ShiftRight,

    #[token("..")]
    DoublePeriod,

    #[token(".")]
    Period,

    #[token("!")]
    Exclamation,

    #[token("=")]
    Assign,

    #[token("+=")]
    AddAssign,
    #[token("-=")]
    SubtractAssign,
    #[token("*=")]
    MultiplyAssign,
    #[token("/=")]
    DivideAssign,
    #[token("%=")]
    ModuloAssign,

    #[token("++")]
    Increment,
    #[token("--")]
    Decrement,

    #[token(",")]
    Comma,

    #[token("{")]
    OpenCurlyBracket,

    #[token("}")]
    ClosingCurlyBracket,

    #[token("[")]
    OpenSquareBracket,

    #[token("]")]
    ClosingSquareBracket,

    #[token("(")]
    OpenBracket,

    #[token(")")]
    ClosingBracket,

    #[token(":")]
    Colon,

    #[token("#")]
    Hash,

    #[token("?")]
    QuestionMark,

    // important tokens
    #[token("let")]
    Let,

    #[token("const")]
    Const,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("throw")]
    Throw,

    #[token("fn")]
    Function,

    #[token("import")]
    Import,

    #[token("type")]
    Type,

    #[token("native")]
    Native,

    #[token("return")]
    Return,

    // constants
    #[token("nil")]
    Nil,

    #[token("true")]
    True,

    #[token("false")]
    False,

    // identifiers
    #[regex(r#"[a-zA-Z_][a-zA-Z0-9_$]*"#, |lex| lex.slice().to_owned())]
    Identifier(String),

    // literals
    #[regex(r#"0b[01][01_]*"#, |lex| u32::from_str_radix(&lex.slice()[2..], 2))]
    BinaryLiteral(u32),

    #[regex(r#"0o[0-7][0-7_]*"#, |lex| u32::from_str_radix(&lex.slice()[2..], 8))]
    OctalLiteral(u32),

    #[regex(r#"0x[\dA-Fa-f][\dA-Fa-f_]*"#, |lex| u32::from_str_radix(&lex.slice()[2..], 16))]
    HexLiteral(u32),

    #[regex(r#"[0-9][_0-9]*(\.[0-9][_0-9]*)?"#, |lex| lex.slice().parse::<f64>())]
    DecimalLiteral(f64),

    #[regex(r#""(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*'"#, |lex| {
        let slice = lex.slice();
        slice[1..slice.len()-1].to_owned()
    })]
    StringLiteral(String),

    // special
    #[regex(r#"(//.*[\r\n]+)|(/\*[^*]*\*(([^/\*][^\*]*)?\*)*/)"#, |lex| {
        let slice = lex.slice();
        if let Some(stripped) = slice.strip_prefix("//") {
            stripped.trim().to_owned()
        } else {
            slice[2..slice.len()-2].trim_start().to_owned()
        }
    })]
    Comment(String),

    #[regex(r#"[;\r\n]+"#)]
    StatementSeparator,

    #[regex(r#"[ \t]+"#, logos::skip)]
    #[error]
    Error,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        let str = match self {
            ThickArrow => "a thick arrow",
            BitwiseAnd | BitwiseOr | Or | And | Equal | NotEqual | MoreOrEqual | LessOrEqual
            | MoreThan | LessThan | Star | Modulo | BitwiseXor | DoubleStar | Plus | Minus
            | Slash | ShiftLeft | ShiftRight | DoublePeriod => "a binary operator",
            Period => "a period",
            Exclamation => "an exclamation mark",
            Assign | AddAssign | SubtractAssign | MultiplyAssign | DivideAssign | ModuloAssign => {
                "an assignment"
            }
            Increment => "an incrementation",
            Decrement => "a decrementation",
            Comma => "a comma",
            OpenCurlyBracket => "an opening delimeter '{'",
            OpenSquareBracket => "an opening delimeter '['",
            OpenBracket => "an opening delimeter '('",
            ClosingCurlyBracket => "a closing delimeter '}'",
            ClosingSquareBracket => "a closing delimeter ']'",
            ClosingBracket => "a closing delimeter ')'",
            Colon => "a colon",
            Hash => "a hash",
            QuestionMark => "a question mark",
            Is | In | Let | Const | If | Else | Throw | Function | Import | Type | Native
            | Return => "a keyword",
            Nil => "a nil literal",
            True | False => "a boolean constant",
            Identifier(_) => "an identifier",
            BinaryLiteral(_) | OctalLiteral(_) | HexLiteral(_) | DecimalLiteral(_) => {
                "a number literal"
            }
            StringLiteral(_) => "a string literal",
            Comment(_) => "a comment literal",
            StatementSeparator => "a statement separator",
            Error => "an unknown token",
        };
        write!(f, "{}", str)
    }
}
