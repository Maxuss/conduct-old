use std::{fmt::Display, iter::repeat_with};

use logos::{Lexer, Logos};

use crate::err::{CodeArea, CodeSource, ParsingError, Res};

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

    #[token("!!")]
    DoubleBang,
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

    #[token("module")]
    Module,

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

    #[regex(r#""(?:\\.|[^\\"])*"|'(?:\\.|[^\\'])*'"#, parse_string)]
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
            DoubleBang => "a double exclamation mark",
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
            Module | Is | In | Let | Const | If | Else | Throw | Function | Import | Type
            | Native | Return => "a keyword",
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

fn parse_string(lex: &mut Lexer<Token>) -> Res<String> {
    let slice = lex.slice();
    let slice_clone = slice.clone();
    let len = slice.len();
    let slice = slice[1..slice.len() - 1].to_owned();
    let mut chars = slice.chars();
    let mut out_buf = String::with_capacity(slice.len());
    while let Some(char) = chars.next() {
        out_buf.push(if char == '\\' {
            match chars.next() {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('\'') => '\'',
                Some('"') => '"',
                Some('x') => {
                    let hex = [chars.next(), chars.next()]
                        .into_iter()
                        .collect::<Option<String>>();
                    let hex = if let Some(hex) = hex {
                        hex.to_owned()
                    } else {
                        return Err(ParsingError::UnexpectedEOF {
                            at: CodeArea {
                                src: CodeSource::Inline(slice),
                                line: 1,
                                span: (0, len),
                            },
                        });
                    };
                    let byte_hex =
                        u32::from_str_radix(&hex, 16).map_err(|e| ParsingError::SyntaxError {
                            message: format!("{e}"),
                            at: CodeArea {
                                src: CodeSource::Inline(slice_clone.to_owned()),
                                line: 1,
                                span: (0, len),
                            },
                        })?;
                    char::from_u32(byte_hex).ok_or(ParsingError::SyntaxError {
                        message: "Failed to parse character".to_owned(),
                        at: CodeArea {
                            src: CodeSource::Inline(slice_clone.to_owned()),
                            line: 1,
                            span: (0, len),
                        },
                    })?
                }
                Some('u') => {
                    let hex = repeat_with(|| chars.next())
                        .take(4)
                        .collect::<Option<String>>();
                    let hex = if let Some(hex) = hex {
                        hex.to_owned()
                    } else {
                        return Err(ParsingError::UnexpectedEOF {
                            at: CodeArea {
                                src: CodeSource::Inline(slice_clone.to_owned()),
                                line: 1,
                                span: (0, len),
                            },
                        });
                    };
                    let byte_hex =
                        u32::from_str_radix(&hex, 16).map_err(|e| ParsingError::SyntaxError {
                            message: format!("{e}"),
                            at: CodeArea {
                                src: CodeSource::Inline(slice_clone.to_owned()),
                                line: 1,
                                span: (0, len),
                            },
                        })?;
                    char::from_u32(byte_hex).ok_or(ParsingError::SyntaxError {
                        message: "Failed to parse character".to_owned(),
                        at: CodeArea {
                            src: CodeSource::Inline(slice_clone.to_owned()),
                            line: 1,
                            span: (0, slice.len()),
                        },
                    })?
                }
                other => {
                    return Err(ParsingError::SyntaxError {
                        message: format!("Invalid escape sequence: {other:?}"),
                        at: CodeArea {
                            src: CodeSource::Inline(slice_clone.to_owned()),
                            line: 1,
                            span: (0, len),
                        },
                    })
                }
            }
        } else {
            char
        });
    }
    Ok(out_buf)
}
