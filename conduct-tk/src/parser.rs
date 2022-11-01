use ahash::AHashMap;
use ariadne::ReportKind;
use logos::{Lexer, Logos};

use crate::{
    ast::{
        Assignment, BinaryOperation, BinaryOperator, ElseIfStatement, Expression, IfStatement,
        Literal, Path, PathElement, Statement, UnaryOperator, ValueBody,
    },
    bail, check,
    err::{error, CodeArea, CodeSource, ConductCache, ParsingError, Res},
    tk::Token,
};

macro_rules! op_precedence {
    {
        $p_f:expr, $a_f:ident => $($op_f:ident)+,
        $(
            $p:expr, $a:ident => $($op:ident)+,
        )*
    } => {
        fn operator_precedence(op: &BinaryOperator) -> u8 {
            match op {
                $(
                    $(
                        BinaryOperator::$op => $p,
                    )+
                )*
                $(
                    BinaryOperator::$op_f => $p_f,
                )+
            }
        }
        pub enum OpAssociativity {
            Left,
            Right,
        }
        fn operator_associativity(op: &BinaryOperator) -> OpAssociativity {
            match op {
                $(
                    $(
                        BinaryOperator::$op => OpAssociativity::$a,
                    )+
                )*
                $(
                    BinaryOperator::$op_f => OpAssociativity::$a_f,
                )+
            }
        }
        const HIGHEST_PRECEDENCE: u8 = $p_f;
    }
}

op_precedence! { // make sure the highest precedence is at the top
    12, Left => BitwiseXor ShiftLeft ShiftRight,
    11, Left => BitwiseOr,
    10, Left => BitwiseAnd,
    9, Right => Power,
    8, Left => Modulo Multiply Divide,
    7, Left => Add Subtract,
    6, Left => Range,
    5, Left => LessThanOrEqual GreaterThanOrEqual,
    4, Left => LessThan GreaterThan,
    3, Left => Is NotEquals Equals,
    2, Left => And,
    1, Left => Or,
}

macro_rules! display {
    ($tk:expr) => {{
        match $tk {
            Some(tok) => format!("{tok}"),
            None => "EOF".to_owned(),
        }
    }};
}

pub struct Parser<'lex> {
    lexer: Lexer<'lex, Token>,
    index: usize,
    stack: Vec<(Option<Token>, String, (usize, usize))>,
    source: CodeSource,
    line: usize,
}

impl<'lex> Parser<'lex> {
    pub fn new(src: CodeSource, lexer: Lexer<'lex, Token>) -> Self {
        Self {
            lexer,
            index: 0,
            stack: vec![],
            source: src,
            line: 1,
        }
    }

    pub fn new_inline(str: &'lex str) -> Self {
        Self {
            lexer: Token::lexer(str),
            index: 0,
            stack: vec![],
            source: CodeSource::Inline(str.to_owned()),
            line: 1,
        }
    }

    fn inner_next(&mut self) -> Option<Token> {
        if self.index == 0 {
            let next_elem = self.lexer.next();

            let slice = self.lexer.slice().to_string();
            let range: core::ops::Range<usize> = self.lexer.span();

            self.stack
                .push((next_elem.clone(), slice, (range.start, range.end)));
            next_elem
        } else {
            self.index -= 1;
            self.stack[self.stack.len() - self.index - 1].0.to_owned()
        }
    }

    pub fn next(&mut self, ignore_unneeded: bool) -> Option<Token> {
        let next = self.inner_next();
        if ignore_unneeded
            && matches!(
                next,
                Some(Token::StatementSeparator) | Some(Token::Comment(_))
            )
        {
            self.next(ignore_unneeded)
        } else {
            next
        }
    }

    fn next_nocomment(&mut self) -> Option<Token> {
        let next = self.inner_next();
        if let Some(Token::Comment(_)) = next {
            self.next_nocomment()
        } else {
            next
        }
    }

    pub fn prev(&mut self) -> Option<Token> {
        self.inner_prev(true)
    }

    fn inner_prev(&mut self, ignore_unneeded: bool) -> Option<Token> {
        self.index += 1;
        let len = self.stack.len();
        if len > self.index {
            if ignore_unneeded
                && matches!(
                    self.stack[len - self.index - 1].0,
                    Some(Token::StatementSeparator) | Some(Token::Comment(_))
                )
            {
                self.inner_prev(ignore_unneeded)
            } else if len - self.index >= 1 {
                self.stack[len - self.index - 1].0.clone()
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn current(&self) -> Option<Token> {
        let len = self.stack.len();
        if len == 0 || len - self.index < 1 {
            None
        } else {
            self.stack[len - self.index - 1].0.clone()
        }
    }

    pub fn slice(&self) -> &str {
        &self.stack[self.stack.len() - self.index - 1].1
    }

    pub fn position(&self) -> (usize, usize) {
        if self.stack.len() - self.index == 0 {
            return (0, 0);
        }
        let range = &self.stack[self.stack.len() - self.index - 1].2;

        *range
    }

    fn area(&self) -> CodeArea {
        CodeArea {
            src: self.source.clone(),
            line: self.line,
            span: self.stack.last().unwrap().2,
        }
    }

    #[inline(always)]
    fn ensure(&mut self, expected_token: Token) -> Res<()> {
        let v = self.inner_next();
        let expected = format!("{expected_token}");
        match v {
            Some(token) => {
                if token == expected_token {
                    Ok(())
                } else {
                    Err(ParsingError::Expected {
                        expected,
                        found: format!("{token}"),
                        at: self.area(),
                    })
                }
            }
            other => Err(ParsingError::Expected {
                expected,
                found: display!(other),
                at: self.area(),
            }),
        }
    }

    pub fn parse_value(&mut self) -> Res<ValueBody> {
        if let Some(mut value) = self.next(true) {
            let operator = if let Some(op) = self.parse_unary_operator() {
                value = if let Some(value) = self.next(true) {
                    value
                } else {
                    return Err(ParsingError::Expected {
                        at: CodeArea {
                            src: self.source.clone(),
                            line: self.line,
                            span: self.stack.last().unwrap().2,
                        },
                        expected: "a literal token".to_owned(),
                        found: "EOF".to_owned(),
                    });
                };
                Some(op)
            } else {
                None
            };
            let value = match value {
                Token::DecimalLiteral(num) => Literal::Number(num),
                Token::BinaryLiteral(num) | Token::OctalLiteral(num) | Token::HexLiteral(num) => {
                    Literal::Number(num as f64)
                }
                Token::StringLiteral(str) => Literal::String(str),
                Token::True => Literal::Boolean(true),
                Token::False => Literal::Boolean(false),
                Token::Nil => Literal::Nil,
                Token::Identifier(id) => Literal::Reference(id),
                Token::OpenSquareBracket => {
                    // parsing an array
                    let mut buf: Vec<Expression> = Vec::new();
                    loop {
                        if self.next(true) == Some(Token::ClosingSquareBracket) {
                            break;
                        }

                        self.prev();
                        buf.push(check!(self.parse_expression()));
                        match self.next(true) {
                            Some(Token::ClosingSquareBracket) => {
                                // array end
                                break;
                            }
                            Some(Token::Comma) => {
                                // all fine
                                continue;
                            }
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "a comma ',' or a closing delimeter ']'".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        }
                    }
                    Literal::Array(buf)
                }
                Token::OpenCurlyBracket => {
                    // parsing a compound
                    let mut compound: AHashMap<String, Expression> = AHashMap::new();
                    loop {
                        // key: value
                        let key = match self.next(true) {
                            Some(Token::ClosingCurlyBracket) => break,
                            Some(Token::StringLiteral(str)) => str,
                            Some(Token::Identifier(id)) => id,
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "a string literal or an identifier".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        };

                        check!(self.ensure(Token::Colon));

                        let value = check!(self.parse_expression());

                        compound.insert(key, value);

                        match self.next(true) {
                            Some(Token::ClosingCurlyBracket) => {
                                // encountered end of compound
                                break;
                            }
                            Some(Token::Comma) => {
                                // all fine here
                                continue;
                            }
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "a comma ',' or a closing delimeter '}'".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        }
                    }
                    Literal::Compound(compound)
                }
                other => {
                    return Err(ParsingError::Expected {
                        expected: "a literal value".to_owned(),
                        found: format!("{other}"),
                        at: CodeArea {
                            src: self.source.clone(),
                            line: self.line,
                            span: self.stack.last().unwrap().2,
                        },
                    })
                }
            };
            Ok(ValueBody { value, operator })
        } else {
            return Err(ParsingError::Expected {
                at: CodeArea {
                    src: self.source.clone(),
                    line: self.line,
                    span: self.stack.last().unwrap().2,
                },
                expected: "a literal token".to_owned(),
                found: "EOF".to_owned(),
            });
        }
    }

    pub fn parse_expression(&mut self) -> Res<Expression> {
        let next_token = self.next(false);
        match next_token {
            Some(Token::OpenBracket) => {
                // lambda function declaration
                // e.g.
                // (arg1, arg2, ...) => { <snip> }
                // TODO: parse function
                bail!(
                    CodeArea {
                        src: self.source.clone(),
                        line: self.line,
                        span: self.stack.last().unwrap().2,
                    },
                    "Unsupported"
                )
            }
            Some(_) => {
                self.prev();
                // anything else that is left are literals
                let literal = check!(self.parse_value());
                // there are multiple cases from which we can go now.
                // A: it's a path sequence ("a".b.c()[d])
                // B: it's a binary operation (a + b) / c
                // C: it's a simple literal (if neither of previous matched)
                let _ = self.next(false);

                // is it a path sequence?
                match self.parse_path_element() {
                    Ok(path) => {
                        let value = Expression::Literal(literal);
                        let mut path = vec![path];
                        let _ = self.next(true);
                        while let Ok(element) = self.parse_path_element() {
                            path.push(element);
                            self.next(true);
                        }
                        self.prev();
                        return Ok(Expression::Path(Path {
                            base: Box::new(value),
                            elements: path,
                        }));
                    }
                    Err(ParsingError::Handled) => {
                        // all fine, we can move forward
                    }
                    Err(other) => {
                        other
                            .report()
                            .report()
                            .print(ConductCache::default())
                            .unwrap();
                        return Err(ParsingError::Handled);
                    }
                }

                // ok it isn't a path sequence, maybe it's a binary operation?
                self.prev();
                match self.parse_binary_operation(Expression::Literal(literal.clone())) {
                    Ok(binary) => return Ok(binary),
                    Err(ParsingError::Handled) => {
                        // all fine, we can move forward
                    }
                    Err(other) => {
                        other
                            .report()
                            .report()
                            .print(ConductCache::default())
                            .unwrap();
                        return Err(ParsingError::Handled);
                    }
                }

                // seems that it's just a literal
                // we need to ensure that its EOL tho
                Ok(Expression::Literal(literal))
            }
            _ => {
                return Err(ParsingError::UnexpectedEOF {
                    at: CodeArea {
                        src: self.source.clone(),
                        line: self.line,
                        span: self.stack.last().unwrap().2,
                    },
                })
            }
        }
    }

    pub fn parse_statement(&mut self) -> Res<Statement> {
        let next = self.next(true);
        match next {
            Some(Token::Import) => {
                // import statement
                let path = check!(self.parse_expression());
                match path {
                    Expression::Literal(ValueBody {
                        value: Literal::Reference(id),
                        ..
                    }) => {
                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Import(id))
                    }
                    Expression::Path(actual_path) => {
                        // validate that it is all period separated
                        let all_periods = actual_path
                            .elements
                            .iter()
                            .all(|each| matches!(each, PathElement::AccessProperty(_)));
                        if !all_periods {
                            return Err(ParsingError::Expected {
                                expected: "a period separated path segment (e.g. `foo.bar.baz`)"
                                    .to_owned(),
                                found: actual_path.to_string(),
                                at: self.area(),
                            });
                        }
                        let mut buf = String::new();
                        let inner = *actual_path.base;
                        match inner {
                            Expression::Literal(ValueBody {
                                value: Literal::Reference(reference),
                                ..
                            }) => {
                                buf += &reference;
                            }
                            other => {
                                return Err(ParsingError::Expected {
                                    expected:
                                        "an identifier based path segment (e.g. `foo.bar.baz`)"
                                            .to_owned(),
                                    found: format!("{other}"),
                                    at: self.area(),
                                });
                            }
                        }
                        let buf =
                            actual_path
                                .elements
                                .iter()
                                .fold(buf, |buffer, each| match each {
                                    PathElement::AccessProperty(prop) => {
                                        buffer + &format!(".{prop}")
                                    }
                                    _ => unreachable!(),
                                });

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Import(buf))
                    }
                    other => Err(ParsingError::Expected {
                        expected: "a period separated path segment (e.g. `foo.bar.baz`)".to_owned(),
                        found: format!("{other}"),
                        at: self.area(),
                    }),
                }
            }
            Some(Token::Return) => {
                // return statement
                match self.next(false) {
                    Some(Token::StatementSeparator) | Some(Token::ClosingCurlyBracket) => {
                        Ok(Statement::Return(Expression::Literal(ValueBody {
                            value: Literal::Nil,
                            operator: None,
                        })))
                    }
                    _ => {
                        self.prev();
                        let expr = check!(self.parse_expression());

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Return(expr))
                    }
                }
            }
            Some(Token::Let) => {
                // let statement
                let name = match self.next(false) {
                    Some(Token::Identifier(id)) => id,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "an identifier (e.g. `foo`)".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };
                match self.next(false) {
                    Some(Token::Assign) => {
                        // we are actually assigning a value here
                        let value = check!(self.parse_expression());

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Variable(name, value))
                    }
                    Some(Token::StatementSeparator) | Some(Token::Comment(_)) | None => {
                        // initializing a nil variable
                        Ok(Statement::Variable(
                            name,
                            Expression::Literal(ValueBody {
                                value: Literal::Nil,
                                operator: None,
                            }),
                        ))
                    }
                    other => Err(ParsingError::Expected {
                        expected: "statement separator or variable value".to_owned(),
                        found: display!(other),
                        at: self.area(),
                    }),
                }
            }
            Some(Token::Const) => {
                // const statement
                let name = match self.next(false) {
                    Some(Token::Identifier(id)) => id,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "an identifier (e.g. `foo`)".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };
                match self.next(false) {
                    Some(Token::Assign) => {
                        // we are actually assigning a value here
                        let value = check!(self.parse_expression());

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Constant(name, value))
                    }
                    other => Err(ParsingError::Expected {
                        expected: "constant value".to_owned(),
                        found: display!(other),
                        at: self.area(),
                    }),
                }
            }
            Some(Token::Native) => {
                match self.next(false) {
                    Some(Token::Const) => {
                        // native const
                        let name = match self.next(false) {
                            Some(Token::Identifier(id)) => id,
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "an identifier (e.g. `foo`)".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        };

                        match self.next(false) {
                            Some(Token::StatementSeparator) | Some(Token::Comment(_)) | None => {}
                            Some(Token::Assign) => {
                                return Err(ParsingError::SyntaxError {
                                    message: format!(
                                        "Native constant `{name}` may not explicitly hold value."
                                    ),
                                    at: self.area(),
                                })
                            }
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "a statement separator".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        }

                        Ok(Statement::NativeConstant(name))
                    }
                    Some(Token::Function) => {
                        // native function

                        let name = match self.next(false) {
                            Some(Token::Identifier(id)) => id,
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "an identifier (e.g. `foo`)".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        };

                        check!(self.ensure(Token::OpenBracket));

                        let mut args: Vec<String> = vec![];

                        loop {
                            match self.next(false) {
                                Some(Token::Identifier(id)) => {
                                    args.push(id);
                                }
                                Some(Token::ClosingBracket) => {
                                    break;
                                }
                                other => {
                                    return Err(ParsingError::Expected {
                                        expected: "an identifier (e.g. `foo`)".to_owned(),
                                        found: display!(other),
                                        at: self.area(),
                                    })
                                }
                            }

                            match self.next(false) {
                                Some(Token::Comma) => {
                                    continue;
                                }
                                Some(Token::ClosingBracket) => {
                                    break;
                                }
                                other => {
                                    return Err(ParsingError::Expected {
                                        expected: "a comma ',' or a closing delimeter ')'"
                                            .to_owned(),
                                        found: display!(other),
                                        at: self.area(),
                                    })
                                }
                            }
                        }

                        match self.next(false) {
                            Some(Token::StatementSeparator) | Some(Token::Comment(_)) | None => {}
                            Some(Token::OpenCurlyBracket) => {
                                return Err(ParsingError::SyntaxError {
                                    message: format!(
                                        "Native function `{name}` may not explicitly have code."
                                    ),
                                    at: self.area(),
                                })
                            }
                            other => {
                                return Err(ParsingError::Expected {
                                    expected: "a statement separator".to_owned(),
                                    found: display!(other),
                                    at: self.area(),
                                })
                            }
                        }

                        Ok(Statement::NativeFunction(name, args))
                    }
                    Some(Token::Let) => Err(ParsingError::FutureFeature {
                        message: "native variables are still planned",
                        at: self.area(),
                    }),
                    other => Err(ParsingError::Expected {
                        expected: "a keyword (fun/const/let)".to_owned(),
                        found: display!(other),
                        at: self.area(),
                    }),
                }
            }
            Some(Token::Function) => {
                // function definition statement

                let name = match self.next(false) {
                    Some(Token::Identifier(id)) => id,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "an identifier (e.g. `foo`)".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };

                check!(self.ensure(Token::OpenBracket));

                let mut args: Vec<String> = vec![];

                loop {
                    match self.next(false) {
                        Some(Token::Identifier(id)) => {
                            args.push(id);
                        }
                        Some(Token::ClosingBracket) => {
                            break;
                        }
                        other => {
                            return Err(ParsingError::Expected {
                                expected: "an identifier (e.g. `foo`)".to_owned(),
                                found: display!(other),
                                at: self.area(),
                            })
                        }
                    }

                    match self.next(false) {
                        Some(Token::Comma) => {
                            continue;
                        }
                        Some(Token::ClosingBracket) => {
                            break;
                        }
                        other => {
                            return Err(ParsingError::Expected {
                                expected: "a comma ',' or a closing delimeter ')'".to_owned(),
                                found: display!(other),
                                at: self.area(),
                            })
                        }
                    }
                }

                check!(self.ensure(Token::OpenCurlyBracket));

                let statements = check!(self.parse_body());

                match self.next_nocomment() {
                    Some(Token::StatementSeparator) => {
                        // eating an unnecessary statement separator, but providing a warning if it is a semicolon
                        if self.slice().contains(';') {
                            let err = error(
                                "A00",
                                self.area(),
                                "Unnecessary semicolon",
                                &[(self.area(), "Here")],
                            );
                            err.builder(ReportKind::Advice)
                                .with_note("This semicolon can be removed.")
                                .finish()
                                .print(ConductCache::default())
                                .unwrap();
                        }
                    }
                    _ => {
                        // returning the token
                        self.prev();
                    }
                }
                Ok(Statement::Function(name, args, statements))
            }
            Some(Token::If) => {
                // parsing the if condition
                let condition = check!(self.parse_expression());
                // making sure we are entering if body
                check!(self.ensure(Token::OpenCurlyBracket));
                // parsing the if body
                let body = check!(self.parse_body());

                // now let's check if the else conditions are present
                let mut else_body: Option<Vec<Statement>> = None;
                let mut else_ifs: Vec<ElseIfStatement> = vec![];
                loop {
                    let next = self.next(true);
                    if next != Some(Token::Else) {
                        self.prev(); // shifting back
                        break;
                    }
                    match self.next(false) {
                        Some(Token::OpenCurlyBracket) => {
                            // parsing else body and ending the loop
                            else_body = Some(check!(self.parse_body()));
                            break;
                        }
                        Some(Token::If) => {
                            // parsing the `else if` condition
                            let else_if_condition = check!(self.parse_expression());
                            // making sure we are entering the `else if` body
                            check!(self.ensure(Token::OpenCurlyBracket));
                            // parsing the `else if` body
                            let else_if_body = check!(self.parse_body());
                            else_ifs.push(ElseIfStatement {
                                condition: else_if_condition,
                                body: else_if_body,
                            })
                        }
                        None => {
                            // we encountered EOF, exit the loop
                            break;
                        }
                        Some(other) => {
                            return Err(ParsingError::Expected {
                                expected: "an `else` or `else if` statement".to_owned(),
                                found: format!("{other}"),
                                at: self.area(),
                            })
                        }
                    }
                }

                match self.next_nocomment() {
                    Some(Token::StatementSeparator) => {
                        // eating an unnecessary statement separator, but providing a warning if it is a semicolon
                        if self.slice().contains(';') {
                            let err = error(
                                "A00",
                                self.area(),
                                "Unnecessary semicolon",
                                &[(self.area(), "Here")],
                            );
                            err.builder(ReportKind::Advice)
                                .with_note("This semicolon can be removed.")
                                .finish()
                                .print(ConductCache::default())
                                .unwrap();
                        }
                    }
                    _ => {
                        // returning the token
                        self.prev();
                    }
                }

                Ok(Statement::If(IfStatement {
                    condition,
                    body,
                    else_ifs,
                    else_body,
                }))
            }
            Some(_) => {
                self.prev(); // shifting backwards

                // parsing an expression
                let expr = check!(self.parse_expression());
                self.next(false);
                // we might have an assignment here, so let's check for it
                match self.parse_assignment() {
                    Some(assignment) => {
                        // this is an assignment statement
                        let next = check!(self.parse_expression());

                        let _ = self.inner_next();
                        Ok(Statement::AssignValue(expr, assignment, next))
                    }
                    None => {
                        // not shifting back because of a statement separator
                        // this is a simple expression statement
                        Ok(Statement::Expression(expr))
                    }
                }
            }
            None => Err(ParsingError::UnexpectedEOF { at: self.area() }),
        }
    }

    fn parse_body(&mut self) -> Res<Vec<Statement>> {
        let mut statements: Vec<Statement> = Vec::new();

        loop {
            if self.next(true) == Some(Token::ClosingCurlyBracket) {
                break;
            }
            self.prev();

            let stmt = check!(self.parse_statement());
            statements.push(stmt);

            if self.next_nocomment() == Some(Token::ClosingCurlyBracket) {
                // encountered end of function body
                break;
            }
            self.prev();
        }
        Ok(statements)
    }

    fn parse_assignment(&self) -> Option<Assignment> {
        Some(match self.current() {
            Some(Token::Assign) => Assignment::Assign,
            Some(Token::AddAssign) => Assignment::AddAssign,
            Some(Token::DivideAssign) => Assignment::DivideAssign,
            Some(Token::ModuloAssign) => Assignment::ModuloAssign,
            Some(Token::MultiplyAssign) => Assignment::MultiplyAssign,
            Some(Token::SubtractAssign) => Assignment::SubtractAssign,
            _ => return None,
        })
    }

    fn parse_binary_operation(&mut self, current: Expression) -> Res<Expression> {
        let _ = self.next(true);

        let mut values: Vec<Expression> = vec![current];
        let mut ops: Vec<BinaryOperator> = vec![];
        let op = if let Some(op) = self.parse_binary_operator() {
            op
        } else {
            self.prev();
            return Err(ParsingError::Handled); // seems it isn't a binary operation
        };
        ops.push(op);
        values.push(check!(self.parse_expression()));

        while self.next(true).is_some() {
            // we are looking for next operator

            if let Some(op) = self.parse_binary_operator() {
                ops.push(op);
                values.push(check!(self.parse_expression()));
            } else {
                break;
            }
        }

        // we need the previous token
        self.prev();

        // fix precedence of operator
        let fixed = Self::fix_precedence(BinaryOperation {
            values,
            operators: ops,
        });

        Ok(Expression::BinaryOperation(fixed))
    }

    fn fix_precedence(mut op: BinaryOperation) -> BinaryOperation {
        for val in &mut op.values {
            if let Expression::BinaryOperation(op) = val {
                *op = Self::fix_precedence(op.clone());
            }
        }

        if op.operators.len() <= 1 {
            op
        } else {
            let mut lowest = HIGHEST_PRECEDENCE;
            let mut assoc = OpAssociativity::Left;

            for operator in &op.operators {
                let p = operator_precedence(operator);
                if p < lowest {
                    lowest = p;
                    assoc = operator_associativity(operator);
                }
            }

            let mut new_expr = BinaryOperation {
                values: vec![],
                operators: vec![],
            };

            let op_loop: Vec<(usize, &BinaryOperator)> = if let OpAssociativity::Left = assoc {
                op.operators.iter().enumerate().rev().collect()
            } else {
                op.operators.iter().enumerate().collect()
            };

            for (i, oper) in op_loop {
                if operator_precedence(oper) == lowest {
                    new_expr.operators.push(*oper);

                    let val1 = if i == op.operators.len() - 1 {
                        op.values.last().unwrap().clone()
                    } else {
                        // expr.operators[(i + 1)..].to_vec(),
                        //     values: expr.values[(i + 1)..]
                        Expression::BinaryOperation(Self::fix_precedence(BinaryOperation {
                            operators: op.operators[(i + 1)..].to_vec(),
                            values: op.values[(i + 1)..].to_vec(),
                        }))
                    };

                    let val2 = if i == 0 {
                        op.values[0].clone()
                    } else {
                        Expression::BinaryOperation(Self::fix_precedence(BinaryOperation {
                            operators: op.operators[..i].to_vec(),
                            values: op.values[..(i + 1)].to_vec(),
                        }))
                    };

                    new_expr.values.push(val1);
                    new_expr.values.push(val2);

                    break;
                }
            }
            new_expr.operators.reverse();
            new_expr.values.reverse();
            new_expr
        }
    }

    fn parse_unary_operator(&self) -> Option<UnaryOperator> {
        Some(match self.current() {
            Some(Token::Exclamation) => UnaryOperator::Bang,
            Some(Token::Minus) => UnaryOperator::Minus,
            _ => return None,
        })
    }

    fn parse_binary_operator(&self) -> Option<BinaryOperator> {
        Some(match self.current() {
            Some(Token::BitwiseOr) => BinaryOperator::BitwiseOr,
            Some(Token::BitwiseAnd) => BinaryOperator::BitwiseAnd,
            Some(Token::BitwiseXor) => BinaryOperator::BitwiseXor,
            Some(Token::Equal) => BinaryOperator::Equals,
            Some(Token::NotEqual) => BinaryOperator::NotEquals,
            Some(Token::LessThan) => BinaryOperator::LessThan,
            Some(Token::MoreThan) => BinaryOperator::GreaterThan,
            Some(Token::MoreOrEqual) => BinaryOperator::GreaterThanOrEqual,
            Some(Token::LessOrEqual) => BinaryOperator::LessThanOrEqual,
            Some(Token::Or) => BinaryOperator::Or,
            Some(Token::And) => BinaryOperator::And,
            Some(Token::ShiftLeft) => BinaryOperator::ShiftLeft,
            Some(Token::ShiftRight) => BinaryOperator::ShiftRight,
            Some(Token::Plus) => BinaryOperator::Add,
            Some(Token::Minus) => BinaryOperator::Subtract,
            Some(Token::Slash) => BinaryOperator::Divide,
            Some(Token::Star) => BinaryOperator::Multiply,
            Some(Token::DoubleStar) => BinaryOperator::Power,
            Some(Token::Modulo) => BinaryOperator::Modulo,
            Some(Token::Is) => BinaryOperator::Is,
            Some(Token::DoublePeriod) => BinaryOperator::Range,
            _ => return None,
        })
    }

    fn parse_path_element(&mut self) -> Res<PathElement> {
        let current = self.current();
        Ok(match current {
            Some(Token::Period) => {
                // access property
                let property = match self.next(true) {
                    Some(Token::Identifier(id)) => id,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "closing delimeter ')'".to_owned(),
                            found: display!(other),
                            at: CodeArea {
                                src: self.source.clone(),
                                line: self.line,
                                span: self.stack.last().unwrap().2,
                            },
                        })
                    }
                };

                PathElement::AccessProperty(property)
            }
            Some(Token::OpenSquareBracket) => {
                // indexing property
                // let _ = self.next(true);
                let index = check!(self.parse_expression());
                match self.next(true) {
                    Some(Token::ClosingSquareBracket) => PathElement::Index(index),
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "closing delimeter ']'".to_owned(),
                            found: display!(other),
                            at: CodeArea {
                                src: self.source.clone(),
                                line: self.line,
                                span: self.stack.last().unwrap().2,
                            },
                        })
                    }
                }
            }
            Some(Token::OpenBracket) => {
                // calling a function
                let mut args: Vec<Expression> = vec![];
                while self.current() != Some(Token::ClosingBracket)
                    && !matches!(self.next(true), Some(Token::ClosingBracket) | None)
                {
                    self.prev(); // moving back to self.current
                    args.push(check!(self.parse_expression()));
                    match self.next(true) {
                        Some(Token::ClosingBracket) | Some(Token::Comma) => {
                            // noop
                        }
                        other => {
                            return Err(ParsingError::Expected {
                                expected: "a comma ',' or a closing delimeter ')'".to_owned(),
                                found: display!(other),
                                at: self.area(),
                            })
                        }
                    }
                }
                PathElement::Invoke(args)
            }
            _ => {
                return Err(ParsingError::Handled);
            }
        })
    }
}
