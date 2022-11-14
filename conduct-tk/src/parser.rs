use ahash::AHashMap;
use ariadne::ReportKind;
use logos::{Lexer, Logos};

use crate::{
    ast::{
        Assignment, BinaryOperation, BinaryOperator, CatchClause, ElseIfStatement, Expression,
        ForLoopStatement, IfStatement, Literal, Path, PathElement, Spanned, Statement, Ternary,
        TryCatchStatement, TypeReference, UnaryOperator, ValueBody,
    },
    check,
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
    8, Left => Modulo Multiply Divide In,
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

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut stmts: Vec<Statement> = vec![];
        while let Ok(stmt) = self.parse_statement() {
            stmts.push(stmt)
        }
        stmts
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

    fn span(&self) -> crate::ast::Span {
        self.stack.last().unwrap().2
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
                        at: self.area(),
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
                    let mut buf: Vec<Spanned<Expression>> = Vec::new();

                    if self.next(true) != Some(Token::ClosingSquareBracket) {
                        self.prev();
                        loop {
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
                                        expected: "a comma ',' or a closing delimeter ']'"
                                            .to_owned(),
                                        found: display!(other),
                                        at: self.area(),
                                    })
                                }
                            }
                        }
                    }
                    Literal::Array(buf)
                }
                Token::OpenCurlyBracket => {
                    // parsing a compound
                    let mut compound: AHashMap<String, Spanned<Expression>> = AHashMap::new();
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
                Token::Type => {
                    // a type definition
                    check!(self.ensure(Token::OpenCurlyBracket));

                    let mut properties: AHashMap<String, String> = AHashMap::new();

                    if self.next(true) != Some(Token::ClosingCurlyBracket) {
                        self.prev();
                        loop {
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

                            let typename = match self.next(true) {
                                Some(Token::StringLiteral(str)) => str,
                                Some(Token::Identifier(id)) => id,
                                other => {
                                    return Err(ParsingError::Expected {
                                        expected: "a type name".to_owned(),
                                        found: display!(other),
                                        at: self.area(),
                                    })
                                }
                            };

                            properties.insert(key, typename);

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
                                        expected: "a comma ',' or a closing delimeter '}'"
                                            .to_owned(),
                                        found: display!(other),
                                        at: self.area(),
                                    })
                                }
                            }
                        }
                    }

                    Literal::TypeDefinition(properties)
                }
                other => {
                    return Err(ParsingError::Expected {
                        expected: "a literal value".to_owned(),
                        found: format!("{other}"),
                        at: self.area(),
                    })
                }
            };
            Ok(ValueBody { value, operator })
        } else {
            Err(ParsingError::Expected {
                at: self.area(),
                expected: "a literal token".to_owned(),
                found: "EOF".to_owned(),
            })
        }
    }

    pub fn parse_expression(&mut self) -> Res<Spanned<Expression>> {
        let next_token = self.next(false);
        match next_token {
            Some(Token::OpenBracket) => {
                // lambda function declaration
                // e.g.
                // (arg1, arg2, ...) => { <snip> }

                let begin = self.span().1;

                let args = check!(self.parse_function_params());

                check!(self.ensure(Token::ThickArrow));

                check!(self.ensure(Token::OpenCurlyBracket));

                let body = check!(self.parse_body());

                if let Some(Token::StatementSeparator) = self.next_nocomment() {
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

                let end = self.span().1;

                self.prev();

                Ok((Expression::Function(args, body), (begin, end)))
            }
            Some(_) => {
                self.prev();
                // anything else that is left are literals
                let begin = self.span().0;
                let literal = check!(self.parse_value());
                // there are multiple cases from which we can go now.
                // A: it's a path sequence ("a".b.c()[d])
                // B: it's a binary operation (a + b) / c
                // C: it's a simple literal (if neither of previous matched)
                let _ = self.next(false);

                // is it a path sequence?
                let value = match self.parse_path_element() {
                    Ok(path) => {
                        let value = Expression::Literal(literal);
                        let mut path = vec![path];
                        let _ = self.next(true);
                        while let Ok(element) = self.parse_path_element() {
                            path.push(element);
                            self.next(true);
                        }
                        self.prev();
                        let end = self.span().1;
                        (
                            Expression::Path(Path {
                                base: Box::new(value),
                                elements: path,
                            }),
                            (begin, end),
                        )
                    }
                    Err(ParsingError::Handled) => {
                        // all fine, we can move forward
                        // ok it isn't a path sequence, maybe it's a binary operation?
                        self.prev();
                        match self.parse_binary_operation((
                            Expression::Literal(literal.clone()),
                            self.span(),
                        )) {
                            Ok(binary) => binary,
                            Err(ParsingError::Handled) => {
                                // all fine, we can move forward

                                // seems that it's just a literal
                                (Expression::Literal(literal), (begin, self.span().1))
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
                    }
                    Err(other) => {
                        other
                            .report()
                            .report()
                            .print(ConductCache::default())
                            .unwrap();
                        return Err(ParsingError::Handled);
                    }
                };

                match self.next_nocomment() {
                    Some(Token::QuestionMark) => {
                        // this is a ternary!
                        // condition ? <if clause> : <else clause>
                        let if_clause = check!(self.parse_expression());

                        check!(self.ensure(Token::Colon));

                        let else_clause = check!(self.parse_expression());

                        let end = self.span().1;

                        Ok((
                            Expression::Ternary(Ternary {
                                condition: Box::new(value),
                                if_clause: Box::new(if_clause),
                                else_clause: Box::new(else_clause),
                            }),
                            (begin, end),
                        ))
                    }
                    _ => {
                        self.prev();

                        Ok(value)
                    }
                }
            }
            _ => Err(ParsingError::UnexpectedEOF { at: self.area() }),
        }
    }

    pub fn parse_statement(&mut self) -> Res<Statement> {
        let next = self.next(true);
        match next {
            Some(Token::Import) => {
                // import statement
                let path = match self.next(true) {
                    Some(Token::Identifier(_)) => check!(self.parse_complex_identifier()),
                    Some(Token::StringLiteral(str)) => str,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "a path to import".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };
                Ok(Statement::Import((path, self.span())))
            }
            Some(Token::Module) => {
                let name = match self.next_nocomment() {
                    Some(Token::Identifier(id)) => id,
                    Some(Token::StringLiteral(str)) => str,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "an identifier".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };
                Ok(Statement::Module((name, self.span())))
            }
            Some(Token::Return) => {
                // return statement
                match self.next(false) {
                    Some(Token::StatementSeparator) | Some(Token::ClosingCurlyBracket) => {
                        Ok(Statement::Return((
                            Expression::Literal(ValueBody {
                                value: Literal::Nil,
                                operator: None,
                            }),
                            self.span(),
                        )))
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
                let name_span = self.span();
                match self.next(false) {
                    Some(Token::Assign) => {
                        // we are actually assigning a value here
                        let value = check!(self.parse_expression());

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Variable((name, name_span), value))
                    }
                    Some(Token::StatementSeparator) | Some(Token::Comment(_)) | None => {
                        // initializing a nil variable
                        Ok(Statement::Variable(
                            (name, name_span),
                            (
                                Expression::Literal(ValueBody {
                                    value: Literal::Nil,
                                    operator: None,
                                }),
                                name_span,
                            ),
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
                let name_span = self.span();
                match self.next(false) {
                    Some(Token::Assign) => {
                        // we are actually assigning a value here
                        let value = check!(self.parse_expression());

                        let _ = self.inner_next(); // eating statement separator

                        Ok(Statement::Constant((name, name_span), value))
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
                        let name_span = self.span();

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

                        Ok(Statement::NativeConstant((name, name_span)))
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
                        let name_span = self.span();

                        check!(self.ensure(Token::OpenBracket));

                        let args = check!(self.parse_function_params());

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

                        Ok(Statement::NativeFunction((name, name_span), args))
                    }
                    Some(Token::Let) => Err(ParsingError::FutureFeature {
                        message: "native variables are still planned",
                        at: self.area(),
                    }),
                    other => Err(ParsingError::Expected {
                        expected: "a keyword (fn/const/let)".to_owned(),
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
                let name_span = self.span();

                check!(self.ensure(Token::OpenBracket));

                let args = check!(self.parse_function_params());

                check!(self.ensure(Token::OpenCurlyBracket));

                let statements = check!(self.parse_body());

                self.eat_postbody_ss();

                Ok(Statement::Function((name, name_span), args, statements))
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

                self.eat_postbody_ss();

                Ok(Statement::If(IfStatement {
                    condition,
                    body,
                    else_ifs,
                    else_body,
                }))
            }
            Some(Token::For) => {
                // for loop
                let iterable = match self.next_nocomment() {
                    Some(Token::Identifier(id)) => id,
                    Some(Token::StringLiteral(str)) => str,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "an iterable name".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };

                check!(self.ensure(Token::In));

                let iterator = check!(self.parse_expression());

                check!(self.ensure(Token::OpenCurlyBracket));

                let body = check!(self.parse_body());

                self.eat_postbody_ss();

                Ok(Statement::ForLoop(ForLoopStatement {
                    iterable,
                    iterator,
                    code: body,
                }))
            }
            Some(Token::While) => {
                // while loop

                let condition = check!(self.parse_expression());

                check!(self.ensure(Token::OpenCurlyBracket));

                let body = check!(self.parse_body());

                self.eat_postbody_ss();

                Ok(Statement::WhileLoop(condition, body))
            }
            Some(Token::Break) => Ok(Statement::Break(self.span())),
            Some(Token::Continue) => Ok(Statement::Continue(self.span())),
            Some(Token::Throw) => {
                let throwable = check!(self.parse_expression());

                let _ = self.next_nocomment(); // eating statement separator

                Ok(Statement::Throw(throwable))
            }
            Some(Token::Try) => {
                check!(self.ensure(Token::OpenCurlyBracket));

                let body = check!(self.parse_body());

                let mut catch_clauses: Vec<CatchClause> = vec![];

                // checking if we have any catch clauses
                // if we don't have any, the code just exits from the try scope silently
                loop {
                    match self.next(false) {
                        Some(Token::Catch) => {
                            let (throwable_name, variable_name) = match self.next(false) {
                                Some(Token::QuestionMark) => (
                                    (
                                        TypeReference {
                                            name: "any".to_owned(),
                                            nullable: true,
                                        },
                                        self.span(),
                                    ),
                                    "_".to_owned(),
                                ),
                                _ => {
                                    let throwable_name = check!(self.parse_typeref());
                                    check!(self.ensure(Token::As));
                                    let variable_name = match self.next_nocomment() {
                                        Some(Token::Identifier(id)) => id,
                                        Some(Token::StringLiteral(str)) => str,
                                        other => {
                                            return Err(ParsingError::Expected {
                                                expected: "an error constant name".to_owned(),
                                                found: display!(other),
                                                at: self.area(),
                                            })
                                        }
                                    };
                                    (throwable_name, variable_name)
                                }
                            };
                            check!(self.ensure(Token::OpenCurlyBracket));
                            let body = check!(self.parse_body());
                            self.eat_postbody_ss();
                            catch_clauses.push(CatchClause {
                                catches: throwable_name,
                                name: variable_name,
                                body,
                            })
                        }
                        _ => {
                            self.prev();
                            break;
                        }
                    }
                }

                Ok(Statement::TryCatch(TryCatchStatement {
                    try_clause: body,
                    catch_clauses,
                }))
            }
            Some(Token::Export) => {
                let path = match self.next(true) {
                    Some(Token::Identifier(_)) => check!(self.parse_complex_identifier()),
                    Some(Token::StringLiteral(str)) => str,
                    other => {
                        return Err(ParsingError::Expected {
                            expected: "a path to export".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                };
                Ok(Statement::Export((path, self.span())))
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
                        Ok(Statement::AssignValue(
                            expr,
                            (assignment, self.span()),
                            next,
                        ))
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

    fn parse_typeref(&mut self) -> Res<Spanned<TypeReference>> {
        let name = match self.current() {
            Some(Token::Star) => "any".to_owned(),
            Some(Token::Identifier(_)) => check!(self.parse_complex_identifier()),
            Some(Token::StringLiteral(strlit)) => strlit,
            other => {
                return Err(ParsingError::Expected {
                    expected: "a type name".to_owned(),
                    found: display!(other),
                    at: self.area(),
                })
            }
        };
        let (begin, mut end) = self.span();

        let nullable = match self.next_nocomment() {
            // a nullable type
            Some(Token::QuestionMark) => {
                end += 1;
                true
            }
            _ => {
                self.prev();
                false
            }
        };

        Ok((TypeReference { name, nullable }, (begin, end)))
    }

    fn parse_complex_identifier(&mut self) -> Res<String> {
        let mut out = match self.current() {
            Some(Token::Identifier(id)) => id,
            other => {
                return Err(ParsingError::Expected {
                    expected: "an identifier".to_owned(),
                    found: display!(other),
                    at: self.area(),
                })
            }
        };
        loop {
            if self.next(true) != Some(Token::Period) {
                self.prev();
                break;
            }
            out.push('.');
            out += &match self.next(true) {
                Some(Token::StringLiteral(str)) => str,
                Some(Token::Identifier(id)) => id,
                other => {
                    return Err(ParsingError::Expected {
                        expected: "an identifier".to_owned(),
                        found: display!(other),
                        at: self.area(),
                    })
                }
            }
        }
        Ok(out)
    }

    fn parse_function_params(&mut self) -> Res<Vec<Spanned<String>>> {
        let mut args: Vec<Spanned<String>> = vec![];

        loop {
            match self.next(false) {
                Some(Token::Identifier(id)) => {
                    args.push((id, self.span()));
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

        Ok(args)
    }

    fn eat_postbody_ss(&mut self) {
        if let Some(Token::StatementSeparator) = self.next_nocomment() {
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
        self.prev();
    }

    fn parse_body(&mut self) -> Res<Vec<Statement>> {
        let mut statements: Vec<Statement> = Vec::new();

        if self.next(true) != Some(Token::ClosingCurlyBracket) {
            self.prev();
            loop {
                let stmt = check!(self.parse_statement());
                statements.push(stmt);

                if self.next(true) == Some(Token::ClosingCurlyBracket) {
                    // encountered end of function body
                    break;
                }
                self.prev();
            }
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

    fn parse_binary_operation(&mut self, current: Spanned<Expression>) -> Res<Spanned<Expression>> {
        let _ = self.next(true);
        let begin = self.span().1;

        let mut values = vec![current];
        let mut ops: Vec<Spanned<BinaryOperator>> = vec![];
        let op = if let Some(op) = self.parse_binary_operator() {
            (op, self.span())
        } else {
            self.prev();
            return Err(ParsingError::Handled); // seems it isn't a binary operation
        };
        ops.push(op);
        values.push(check!(self.parse_expression()));

        while self.next(true).is_some() {
            // we are looking for next operator

            if let Some(op) = self.parse_binary_operator() {
                ops.push((op, self.span()));
                values.push(check!(self.parse_expression()));
            } else {
                break;
            }
        }

        let end = self.span().1;

        // we need the previous token
        self.prev();

        // fix precedence of operator
        let fixed = Self::fix_precedence((
            BinaryOperation {
                values,
                operators: ops,
            },
            (begin, end),
        ));

        Ok((Expression::BinaryOperation(fixed.0), fixed.1))
    }

    fn fix_precedence(mut op: Spanned<BinaryOperation>) -> Spanned<BinaryOperation> {
        for val in &mut op.0.values {
            if let (Expression::BinaryOperation(op), span) = val {
                *op = Self::fix_precedence((op.clone(), *span)).0;
            }
        }

        if op.0.operators.len() <= 1 {
            op
        } else {
            let mut lowest = HIGHEST_PRECEDENCE;
            let mut assoc = OpAssociativity::Left;

            for operator in &op.0.operators {
                let p = operator_precedence(&operator.0);
                if p < lowest {
                    lowest = p;
                    assoc = operator_associativity(&operator.0);
                }
            }

            let mut new_expr = BinaryOperation {
                values: vec![],
                operators: vec![],
            };

            let op_loop: Vec<(usize, &Spanned<BinaryOperator>)> =
                if let OpAssociativity::Left = assoc {
                    op.0.operators.iter().enumerate().rev().collect()
                } else {
                    op.0.operators.iter().enumerate().collect()
                };

            for (i, oper) in op_loop {
                if operator_precedence(&oper.0) == lowest {
                    new_expr.operators.push(*oper);

                    let val1 = if i == op.0.operators.len() - 1 {
                        op.0.values.last().unwrap().clone()
                    } else {
                        // expr.operators[(i + 1)..].to_vec(),
                        //     values: expr.values[(i + 1)..]
                        (
                            Expression::BinaryOperation(
                                Self::fix_precedence((
                                    BinaryOperation {
                                        operators: op.0.operators[(i + 1)..].to_vec(),
                                        values: op.0.values[(i + 1)..].to_vec(),
                                    },
                                    op.1,
                                ))
                                .0,
                            ),
                            op.1,
                        )
                    };

                    let val2 = if i == 0 {
                        op.0.values[0].clone()
                    } else {
                        (
                            Expression::BinaryOperation(
                                Self::fix_precedence((
                                    BinaryOperation {
                                        operators: op.0.operators[..i].to_vec(),
                                        values: op.0.values[..(i + 1)].to_vec(),
                                    },
                                    op.1,
                                ))
                                .0,
                            ),
                            op.1,
                        )
                    };

                    new_expr.values.push(val1);
                    new_expr.values.push(val2);

                    break;
                }
            }
            new_expr.operators.reverse();
            new_expr.values.reverse();
            (new_expr, op.1)
        }
    }

    fn parse_unary_operator(&self) -> Option<UnaryOperator> {
        Some(match self.current() {
            Some(Token::Exclamation) => UnaryOperator::Bang,
            Some(Token::Minus) => UnaryOperator::Minus,
            Some(Token::Increment) => UnaryOperator::Increment,
            Some(Token::Decrement) => UnaryOperator::Decrement,
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
            Some(Token::In) => BinaryOperator::In,
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
                            expected: "a closing delimeter ')'".to_owned(),
                            found: display!(other),
                            at: self.area(),
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
                            expected: "a closing delimeter ']'".to_owned(),
                            found: display!(other),
                            at: self.area(),
                        })
                    }
                }
            }
            Some(Token::OpenBracket) => {
                // calling a function
                let mut args: Vec<Spanned<Expression>> = vec![];
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
            Some(Token::DoubleBang) => PathElement::NullAssert,
            _ => {
                return Err(ParsingError::Handled);
            }
        })
    }
}
