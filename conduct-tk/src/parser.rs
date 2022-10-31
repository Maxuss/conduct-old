use logos::Lexer;

use crate::{
    ast::{
        BinaryOperation, BinaryOperator, Expression, Literal, Path, PathElement, UnaryOperator,
        ValueBody,
    },
    tk::Token,
};

pub struct Parser<'lex> {
    lexer: Lexer<'lex, Token>,
    index: usize,
    stack: Vec<(Option<Token>, String, (usize, usize))>,
}

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

impl<'lex> Parser<'lex> {
    pub fn new(lexer: Lexer<'lex, Token>) -> Self {
        Self {
            lexer,
            index: 0,
            stack: vec![],
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
            && match next {
                Some(Token::StatementSeparator) | Some(Token::Comment(_)) => true,
                _ => false,
            }
        {
            self.next(ignore_unneeded)
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
                && match self.stack[len - self.index - 1].0 {
                    Some(Token::StatementSeparator) | Some(Token::Comment(_)) => true,
                    _ => false,
                }
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

    pub fn parse_value(&mut self) -> Option<ValueBody> {
        if let Some(mut value) = self.next(true) {
            let operator = if let Some(op) = self.parse_unary_operator() {
                value = if let Some(value) = self.next(true) {
                    value
                } else {
                    panic!("Unexpected EOF!")
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
                other => panic!("Invalid literal value: {other:?}"),
            };
            Some(ValueBody { value, operator })
        } else {
            None
        }
    }

    pub fn parse_expression(&mut self) -> Option<Expression> {
        let next_token = self.next(false);
        match next_token {
            Some(Token::OpenBracket) => {
                // lambda function declaration
                // e.g.
                // (arg1, arg2, ...) => { <snip> }
                // TODO: parse function
                return None;
            }
            Some(_) => {
                self.prev();
                // anything else that is left are literals
                let literal = self.parse_value()?;
                // there are multiple cases from which we can go now.
                // A: it's a path sequence ("a".b.c()[d])
                // B: it's a binary operation (a + b) / c
                // C: it's a simple literal (if neither of previous matched)
                let _ = self.next(false);

                // is it a path sequence?
                if let Some(path) = self.parse_path_element() {
                    let value = Expression::Literal(literal);
                    let mut path = vec![path];
                    let _ = self.next(true);
                    while let Some(element) = self.parse_path_element() {
                        path.push(element);
                        self.next(true);
                    }
                    self.prev();
                    return Some(Expression::Path(Path {
                        base: Box::new(value),
                        elements: path,
                    }));
                }

                // ok it isn't a path sequence, maybe it's a binary operation?
                self.prev();
                if let Some(binary_operator) =
                    self.parse_binary_operation(Expression::Literal(literal.clone()))
                {
                    return Some(binary_operator);
                }

                // seems that it's just a literal
                Some(Expression::Literal(literal))
            }
            _ => {
                // unexpected EOF!
                return None;
            }
        }
    }

    fn parse_binary_operation(&mut self, current: Expression) -> Option<Expression> {
        let _ = self.next(true);

        let mut values: Vec<Expression> = vec![current];
        let mut ops: Vec<BinaryOperator> = vec![];
        let op = if let Some(op) = self.parse_binary_operator() {
            op
        } else {
            self.prev();
            return None;
        };
        ops.push(op);
        values.push(self.parse_expression()?);

        while let Some(_) = self.next(true) {
            // we are looking for next operator

            if let Some(op) = self.parse_binary_operator() {
                ops.push(op);
                values.push(self.parse_expression()?);
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

        match self.next(false) {
            Some(Token::QuestionMark) => {
                // TODO: parse ternaries
                Some(Expression::BinaryOperation(fixed))
            }
            _ => {
                // once again, the token was unneeded
                self.inner_prev(false);
                Some(Expression::BinaryOperation(fixed))
            }
        }
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

    fn parse_path_element(&mut self) -> Option<PathElement> {
        let current = self.current();
        Some(match current {
            Some(Token::Period) => {
                // access property
                let property = match self.next(true)? {
                    Token::Identifier(id) => id,
                    other => panic!("Invalid token! Expected an identifier but got {other:?}"),
                };

                PathElement::AccessProperty(property)
            }
            Some(Token::OpenSquareBracket) => {
                // indexing property
                // let _ = self.next(true);
                let index = self.parse_expression()?;
                match self.next(true) {
                    Some(Token::ClosingSquareBracket) => PathElement::Index(index),
                    other => panic!("Unclosed square bracket! Got {other:?}"),
                }
            }
            Some(Token::OpenBracket) => {
                // calling a function
                let mut args: Vec<Expression> = vec![];
                while self.next(true) != Some(Token::ClosingBracket) {
                    self.prev();
                    args.push(self.parse_expression()?);
                    match self.next(true) {
                        Some(Token::ClosingBracket) | Some(Token::Comma) => {
                            // noop
                        }
                        other => {
                            panic!("Expected a comma but got {other:?}!")
                        }
                    }
                }
                PathElement::Invoke(args)
            }
            _ => {
                return None;
            }
        })
    }
}
