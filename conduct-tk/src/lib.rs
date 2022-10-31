pub mod ast;
pub mod parser;
pub mod tk;

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::{parser::Parser, tk::Token};

    #[test]
    fn basic_tokenization() {
        let text = r#"
        /*
        Line 1 comment
        Line 2 comment
        */
        import std.io

        let var = 0xFFAAFF
        var = 0b010101
        var = 0o143047
        var = 1234567890123456
        var = "Hello, World!"

        native fun callable(name) {
            println("Hello ${name}")
        }
        "#;
        let mut lexer = Token::lexer(text.trim());
        while let Some(tk) = lexer.next() {
            println!("{tk:?}")
        }
    }

    #[test]
    fn binary_ops() {
        let mut parser = Parser::new(Token::lexer("1234 + 16 ** 12 / 13 == 24"));
        let expr = parser.parse_expression();
        println!("{expr:?}")
    }

    #[test]
    fn paths() {
        let mut parser = Parser::new(Token::lexer("variable.property[index](arg1, arg2, 0xBAD,)"));
        let expr = parser.parse_expression();
        println!("{expr:?}");
    }

    #[test]
    fn unary_operator() {
        let mut parser = Parser::new(Token::lexer("!true & -3 ** 4"));
        let expr = parser.parse_expression();
        println!("{expr:?}");
    }
}
