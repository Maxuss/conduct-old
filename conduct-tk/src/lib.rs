pub mod tk;

#[cfg(test)]
mod tests {
    use logos::Logos;

    use crate::tk::Token;

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
}
