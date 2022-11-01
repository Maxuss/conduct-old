pub mod ast;
pub mod err;
pub mod parser;
pub mod tk;

#[cfg(test)]
mod tests {
    use ariadne::Fmt;
    use logos::Logos;

    use crate::{
        err::{CodeArea, CodeSource, ConductCache, ErrorReport, FancyColorGenerator},
        parser::Parser,
        tk::Token,
    };

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
        let mut parser = Parser::new_inline("1234 + 16 ** 12 / 13 == 24");
        let expr = parser.parse_expression();
        assert!(expr.is_ok())
    }

    #[test]
    fn paths() {
        let mut parser = Parser::new_inline("variable.property[index](arg1, arg2, 0xBAD,)");
        let expr = parser.parse_expression();
        assert!(expr.is_ok())
    }

    #[test]
    fn unary_operator() {
        let mut parser = Parser::new_inline("!true & -3 ** 4");
        let expr = parser.parse_expression();
        assert!(expr.is_ok())
    }

    #[test]
    fn error_reports() {
        println!();
        let area1 = CodeArea {
            src: CodeSource::File("src/parser.rs".into()),
            line: 10,
            span: (276, 360),
        };
        let area2 = CodeArea {
            src: CodeSource::Inline("let b = false\nlet a = different".to_owned()),
            line: 2,
            span: (14, 31),
        };
        let area_current = CodeArea {
            src: CodeSource::Inline("import 'tech/test.cd'".to_owned()),
            line: 1,
            span: (7, 21),
        };

        let mut colors = FancyColorGenerator::default();

        let report = ErrorReport {
            call_stack: vec![area1.clone(), area2.clone()],
            current_module: "tests".to_owned(),
            position: area_current.clone(),
            message: format!("Syntax error"),
            labels: vec![
                (
                    area1,
                    format!(
                        "Something went {} wrong here",
                        "very".fg(colors.next_color())
                    ),
                ),
                (
                    area2,
                    format!(
                        "And then this thing {} too!",
                        "failed miserably".fg(colors.next_color())
                    ),
                ),
                (
                    area_current,
                    format!(
                        "This is literally {}. The Gorbino's Quest of {}",
                        "Gorbino's Quest".fg(colors.next_color()),
                        "Errors".fg(colors.next_color())
                    ),
                ),
            ],
        };
        assert!(report.report().print(ConductCache::default()).is_ok());
    }

    #[test]
    fn errors() {
        let mut parser = Parser::new_inline("val(a");
        let expr = parser.parse_expression();
        assert!(expr.is_err())
    }
}
