pub mod ast;
pub mod err;
pub mod parser;
pub mod tk;

#[cfg(test)]
mod tests {
    use ariadne::Fmt;
    use logos::Logos;

    use crate::{
        check,
        err::{CodeArea, CodeSource, ConductCache, ErrorReport, FancyColorGenerator, Res},
        parser::Parser,
        tk::Token,
    };

    #[allow(unused_macros)]
    macro_rules! printcheck {
        ($expr:expr) => {
            println!("{:#?}", check!($expr))
        };
    }

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
            code: "E99",
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

    #[test]
    fn stmt_import() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
import std
import a.b.c
import _native.module
        "#
            .trim(),
        );
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        Ok(())
    }

    #[test]
    fn stmt_return() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
return
return abc
return 123
        "#
            .trim(),
        );
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        Ok(())
    }

    #[test]
    fn stmt_let() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
let a
let b = 1 + d()
let c = nil
        "#
            .trim(),
        );
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_const() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
const a = 0xFFAAFF;
const b = 1 + d()
const c = nil
        "#
            .trim(),
        );
        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_native_const() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
native const a;
native const b
native const internal$constant
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_native_fun() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
native fun pow(a, b)
native fun eval(code)
native fun noargs()
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_native_let() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
native let a;
        "#
            .trim(),
        );

        assert!(parser.parse_statement().is_err());

        Ok(())
    }

    #[test]
    fn stmt_fun() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#" 
fun main(args) {
    let a = 123
    let b = 456
}

fun empty() {

}

fun semicolon() {

};
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_if() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#" 
if true {
    // empty
}

if !false {
    let a = b
} else {
    // do other stuff
}

if false {
    let a = b
} else if true {
    let a = c
} else if nil {
    let a = d
} else {
    let a = nil
}
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_assign() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#" 
// let a

a = false
a += 1
a -= "Hello, World!"
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn stmt_expr() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#" 
// import sys
// import std.io

println('Hello, ${env[0]}')
12
file.create(args[0])
        "#
            .trim(),
        );

        check!(parser.parse_statement());
        check!(parser.parse_statement());
        check!(parser.parse_statement());

        Ok(())
    }

    #[test]
    fn literal_array() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#" 
[]
[[1, 2, 3], [1, 2, 3], [1, 2, 3]]
[
    'a',
    'b',
    'c'
]
        "#
            .trim(),
        );

        check!(parser.parse_value());
        check!(parser.parse_value());
        check!(parser.parse_value());

        Ok(())
    }

    #[test]
    fn literal_compound() -> Res<()> {
        let mut parser = Parser::new_inline(
            r#"
{
    int: 123,
    'string': "Hello, World!",
    array: [1, 2, 3]
}
{
    nested_object: {
        abc: 123
    }
}
{}
        "#
            .trim(),
        );

        printcheck!(parser.parse_value());
        printcheck!(parser.parse_value());
        printcheck!(parser.parse_value());

        Ok(())
    }
}
