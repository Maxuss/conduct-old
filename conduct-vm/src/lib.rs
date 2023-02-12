pub mod mem;
pub mod op;
pub mod translator;

#[cfg(test)]
mod tests {
    use conduct_tk::{check, err::Res, parser::Parser, validate::Validator};

    use crate::translator::Ast2OpTranslator;

    use super::*;

    #[test]
    fn build_opcodes() -> Res<()> {
        let mut translator = Ast2OpTranslator::new();
        let parser = Parser::new_inline(
            r#"
import cdplus.fns

print("Hello, World!")
let a = 123
const b = !false

if true {
    failure().unwrap()
} else {
    println(readln())
}"#,
        );
        let validator = Validator::from(&parser);
        check!(parser
            .then_pipe(validator)
            .then_pipe(translator)
            .finish_pipeline());
        Ok(())
    }
}
