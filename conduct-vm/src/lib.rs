pub mod mem;
pub mod op;
pub mod translator;
pub mod vm;

#[cfg(test)]
mod tests {
    use conduct_tk::{check, err::Res, parser::Parser, validate::Validator};

    use crate::translator::Ast2OpTranslator;

    use super::*;

    #[test]
    fn build_opcodes() -> Res<()> {
        let translator = Ast2OpTranslator::new();
        let parser = Parser::new_inline(
            r#"
fn main() {
    let a = "First"
    let b = "Second"
    if false {
        debug(a)
    } else if true {
        debug("Another")
    } else {
        debug(b)
    }
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
