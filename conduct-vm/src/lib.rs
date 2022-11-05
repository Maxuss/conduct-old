#![allow(dead_code)]

pub mod op;
pub mod reg;
pub mod vm;

#[cfg(test)]
mod tests {
    use crate::asm;
    use crate::op::Opcode;
    use crate::vm::Vm;

    #[test]
    fn test_basic_opcodes() {
        let vm = Vm::new();
        let opcodes = asm! {
            NOP
            // defines a global constant with name 'my_var' and value 123
            PUSH ["my_var"]
            PUSH [123]
            DEF_GLOBAL_CONST

            // concatenates two strings into 'my_var'
            PUSH ["my"]
            PUSH ["_var"]
            CONCAT

            // loads global with name 'my_var'
            LOAD_GLOBAL

            // defines a global mutable variable with name 'mut' and nil value
            PUSH ["mut"]
            PUSH [vec![0x03, 0x00]]
            DEF_GLOBAL_MUT

            // loads a global variable with name 'mut'
            PUSH ["mut"]
            LOAD_GLOBAL
        };
        vm.run(&opcodes);
        println!()
    }
}
