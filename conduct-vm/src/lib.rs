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
        let mut vm = Vm::new();
        let ptr1 = vm.store_str("Hello, ");
        let ptr2 = vm.store_str("World!");
        let opcodes = asm! {
            NOP
            PUSH [ptr1]
            RDLOAD
            PUSH [ptr2]
            RDLOAD
            CONCAT
            HDEBUG
        };
        vm.run(&opcodes);
    }
}
