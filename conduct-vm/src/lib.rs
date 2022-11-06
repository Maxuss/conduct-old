#![allow(dead_code)]

pub mod ffi;
pub mod fnc;
pub mod op;
pub mod reg;
pub mod stdlib;
pub mod vm;

#[cfg(test)]
mod tests {

    use conduct_tk::AHashMap;

    use crate::op::Opcode;
    use crate::reg::StackValue;
    use crate::vm::{Variable, Vm};
    use crate::{asm, unwrap_args};

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
            PUSH [vec![0x03]]
            DEF_GLOBAL_MUT

            // loads a global variable with name 'mut'
            PUSH ["mut"]
            LOAD_GLOBAL
        };
        vm.run(&opcodes);
    }

    #[test]
    fn test_predefined_opcodes() {
        let vm = Vm::new();
        let opcodes = asm! {
            NOP

            PUSH ["__VM_STACK_SIZE"]
            LOAD_GLOBAL
            DEBUG

            PUSH ["__VM_HEAP_PREALLOC"]
            LOAD_GLOBAL
            DEBUG
        };
        vm.run(&opcodes);
    }

    #[test]
    fn test_native_values() {
        let mut vm = Vm::new();
        let opcodes = asm! {
            NOP

            PUSH ["NATIVE_CONST"]
            LOAD_NATIVE
            PUSH *0x59
            LOAD_NATIVE
            DEBUG
        };
        vm.add_native_value("NATIVE_CONST", |_| Variable {
            mutable: false,
            value: StackValue::Number(rand::random::<i32>() as f64),
        });
        vm.run(&opcodes);
    }

    #[test]
    fn test_scopes() {
        let vm = Vm::new();
        // equal to
        /*
        {
            {
                const local = 12
                assert local == 12
            }
            assert local == nil
        }
        */
        let opcodes = asm! {
            NOP

            PUSH_SCOPE

            PUSH_SCOPE

            PUSH ["local"]
            PUSH [12]
            DEF_LOCAL_CONST
            PUSH ["local"]
            LOAD_LOCAL
            PUSH [12]
            EQ
            ASSERT

            POP_SCOPE

            PUSH ["local"]
            LOAD_LOCAL
            PUSH [vec![0x03]]
            EQ
            ASSERT

            POP_SCOPE
        };
        vm.run(&opcodes);
    }

    #[test]
    fn loop_flow() {
        let mut vm = Vm::new();
        // equal to
        /*
        const MESSAGE = "Processing loop"
        // native const CHECK
        while CHECK {
            debug MESSAGE
        }
        */
        let opcodes = asm! {
            NOP

            PUSH ["MESSAGE"]
            PUSH ["Processing loop"]
            DEF_GLOBAL_CONST

            PUSH ["CHECK"]
            LOAD_NATIVE
            NEG
            JMP_IF [vec![0x00, 17]]
            PUSH_SCOPE

            PUSH ["MESSAGE"]
            LOAD_GLOBAL
            DEBUG

            POP_SCOPE
            JMPB [vec![0x00, 30]]
        };
        vm.add_native_value("CHECK", |_| Variable {
            mutable: false,
            value: StackValue::Boolean(rand::random()),
        });

        vm.run(&opcodes);
    }

    #[test]
    fn conditional_flow() {
        let mut vm = Vm::new();
        // equal to
        /*
        // native const A
        // native const B

        const IF = "Encountered IF"
        const ELSE_IF = "Encountered ELSE IF"
        const ELSE = "Encountered ELSE"

        if A {
            debug IF
        } else if B {
            debug ELSE_IF
        } else {
            debug ELSE
        }
        */
        let opcodes = asm! {
            NOP

            PUSH ["A"]
            PUSH ["B"]

            PUSH ["IF"]
            PUSH ["Encountered IF"]
            DEF_GLOBAL_CONST

            PUSH ["ELSE_IF"]
            PUSH ["Encountered ELSE IF"]
            DEF_GLOBAL_CONST

            PUSH ["ELSE"]
            PUSH ["Encountered ELSE"]
            DEF_GLOBAL_CONST

            ///////////////
            PUSH *0x59
            LOAD_NATIVE
            NEG
            JMP_IF [vec![0x00, 17]] // if
            PUSH_SCOPE

            PUSH *0x5F
            LOAD_GLOBAL
            DEBUG

            POP_SCOPE
            JMPF [vec![0x00, 29]] // exit scope

            ///////////////
            PUSH *0x5C
            LOAD_NATIVE
            NEG
            JMP_IF [vec![0x00, 15]] // else if
            PUSH_SCOPE

            PUSH *0x73
            LOAD_GLOBAL
            DEBUG

            POP_SCOPE
            JMPF [vec![0x00, 14]] // exit scope

            ///////////////
            PUSH_SCOPE // else

            PUSH *0x91
            LOAD_GLOBAL
            DEBUG

            POP_SCOPE
        };
        vm.add_native_value("A", |_| Variable {
            mutable: false,
            value: StackValue::Boolean(rand::random()),
        });
        vm.add_native_value("B", |_| Variable {
            mutable: false,
            value: StackValue::Boolean(rand::random()),
        });
        vm.run(&opcodes);
    }

    #[test]
    fn native_ffi_functions() {
        fn native(vm: &mut Vm, args: AHashMap<String, StackValue>) -> StackValue {
            unwrap_args!(args {
                let first: HeapPointer;
            });
            let first = vm.read_value(first).unwrap();
            vm.alloc_str(format!("Hello, {}!", first))
        }

        let mut vm = Vm::new();
        // equal to
        /*
        import maxus.ffi
        import std.io

        println(say_hello("World"))
        */
        let opcodes = asm! {
            NOP
            IMPORT ["maxus.ffi"]
            IMPORT ["std.io"]

            PUSH ["World"]
            PUSH ["say_hello"]
            CALL_NATIVE
            PUSH ["println"]
            CALL_NATIVE
        };
        vm.add_native_function("maxus.ffi", "say_hello", vec!["first".to_owned()], native);
        vm.run(&opcodes);
    }

    #[test]
    fn normal_functions() {
        let vm = Vm::new();
        // equal to
        /*
        module self

        fn my_function(arg1) {
            debug("Hello, " + arg1")
        }

        my_function("World")
        my_function("Another Param")
        */

        let opcodes = asm! {
            NOP

            PUSH ["self"]
            MODULE

            PUSH ["my_function"]
            PUSH ["arg1"]
            FUNCTION [vec![0x01, 0x00, 21]] // first number is argument count, the following two are big endian u16 body size
            PUSH ["Hello, "] // 10
            PUSH ["arg1"] // 7
            LOAD_LOCAL // 1
            CONCAT // 1
            DEBUG

            PUSH ["my_function"]
            PUSH ["World"]
            CALL [vec![0x01]]

            PUSH ["my_function"]
            PUSH ["Another Param"]
            CALL [vec![0x01]]
        };
        vm.run(&opcodes).unwrap();
    }

    #[test]
    fn inline_functions() {
        let vm = Vm::new();
        // equal to
        /*
        module self

        const CONST_FUNCTION = (arg1) => {
            debug("Hello, " + arg1)
        }

        debug(CONST_FUNCTION)
        */

        let opcodes = asm! {
            NOP

            PUSH ["self"]
            MODULE

            PUSH ["CONST_FUNCTION"]
            /////
            PUSH ["arg1"]
            INLINE_FUNCTION [vec![0x01, 0x00, 21]] // first number is argument count, the following two are big endian u16 body size
            PUSH ["Hello, "] // 10
            PUSH ["arg1"] // 7
            LOAD_LOCAL // 1
            CONCAT // 1
            DEBUG
            /////
            DEF_GLOBAL_CONST

            PUSH *0x90
            LOAD_GLOBAL
            PUSH ["World"]
            CALL [vec![0x01]]
        };
        vm.run(&opcodes).unwrap();
    }
}
