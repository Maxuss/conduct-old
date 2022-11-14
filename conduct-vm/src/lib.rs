#![allow(dead_code)]

pub mod bc;
pub mod ffi;
pub mod fnc;
pub mod heap;
pub mod op;
pub mod rt;
pub mod stdlib;
pub mod value;
pub mod vm;

#[cfg(test)]
mod tests {

    use conduct_tk::AHashMap;

    use crate::op::Opcode;
    use crate::rt::Runtime;
    use crate::value::Value;
    use crate::vm::Vm;
    use crate::{asm, unwrap_args};

    #[test]
    fn test_basic_opcodes() {
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
            PUSH [3]
            DEF_GLOBAL_MUT

            // loads a global variable with name 'mut'
            PUSH ["mut"]
            LOAD_GLOBAL

            HLT
        };
        let vm = Vm::new_bc(&opcodes);
        vm.run();
    }

    #[test]
    fn test_predefined_opcodes() {
        let opcodes = asm! {
            NOP

            PUSH ["__VM_STACK_SIZE"]
            LOAD_GLOBAL
            DEBUG

            PUSH ["__VM_HEAP_PREALLOC"]
            LOAD_GLOBAL
            DEBUG

            HLT
        };
        let vm = Vm::new_bc(&opcodes);

        vm.run();
    }

    #[test]
    fn test_native_values() {
        let opcodes = asm! {
            NOP

            PUSH ["NATIVE_CONST"]
            LOAD_NATIVE
            DEBUG

            HLT
        };

        let vm = Vm::new_bc(&opcodes);
        vm.rt
            .add_native_const("NATIVE_CONST", Value::Number(rand::random::<i32>() as f64));
        vm.run();
    }

    #[test]
    fn test_scopes() {
        let opcodes = asm! {
            NOP

            PUSH ["local"]
            PUSH [12]
            DEF_LOCAL_CONST [vec![10, 0, 0, 0]]
            LOAD_LOCAL [vec![10, 0, 0, 0]]
            PUSH [12]
            EQ
            ASSERT

            LOAD_LOCAL [vec![10, 0, 0, 0]]
            PUSH [vec![0x03]]
            NEQ
            ASSERT

            HLT
        };
        let vm = Vm::new_bc(&opcodes);
        // equal to
        /*
        {
            const local = 12
            assert local == 12
            assert local == nil
        }
        */

        vm.run();
    }

    #[test]
    fn native_ffi_functions() {
        fn native(rt: &mut Runtime, args: AHashMap<String, Value>) -> Value {
            unwrap_args!(args {
                let first: String;
            });
            Value::String(rt.alloc(format!("Hello, {}!", *first)))
        }
        let opcodes = asm! {
            NOP
            IMPORT ["maxus.ffi"]
            IMPORT ["std.io"]

            PUSH ["World"]
            PUSH ["say_hello"]
            CALL_NATIVE
            PUSH ["println"]
            CALL_NATIVE

            HLT
        };
        let vm = Vm::new_bc(&opcodes);
        // equal to
        /*
        import maxus.ffi
        import std.io

        println(say_hello("World"))
        */

        vm.rt
            .add_native_function("say_hello", vec!["first".to_owned()], native);
        vm.run();
    }

    #[test]
    fn normal_functions() {
        let opcodes = asm! {
            NOP

            PUSH ["my_function"]
            PUSH ["arg1"]
            FUNCTION [vec![0x01, 0x00, 1, 12, 0, 0, 0]] // first number is argument count, the following two are big endian u16 body size

            PUSH ["World"]
            LOAD_LOCAL [vec![12, 0, 0, 0]]
            CALL [vec![0x01]]
            DEBUG

            HLT
        };

        let mut vm = Vm::new_bc(&opcodes);

        vm.rt.preload_function_bytecode(asm! {
            PUSH ["Hello, "]
            LOAD_LOCAL [vec![0, 0, 0, 0]]
            CONCAT
            RETURN
        });

        // equal to
        /*
        module self

        fn my_function(arg1) {
            debug("Hello, " + arg1")
        }

        my_function("World")
        my_function("Another Param")
        */

        vm.run().unwrap();
    }

    #[test]
    fn arrays() {
        let opcodes = asm! {
            NOP

            PUSH ["self"]
            MODULE

            PUSH ["my_array"]
            PUSH ["abc"]
            PUSH ["def"]
            PUSH ["ghi"]
            PUSH [3]
            ARRAY
            DEF_GLOBAL_CONST

            PUSH ["my_array"]
            LOAD_GLOBAL
            PUSH [core::ops::Range { start: 0, end: 2 }]
            INDEX
            PUSH [0]
            INDEX
            DEBUG

            HLT
        };
        let vm = Vm::new_bc(&opcodes);
        // equal to
        /*
        module self

        const my_array = ["abc", "def", "ghi"]

        debug(my_array[0..2][0])

        */

        vm.run().unwrap();
    }

    #[test]
    fn compounds() {
        let opcodes = asm! {
            NOP

            PUSH ["self"]
            MODULE

            PUSH ["my_compound"]
            PUSH ["a"]
            PUSH ["123"]
            PUSH ["b"]
            PUSH [3.]
            PUSH ["c"]
            PUSH ["a third key"]
            PUSH [3]
            COMPOUND
            DEF_GLOBAL_CONST

            PUSH ["my_compound"]
            LOAD_GLOBAL
            PUSH ["c"]
            INDEX
            PUSH [3..5]
            INDEX
            PUSH ["hi"]
            EQ
            ASSERT

            HLT
        };

        let vm = Vm::new_bc(&opcodes);
        // equal to
        /*
        module self

        const my_compound = {
            a: "123",
            b: 3,
            c: "a third key"
        }

        assert my_compound["c"][3..5] == "hi"

        */

        vm.run().unwrap();
    }

    #[test]
    fn closures() {
        let opcodes = asm! {
            NOP

            PUSH ["my_function"]
            PUSH ["arg1"]
            CLOSURE [vec![0x01, 0x00, 1]] // first number is argument count, the following two are big endian u16 body size
            DEF_GLOBAL_CONST

            PUSH ["World"]
            PUSH ["my_function"]
            LOAD_GLOBAL
            CALL [vec![0x01]]
            DEBUG

            HLT
        };

        let mut vm = Vm::new_bc(&opcodes);

        vm.rt.preload_function_bytecode(asm! {
            PUSH ["Hello, "]
            LOAD_LOCAL [vec![0, 0, 0, 0]]
            CONCAT
            RETURN
        });

        // equal to
        /*
        module self

        const my_function = (arg1) => {
            debug("Hello, " + arg1")
        }

        my_function("World")
        my_function("Another Param")
        */

        vm.run().unwrap();
    }

    #[test]
    fn stdlib_io() {
        let opcodes = asm! {
            NOP

            PUSH ["Enter your name: "]
            PUSH ["println"]
            CALL_NATIVE

            PUSH ["Hello, "]
            PUSH ["readln"]
            CALL_NATIVE [vec![0x00]]
            CONCAT
            PUSH ["println"]
            CALL_NATIVE [vec![0x01]]

            HLT
        };

        let vm = Vm::new_bc(&opcodes);
        vm.run().unwrap()
    }

    #[test]
    fn stdlib_mem() {
        let opcodes = asm! {
            NOP

            PUSH ["Hello, World!"]
            PUSH ["ptr"]
            CALL_NATIVE [vec![0x00]]
            DEBUG

            HLT
        };

        let vm = Vm::new_bc(&opcodes);
        vm.run().unwrap();
    }
}
