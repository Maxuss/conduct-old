macro_rules! opcodes {
    (
        $(
            $bc:literal $name:ident
        )*
    ) => {
        #[allow(non_camel_case_types)]
        #[derive(Debug, Copy, Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
        #[repr(u8)]
        pub enum Opcode {
            $(
                $name = $bc
            ),*
        }

        impl Opcode {
            pub fn bytecode(&self) -> u8 {
                match self {
                    $(
                        Self::$name => $bc
                    ),*
                }
            }
        }

        impl From<u8> for Opcode {
            fn from(v: u8) -> Self {
                match v {
                    $(
                        $bc => Self::$name,
                    )*
                    _ => Self::HLT
                }
            }
        }

        impl Into<u8> for Opcode {
            fn into(self) -> u8 {
                self as u8
            }
        }
    };
}

opcodes! {
    0x00 NOP // noop
    0x01 HLT // halts the execution
    0x02 PUSH // pushes value
    0x03 POP // pops value
    0x04 ADD // adds values
    0x05 SUB // subtracts values
    0x06 MUL // multiplies values
    0x07 DIV // divides values
    0x08 NEG // negates values
    0x09 INC // increments a value
    0x0A DEC // decrements a value
    0x0B CONCAT // concatenates two strings from heap and pushes new string to heap
    0x0C EQ // equal
    0x0D NEQ // not equal
    0x0E LT // less than
    0x0F GT // greater than
    0x10 LTQ // less than or equal
    0x11 GTQ // greater than or equal
    0x12 DEF_GLOBAL_CONST // defines a global constant
    0x13 DEF_GLOBAL_MUT // defines a global mutable variable
    0x14 SET_GLOBAL // sets a global variable
    0x15 LOAD_GLOBAL // loads a global variable
    0xFE DEBUG // debug prints current stack value
    0xFF HDEBUG // prints heaps content from pointer
}

#[macro_export]
macro_rules! asm {
    (
        $($op:ident $([$value:expr])?)+
    ) => {{
        #[allow(unused_imports)]
        use $crate::op::IntoAsm;
        let mut buf: Vec<u8> = Vec::new();
        $(
            buf.push(Opcode::$op.into());
            $(
                buf.extend_from_slice(&$value.into_asm());
            )?
        )*
        buf
    }};
}
pub trait IntoAsm {
    fn into_asm(self) -> Vec<u8>;
}

impl IntoAsm for Vec<u8> {
    fn into_asm(self) -> Vec<u8> {
        self
    }
}

impl IntoAsm for u8 {
    fn into_asm(self) -> Vec<u8> {
        vec![0x00, self]
    }
}

impl IntoAsm for i32 {
    fn into_asm(self) -> Vec<u8> {
        (self as f64).into_asm()
    }
}

impl IntoAsm for u64 {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x00];
        out.extend(self.to_be_bytes());
        out
    }
}

impl IntoAsm for usize {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x02];
        out.extend(self.to_be_bytes());
        out
    }
}

impl IntoAsm for f64 {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x00];
        out.extend(self.to_be_bytes());
        out
    }
}

impl<'s> IntoAsm for &'s str {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x01];
        out.extend(self.as_bytes());
        out.push(0);
        out
    }
}

impl IntoAsm for String {
    fn into_asm(self) -> Vec<u8> {
        let mut out = vec![0x01];
        out.extend(self.into_bytes());
        out.push(0);
        out
    }
}
