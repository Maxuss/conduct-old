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
    0x0B RDLOAD // loads a rodata value
    0x0C CONCAT // concatenates two strings from heap and pushes new string to heap
    0xFF HDEBUG // prints heaps content from pointer
}

#[macro_export]
macro_rules! asm {
    (
        $($op:ident $([$value:expr])?)+
    ) => {{
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

impl IntoAsm for u8 {
    fn into_asm(self) -> Vec<u8> {
        vec![self]
    }
}

impl IntoAsm for i32 {
    fn into_asm(self) -> Vec<u8> {
        (self as f64).into_asm()
    }
}

impl IntoAsm for u64 {
    fn into_asm(self) -> Vec<u8> {
        self.to_be_bytes().to_vec()
    }
}

impl IntoAsm for usize {
    fn into_asm(self) -> Vec<u8> {
        (self as f64).into_asm()
    }
}

impl IntoAsm for f64 {
    fn into_asm(self) -> Vec<u8> {
        self.to_be_bytes().to_vec()
    }
}
