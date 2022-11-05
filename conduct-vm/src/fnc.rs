use conduct_tk::AHashMap;

use crate::reg::{Registry, StackValue};

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq, Ord)]
pub struct FunctionDescriptor {
    pub params: Vec<String>,
    pub bytecode_chunk: Vec<u8>,
    pub module: String,
}

impl FunctionDescriptor {
    pub fn prepare_call_site(&self, reg: &mut Registry, args: AHashMap<String, StackValue>) {
        reg.scopes.push();
        for (name, value) in args {
            reg.scopes.define(name, true, value);
        }
    }
}
