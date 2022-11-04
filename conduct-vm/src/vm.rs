use crate::reg::Registry;

pub struct Vm {
    pub rodata: Vec<u8>,
    pub heap: Vec<u8>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            rodata: vec![],
            heap: Vec::with_capacity(1024 * 8),
        }
    }

    pub fn store_str(&mut self, str: &str) -> usize {
        let pos = self.rodata.len();
        self.rodata.extend(str.bytes());
        self.rodata.push(0); // null escaping
        pos
    }

    pub fn store_string(&mut self, str: String) -> usize {
        self.store_str(&str)
    }

    pub fn run<'c>(mut self, bytecode: &'c [u8]) {
        let mut reg = Registry::new(bytecode);
        reg.run(&mut self);
    }
}
