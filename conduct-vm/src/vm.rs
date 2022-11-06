use std::collections::hash_map::Entry;

use conduct_tk::AHashMap;
use internment::Intern;

use crate::{
    ffi::NativeFunctionDescriptor,
    fnc::FunctionDescriptor,
    reg::{HeapValue, Registry, StackValue},
    stdlib,
};

pub type HeapPtr = usize;

#[derive(Clone)]
pub struct Vm {
    pub heap: Vec<u8>,
    pub registry: Registry,
    native_values: AHashMap<String, fn(&mut Vm) -> Variable>,
    global_variables: AHashMap<String, Variable>,
    native_functions: AHashMap<String, NativeFunctionDescriptor>,
    functions: AHashMap<String, FunctionDescriptor>,
    imports: Vec<String>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            heap: Vec::with_capacity(1024 * 8),
            registry: Registry::new(vec![0]),
            native_values: AHashMap::new(),
            global_variables: AHashMap::new(),
            native_functions: AHashMap::new(),
            imports: vec!["self".to_owned()],
            functions: AHashMap::new(),
        }
    }

    pub fn alloc(&mut self, value: HeapValue) -> HeapPtr {
        let ptr = self.heap.len();
        value.store_to(&mut self.heap);
        ptr
    }

    pub fn alloc_str<S: Into<String>>(&mut self, value: S) -> StackValue {
        let ptr = self.alloc(HeapValue::String(Intern::new(value.into())));
        StackValue::HeapPointer(ptr)
    }

    pub fn read_value(&mut self, ptr: HeapPtr) -> Option<HeapValue> {
        let begin = ptr;
        let mut size_bytes: [u8; 8] = Default::default();
        let mem_slice = &self.heap[begin..begin + 8];
        size_bytes.copy_from_slice(mem_slice);
        let size = u64::from_be_bytes(size_bytes) as usize;
        let slice = &self.heap[begin + 8..begin + 8 + size];

        HeapValue::read_from(slice)
    }

    pub fn add_native_value<S: Into<String>>(
        &mut self,
        named: S,
        provider: fn(&mut Vm) -> Variable,
    ) {
        self.native_values.insert(named.into(), provider);
    }

    pub fn add_global_variable<S: Into<String>>(
        &mut self,
        named: S,
        mutable: bool,
        value: StackValue,
    ) {
        self.global_variables
            .insert(named.into(), Variable { mutable, value });
    }

    pub fn set_global_variable<S: Into<String>>(&mut self, named: S, value: StackValue) {
        let name = named.into();
        if self.global_variables.contains_key(&name) && self.global_variables[&name].mutable {
            self.global_variables.insert(
                name,
                Variable {
                    mutable: true,
                    value,
                },
            );
        }
    }

    pub fn get_global_variable<S: Into<String>>(&mut self, named: S) -> Option<StackValue> {
        match self.global_variables.get(&named.into()) {
            Some(v) => Some(v.value),
            None => None,
        }
    }

    pub fn get_native_variable<S: Into<String>>(&mut self, named: S) -> Option<StackValue> {
        match self.native_values.entry(named.into()) {
            Entry::Occupied(occupied) => Some(occupied.get()(self).value),
            Entry::Vacant(_) => None,
        }
    }

    pub fn add_native_function<M: Into<String>, N: Into<String>>(
        &mut self,
        module: M,
        name: N,
        arity: Vec<String>,
        function: fn(&mut Vm, AHashMap<String, StackValue>) -> StackValue,
    ) {
        self.native_functions.insert(
            name.into(),
            NativeFunctionDescriptor {
                callable: function,
                arity,
                module: module.into(),
            },
        );
    }

    pub fn call_native_function<N: Into<String>>(
        &mut self,
        name: N,
        args: Vec<StackValue>,
    ) -> Option<StackValue> {
        let fn_name = name.into();
        let function = self
            .native_functions
            .iter()
            .filter(|(_, desc)| self.imports.iter().any(|import| &desc.module == import))
            .find(|(name, desc)| {
                if !self.imports.contains(&desc.module.to_owned()) {
                    false
                } else {
                    (*name).eq(&fn_name)
                }
            });
        match function {
            Some((_, f)) => {
                if f.arity.len() != args.len() {
                    None
                } else {
                    let callable = f.callable;
                    let mut actual_args = AHashMap::new();
                    let mut index = 0;
                    for each in &f.arity {
                        actual_args.insert(each.to_owned(), args[index]);
                        index += 1;
                    }
                    Some(callable(self, actual_args))
                }
            }
            None => None,
        }
    }

    pub fn add_function<N: Into<String>>(&mut self, name: N, desc: FunctionDescriptor) {
        self.functions.insert(name.into(), desc);
    }

    pub fn get_function<N: Into<String>>(
        &mut self,
        name: N,
        args: Vec<StackValue>,
    ) -> Option<(FunctionDescriptor, AHashMap<String, StackValue>)> {
        let fn_name = name.into();
        let function = self
            .functions
            .iter()
            .filter(|(_, desc)| self.imports.iter().any(|import| &desc.module == import))
            .find(|(name, desc)| {
                if !self.imports.contains(&desc.module.to_owned()) {
                    false
                } else {
                    (*name).eq(&fn_name)
                }
            });
        match function {
            Some((_, f)) => {
                if f.params.len() != args.len() {
                    None
                } else {
                    let mut actual_args = AHashMap::new();
                    let mut index = 0;
                    for each in &f.params {
                        actual_args.insert(each.to_owned(), args[index]);
                        index += 1;
                    }
                    Some((f.to_owned(), actual_args))
                }
            }
            None => None,
        }
    }

    pub fn import<I: Into<String>>(&mut self, path: I) {
        self.imports.push(path.into())
    }

    pub fn get_native_argc(&self, fnc: String) -> Vec<String> {
        let func = self
            .native_functions
            .iter()
            .filter(|(_, desc)| self.imports.iter().any(|import| &desc.module == import))
            .find(|(name, desc)| {
                if !self.imports.contains(&desc.module.to_owned()) {
                    false
                } else {
                    (*name).eq(&fnc)
                }
            });

        match func {
            Some((_, desc)) => desc.arity.clone(),
            _ => vec![],
        }
    }

    fn prepare(&mut self) {
        // heap padding
        let implementation = self.alloc_str(format!(
            "name:Conduct-VM/lang:rust/version:{}",
            env!("CARGO_PKG_VERSION")
        ));
        let stdlib_features = self.alloc_str("__full__");
        let supported_version = self.alloc_str("__latest__");
        let endianness = self.alloc_str(if cfg!(target_endian = "little") {
            "little"
        } else {
            "big"
        });
        let package_dir = self.alloc_str(".crane");
        let bytecode_version = self.alloc_str(format!("{}-bc", env!("CARGO_PKG_VERSION")));

        // conduct important variables
        self.add_global_variable("__CONDUCT_IMPL", false, implementation);
        self.add_global_variable("__CONDUCT_STDLIB_FEATURES", false, stdlib_features);
        self.add_global_variable("__CONDUCT_SUPPORTED_VERSION", false, supported_version);
        self.add_global_variable("__CONDUCT_IS_ASYNC", false, StackValue::Boolean(false));
        self.add_global_variable("__CONDUCT_ASYNC_RT", false, StackValue::Nil);
        self.add_global_variable("__CONDUCT_ENDIANNESS", false, endianness);
        self.add_global_variable(
            "__CONDUCT_CRANE_SUPPORTED",
            false,
            StackValue::Boolean(true),
        );
        self.add_global_variable("__CONDUCT_CRANE_DIR", false, package_dir);

        // vm variables
        self.add_global_variable("__VM_BYTECODE_VERSION", false, bytecode_version);
        self.add_global_variable("__VM_STACK_SIZE", false, StackValue::Number(256.));
        self.add_global_variable("__VM_HEAP_PREALLOC", false, StackValue::Number(1024. * 8.));

        // native functions
        self.add_native_function(
            "std.io",
            "println",
            vec!["string".to_owned()],
            stdlib::io::stdio_println,
        );
    }

    pub fn run(mut self, bytecode: &[u8]) -> Option<()> {
        self.prepare();
        let stack = self.registry.stack.clone();
        let scopes = self.registry.scopes.clone();
        let module = self.registry.module.clone();
        let mut c = Registry {
            bytecode: bytecode.to_owned(),
            ip: 0,
            size: bytecode.len(),
            stack,
            scopes,
            module,
        };
        c.run(&mut self)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Variable {
    pub mutable: bool,
    pub value: StackValue,
}
