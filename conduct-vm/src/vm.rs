use std::{
    iter,
    sync::{Arc, Mutex},
};

use conduct_tk::AHashMap;

use crate::{
    mem::{FunctionData, NativeFunctionData, StackValue},
    op::Opcode,
    translator::Ast2OpTranslator,
};

#[derive(Debug)]
pub struct Vm {
    native_consts: AHashMap<String, StackValue>,
    native_fns: AHashMap<String, NativeFunctionData>,
    stack: Vec<StackValue>,
    variables: AHashMap<String, StackValue>,
    fns: Vec<FunctionData>,
}

impl Vm {
    pub fn new() -> Self {
        Self {
            native_consts: AHashMap::new(),
            native_fns: AHashMap::new(),
            stack: Vec::with_capacity(256),
            fns: Vec::with_capacity(32),
            variables: AHashMap::with_capacity(64),
        }
    }

    pub fn from_translator(t: Ast2OpTranslator) -> Self {
        Self {
            native_consts: AHashMap::new(),
            native_fns: AHashMap::new(),
            stack: t.preinit_stack,
            variables: AHashMap::with_capacity(64),
            fns: t.non_jit_functions,
        }
    }

    pub fn add_native_fn<S: Into<String>>(&mut self, name: S, desc: NativeFunctionData) {
        self.native_fns.insert(name.into(), desc);
    }

    pub fn run_first(mut self) {
        self.add_native_fn(
            "debug",
            NativeFunctionData {
                parameters: vec!["val".to_owned()],
                executor: |vals| {
                    println!("{:#?}", vals.get("val").unwrap());
                    StackValue::Nil
                },
            },
        );
        let first = &self.fns[0].clone();
        self.visit_function(first);
    }

    pub fn visit_function(&mut self, slice: &FunctionData) {
        let mut pc = 0;
        let len = slice.body.len();
        while pc < len {
            let op = &slice.body[pc];
            macro_rules! pop {
                () => {{
                    match self.stack.pop() {
                        Some(v) => v,
                        None => panic!("Stack underflow"),
                    }
                }};
            }
            match op {
                Opcode::FloatConst(f) => self.stack.push(StackValue::Float(*f)),
                Opcode::StringConst(str) => self.stack.push(StackValue::String(str.clone())),
                Opcode::BoolConst(bool) => self.stack.push(StackValue::Bool(*bool)),
                Opcode::Lambda(lambda) => self.stack.push(StackValue::Lambda(*lambda)),
                Opcode::Add => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    println!("LHS: {lhs:#?} RHS: {rhs:#?}");
                    let value = match (lhs, rhs) {
                        (StackValue::String(any), other) => StackValue::String(Arc::from(
                            any.as_ref().to_owned() + other.as_str().as_ref(),
                        )),
                        (other, StackValue::String(any)) => StackValue::String(Arc::from(
                            other.as_str().as_ref().to_owned() + any.as_ref(),
                        )),
                        (StackValue::Float(a), StackValue::Float(b)) => StackValue::Float(a + b),
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Sub => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    let value = match (lhs, rhs) {
                        (StackValue::Float(a), StackValue::Float(b)) => StackValue::Float(a - b),
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Div => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    let value = match (lhs, rhs) {
                        (StackValue::Float(a), StackValue::Float(b)) => {
                            if b == 0. {
                                panic!("Null Division")
                            } else {
                                StackValue::Float(a / b)
                            }
                        }
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Mul => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    let value = match (lhs, rhs) {
                        (StackValue::Float(a), StackValue::Float(b)) => StackValue::Float(a * b),
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Mod => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    let value = match (lhs, rhs) {
                        (StackValue::Float(a), StackValue::Float(b)) => StackValue::Float(a % b),
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Invoke(amount) => {
                    let mut aggregated = iter::repeat_with(|| {
                        match self.stack.pop() {
                            Some(v) => v,
                            None => panic!("Stack underflow"),
                        }
                        .as_value()
                    })
                    .take(*amount)
                    .collect::<Vec<StackValue>>()
                    .into_iter()
                    .rev();

                    println!("{aggregated:#?}");

                    let invoked = pop!().as_value();
                    match invoked {
                        StackValue::NativeFnPtr(native) => {
                            let native_desc = self.native_fns.get(&native).unwrap();
                            let mut args = AHashMap::<String, StackValue>::new();
                            for param in &native_desc.parameters {
                                let value = aggregated.next().unwrap();
                                args.insert(param.to_owned(), value);
                            }
                            let exec = native_desc.executor;
                            self.stack.push(exec(args));
                        }
                        StackValue::Lambda(lambdaptr) => {
                            let desc = self.fns.get(lambdaptr).unwrap().clone();
                            let mut args = AHashMap::<String, StackValue>::new();
                            for param in &desc.parameters {
                                let value = aggregated.next().unwrap();
                                args.insert(param.to_owned(), value);
                            }
                            self.visit_function(&desc);
                        }
                        _ => panic!("Invalid invocation!"),
                    }
                }
                Opcode::Nil => self.stack.push(StackValue::Nil),
                Opcode::DefVar(name) => {
                    let value = pop!().as_value();
                    let value = StackValue::Variable {
                        name: name.to_owned(),
                        mutable: true,
                        value: Arc::new(Mutex::new(value)),
                    };
                    self.variables
                        .insert(name.as_ref().to_owned(), value.clone());
                    self.stack.push(value);
                }
                Opcode::DefConst(name) => {
                    let value = pop!().as_value();
                    let value = StackValue::Variable {
                        name: name.to_owned(),
                        mutable: false,
                        value: Arc::new(Mutex::new(value)),
                    };
                    self.variables
                        .insert(name.as_ref().to_owned(), value.clone());
                    self.stack.push(value);
                }
                Opcode::LoadVar(property) => {
                    if self.variables.contains_key(property.as_ref()) {
                        let var = self.variables.get(property.as_ref()).unwrap().clone();
                        self.stack.push(var);
                    } else if self.native_fns.contains_key(property.as_ref()) {
                        self.stack
                            .push(StackValue::NativeFnPtr(property.as_ref().to_owned()))
                    } else if self.native_consts.contains_key(property.as_ref()) {
                        self.stack
                            .push(StackValue::NativeConstPtr(property.as_ref().to_owned()))
                    } else {
                        panic!("Unresolved variable")
                    }
                }
                Opcode::Debug => {
                    let v = pop!();
                    println!("{v:#?}");
                    self.stack.push(v);
                }
                Opcode::Halt => {
                    return;
                }
                Opcode::Return => {
                    return;
                }
                Opcode::InitObj => self.stack.push(StackValue::Object(AHashMap::new())),
                Opcode::InitArray => self.stack.push(StackValue::Array(Vec::new())),
                Opcode::ObjPush(name) => {
                    let mut pop = pop!().as_value();
                    if let StackValue::Object(obj) = &mut pop {
                        let value = pop!().as_value();
                        obj.insert(name.as_ref().to_owned(), value);
                        self.stack.push(pop)
                    } else {
                        panic!("Malformed Bytecode")
                    }
                }
                Opcode::ArrayPush => {
                    let mut pop = pop!().as_value();
                    if let StackValue::Array(obj) = &mut pop {
                        let value = pop!().as_value();
                        obj.push(value);
                        self.stack.push(pop)
                    } else {
                        panic!("Malformed Bytecode")
                    }
                }
                Opcode::Negate => {
                    let v = pop!().as_value();
                    let v = match v {
                        StackValue::Float(f) => StackValue::Float(-f),
                        StackValue::Bool(b) => StackValue::Bool(!b),
                        other => other,
                    };
                    self.stack.push(v);
                }
                Opcode::Pow => {
                    let rhs = pop!().as_value();
                    let lhs = pop!().as_value();
                    let value = match (lhs, rhs) {
                        (StackValue::Float(a), StackValue::Float(b)) => {
                            if b == 0. {
                                StackValue::Float(1.)
                            } else if b == 1. {
                                StackValue::Float(a)
                            } else {
                                StackValue::Float(a.powf(b))
                            }
                        }
                        (StackValue::Nil, _) => StackValue::Nil,
                        (_, StackValue::Nil) => StackValue::Nil,
                        _ => panic!("Unsupported operation!"),
                    };
                    self.stack.push(value);
                }
                Opcode::Dec => {
                    let value = self.stack.last().unwrap().clone();
                    match value {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant mutation")
                            }
                            let mut mutex = value.as_ref().lock().unwrap();
                            let value = if let StackValue::Float(f) = *mutex {
                                f - 1.
                            } else {
                                0.
                            };
                            *mutex = StackValue::Float(value);
                        }
                        StackValue::Float(v) => {
                            self.stack.pop();
                            self.stack.push(StackValue::Float(v - 1.))
                        }
                        _ => panic!("Malformed Bytecode"),
                    }
                }
                Opcode::Inc => {
                    let value = self.stack.last().unwrap().clone();
                    match value {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant mutation")
                            }
                            let mut mutex = value.as_ref().lock().unwrap();
                            let value = if let StackValue::Float(f) = *mutex {
                                f + 1.
                            } else {
                                0.
                            };
                            *mutex = StackValue::Float(value);
                        }
                        StackValue::Float(v) => {
                            self.stack.pop();
                            self.stack.push(StackValue::Float(v + 1.))
                        }
                        _ => panic!("Malformed Bytecode"),
                    }
                }
                Opcode::GetProp(property) => {
                    let value = pop!().as_value();
                    if let StackValue::Object(obj) = value {
                        let value = obj.get(property.as_ref()).unwrap_or(&StackValue::Nil);
                        self.stack.push(value.to_owned())
                    } else {
                        panic!("Invalid access")
                    }
                }
                Opcode::Index => {
                    let val = pop!().as_value();
                    let indexed = pop!().as_value();
                    if let StackValue::Array(arr) = indexed {
                        if let StackValue::Float(idx) = val {
                            let v = arr.get(idx as usize).unwrap().clone();
                            self.stack.push(v);
                        }
                    } else {
                        panic!("Malformed Bytecode")
                    }
                }
                Opcode::NullAssert => {
                    let v = self.stack.last().unwrap().clone();
                    match v {
                        StackValue::Nil => panic!("Null Assertion Failed"),
                        _ => {}
                    }
                }
                Opcode::Function(_, _) => todo!(),
                Opcode::Jump(to) => pc += *to,
                Opcode::JumpIf(to) => {
                    let condition = pop!().as_bool();
                    if condition {
                        pc += *to;
                    }
                }
                Opcode::JumpIfNot(to) => {
                    let condition = pop!().as_bool();
                    if !condition {
                        pc += *to;
                    }
                }
                Opcode::Pop => {
                    self.stack.pop();
                }
                Opcode::Assign => {
                    let rhs = pop!().as_value();
                    let lhs = pop!();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            *value.lock().unwrap() = rhs;
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
                Opcode::AddAssign => {
                    let lhs = pop!();
                    let rhs = pop!().as_value();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            *value.lock().unwrap() = rhs;
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
                Opcode::SubAssign => {
                    let lhs = pop!();
                    let rhs = pop!().as_value();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            let mut value = value.lock().unwrap();
                            let res = match (value.clone(), rhs) {
                                (StackValue::String(any), other) => StackValue::String(Arc::from(
                                    any.as_ref().to_owned() + other.as_str().as_ref(),
                                )),
                                (other, StackValue::String(any)) => StackValue::String(Arc::from(
                                    other.as_str().as_ref().to_owned() + any.as_ref(),
                                )),
                                (StackValue::Float(a), StackValue::Float(b)) => {
                                    StackValue::Float(a + b)
                                }
                                (StackValue::Nil, _) => StackValue::Nil,
                                (_, StackValue::Nil) => StackValue::Nil,
                                _ => panic!("Unsupported operation!"),
                            };
                            *value = res;
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
                Opcode::DivAssign => {
                    let lhs = pop!();
                    let rhs = pop!().as_value();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            let mut value = value.lock().unwrap();
                            let res = match (value.clone(), rhs) {
                                (StackValue::Float(a), StackValue::Float(b)) => {
                                    if b == 0. {
                                        panic!("Null Division")
                                    } else {
                                        StackValue::Float(a / b)
                                    }
                                }
                                (StackValue::Nil, _) => StackValue::Nil,
                                (_, StackValue::Nil) => StackValue::Nil,
                                _ => panic!("Unsupported operation!"),
                            };
                            *value = res
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
                Opcode::MulAssign => {
                    let lhs = pop!();
                    let rhs = pop!().as_value();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            let mut value = value.lock().unwrap();
                            let res = match (value.clone(), rhs) {
                                (StackValue::Float(a), StackValue::Float(b)) => {
                                    StackValue::Float(a * b)
                                }
                                (StackValue::Nil, _) => StackValue::Nil,
                                (_, StackValue::Nil) => StackValue::Nil,
                                _ => panic!("Unsupported operation!"),
                            };
                            *value = res
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
                Opcode::ModAssign => {
                    let lhs = pop!();
                    let rhs = pop!().as_value();
                    match lhs {
                        StackValue::Variable {
                            name: _,
                            mutable,
                            value,
                        } => {
                            if !mutable {
                                panic!("Constant Mutation")
                            }
                            let mut value = value.lock().unwrap();
                            let res = match (value.clone(), rhs) {
                                (StackValue::Float(a), StackValue::Float(b)) => {
                                    StackValue::Float(a % b)
                                }
                                (StackValue::Nil, _) => StackValue::Nil,
                                (_, StackValue::Nil) => StackValue::Nil,
                                _ => panic!("Unsupported operation!"),
                            };
                            *value = res
                        }
                        _ => panic!("Invalid Assignment"),
                    }
                }
            }
            pc += 1;
        }
    }
}

impl Default for Vm {
    fn default() -> Self {
        Self::new()
    }
}
