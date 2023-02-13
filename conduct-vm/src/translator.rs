use std::{sync::Arc, vec};

use conduct_tk::{
    ast::{Assignment, Expression, ForLoopStatement, Literal, Visitor},
    AHashMap,
};

use crate::{
    mem::{FunctionData, StackValue},
    op::Opcode,
    vm::Vm,
};

type Res<T> = Result<T, String>;

#[derive(Debug, Clone)]
pub struct Ast2OpTranslator {
    pub non_jit_functions: Vec<FunctionData>,
    pub preinit_stack: Vec<StackValue>,
    pub opcodes: Vec<Opcode>,
}

impl Visitor for Ast2OpTranslator {
    fn consume(&mut self, stream: &Vec<conduct_tk::ast::Statement>) {
        for stmt in stream {
            self.visit_stmt(stmt);
        }
        println!("{self:#?}");
        let vm = Vm::from_translator(self.to_owned());
        vm.run_first();
    }

    fn visit_stmt(&mut self, tree: &conduct_tk::ast::Statement) {
        match tree {
            conduct_tk::ast::Statement::Import(_) => {
                // TODO: unimplemented
            }
            conduct_tk::ast::Statement::Export(_) => {
                // TODO: unimplemented
            }
            conduct_tk::ast::Statement::Module(_) => {
                // TODO: unimplemented
            }
            conduct_tk::ast::Statement::Function(name, params, body) => {
                let begin_index = self.opcodes.len();
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                let preserved = self.opcodes.drain(begin_index..).collect::<Vec<Opcode>>();
                let data = FunctionData {
                    parameters: params.iter().map(|each| each.0.to_owned()).collect(),
                    body: preserved,
                };
                self.opcodes
                    .push(Opcode::Lambda(self.non_jit_functions.len()));
                self.opcodes
                    .push(Opcode::DefConst(Arc::from(name.0.as_ref())));
                self.non_jit_functions.push(data);
            }
            conduct_tk::ast::Statement::Variable((name, _), (value, _)) => {
                self.visit_expr(value).unwrap();
                self.opcodes.push(Opcode::DefVar(Arc::from(name as &str)));
            }
            conduct_tk::ast::Statement::Constant((name, _), (value, _)) => {
                self.visit_expr(value).unwrap();
                self.opcodes.push(Opcode::DefConst(Arc::from(name as &str)));
            }
            conduct_tk::ast::Statement::NativeConstant(_) => todo!(),
            conduct_tk::ast::Statement::NativeFunction(_, _) => todo!(),
            conduct_tk::ast::Statement::AssignValue(lhs, op, rhs) => {
                // Generates the bytecode for the right-hand side expression
                self.visit_expr(&rhs.0).unwrap();
                // Generates the bytecode for the left-hand side expression
                self.visit_expr(&lhs.0).unwrap();
                // Generates the appropriate assignment opcode based on the operation
                match op.0 {
                    Assignment::Assign => self.opcodes.push(Opcode::Assign),
                    Assignment::AddAssign => self.opcodes.push(Opcode::AddAssign),
                    Assignment::SubtractAssign => self.opcodes.push(Opcode::SubAssign),
                    Assignment::MultiplyAssign => self.opcodes.push(Opcode::MulAssign),
                    Assignment::DivideAssign => self.opcodes.push(Opcode::DivAssign),
                    Assignment::ModuloAssign => self.opcodes.push(Opcode::ModAssign),
                }
            }
            conduct_tk::ast::Statement::Return(expr) => {
                self.visit_expr(&expr.0).unwrap();
                self.opcodes.push(Opcode::Return);
            }
            conduct_tk::ast::Statement::If(if_statement) => {
                self.visit_expr(&if_statement.condition.0).unwrap();
                let jump_index = self.opcodes.len();
                self.opcodes.push(Opcode::JumpIfNot(0));
                for stmt in &if_statement.body {
                    self.visit_stmt(stmt);
                }
                if !if_statement.else_ifs.is_empty() || if_statement.else_body.is_some() {
                    let jump_out_index = self.opcodes.len();
                    self.opcodes.push(Opcode::Jump(0));
                    self.opcodes[jump_index] = Opcode::JumpIfNot(self.opcodes.len());
                    for else_if in &if_statement.else_ifs {
                        self.visit_expr(&else_if.condition.0).unwrap();
                        let inner_jump_index = self.opcodes.len();
                        self.opcodes.push(Opcode::JumpIfNot(0));
                        for stmt in &else_if.body {
                            self.visit_stmt(stmt);
                        }
                        self.opcodes[inner_jump_index] = Opcode::JumpIfNot(self.opcodes.len())
                    }
                    if let Some(else_body) = &if_statement.else_body {
                        for stmt in else_body {
                            self.visit_stmt(stmt);
                        }
                    }
                    self.opcodes[jump_out_index] = Opcode::Jump(self.opcodes.len());
                } else {
                    self.opcodes[jump_index] = Opcode::JumpIfNot(self.opcodes.len());
                }
            }
            conduct_tk::ast::Statement::WhileLoop(condition, stmts) => {
                let start_index = self.opcodes.len();
                self.visit_expr(&condition.0).unwrap();
                let jump_index = self.opcodes.len();
                self.opcodes.push(Opcode::JumpIfNot(0));
                for stmt in stmts {
                    self.visit_stmt(stmt);
                }
                self.opcodes.push(Opcode::Jump(start_index));
                self.opcodes[jump_index] = Opcode::JumpIfNot(self.opcodes.len());
            }
            conduct_tk::ast::Statement::ForLoop(_) => todo!(),
            conduct_tk::ast::Statement::Throw(_) => todo!(),
            conduct_tk::ast::Statement::TryCatch(_) => todo!(),
            conduct_tk::ast::Statement::Break(_) => todo!(),
            conduct_tk::ast::Statement::Continue(_) => todo!(),
            conduct_tk::ast::Statement::Expression((expr, _)) => {
                self.visit_expr(expr).unwrap();
                self.opcodes.push(Opcode::Pop)
            }
        }
    }
}

impl Ast2OpTranslator {
    pub fn new() -> Ast2OpTranslator {
        Ast2OpTranslator {
            non_jit_functions: vec![],
            preinit_stack: vec![],
            opcodes: vec![],
        }
    }

    fn visit_expr(&mut self, expr: &Expression) -> Res<()> {
        match expr {
            Expression::Literal(lit) => {
                self.visit_literal(&lit.value)?;
                if let Some(uo) = lit.operator {
                    match uo {
                        conduct_tk::ast::UnaryOperator::Bang => self.opcodes.push(Opcode::Negate),
                        conduct_tk::ast::UnaryOperator::Minus => self.opcodes.push(Opcode::Negate),
                        conduct_tk::ast::UnaryOperator::Increment => self.opcodes.push(Opcode::Inc),
                        conduct_tk::ast::UnaryOperator::Decrement => self.opcodes.push(Opcode::Dec),
                    }
                }
            }
            Expression::Path(pth) => {
                self.visit_expr(&pth.base)?;
                for element in &pth.elements {
                    match element {
                        conduct_tk::ast::PathElement::AccessProperty(prop) => {
                            self.opcodes.push(Opcode::GetProp(Arc::from(prop.as_str())))
                        }
                        conduct_tk::ast::PathElement::Index((idx, _)) => {
                            self.visit_expr(idx)?;
                            self.opcodes.push(Opcode::Index);
                        }
                        conduct_tk::ast::PathElement::Invoke(args) => {
                            for (arg, _) in args {
                                self.visit_expr(arg)?;
                            }
                            self.opcodes.push(Opcode::Invoke(args.len()))
                        }
                        conduct_tk::ast::PathElement::NullAssert => {
                            self.opcodes.push(Opcode::NullAssert)
                        }
                    }
                }
            }
            Expression::BinaryOperation(bin) => {
                let mut vals = bin.values.clone();
                let first = vals.remove(0).0;
                self.visit_expr(&first)?;

                macro_rules! simple_op {
                    ($op:ident) => {
                        self.opcodes.push(Opcode::$op)
                    };
                }

                for (op, _) in &bin.operators {
                    let next = vals.remove(0).0;
                    self.visit_expr(&next)?;
                    match op {
                        conduct_tk::ast::BinaryOperator::Add => simple_op!(Add),
                        conduct_tk::ast::BinaryOperator::Subtract => simple_op!(Sub),
                        conduct_tk::ast::BinaryOperator::Range => todo!(),
                        conduct_tk::ast::BinaryOperator::In => todo!(),
                        conduct_tk::ast::BinaryOperator::Multiply => simple_op!(Mul),
                        conduct_tk::ast::BinaryOperator::Divide => simple_op!(Div),
                        conduct_tk::ast::BinaryOperator::Modulo => simple_op!(Mod),
                        conduct_tk::ast::BinaryOperator::Power => simple_op!(Pow),
                        conduct_tk::ast::BinaryOperator::Or => todo!(),
                        conduct_tk::ast::BinaryOperator::And => todo!(),
                        conduct_tk::ast::BinaryOperator::Equals => todo!(),
                        conduct_tk::ast::BinaryOperator::LessThan => todo!(),
                        conduct_tk::ast::BinaryOperator::Is => todo!(),
                        conduct_tk::ast::BinaryOperator::GreaterThan => todo!(),
                        conduct_tk::ast::BinaryOperator::LessThanOrEqual => todo!(),
                        conduct_tk::ast::BinaryOperator::GreaterThanOrEqual => todo!(),
                        conduct_tk::ast::BinaryOperator::NotEquals => todo!(),
                        conduct_tk::ast::BinaryOperator::BitwiseOr => todo!(),
                        conduct_tk::ast::BinaryOperator::BitwiseAnd => todo!(),
                        conduct_tk::ast::BinaryOperator::BitwiseXor => todo!(),
                        conduct_tk::ast::BinaryOperator::ShiftLeft => todo!(),
                        conduct_tk::ast::BinaryOperator::ShiftRight => todo!(),
                    }
                }
            }
            Expression::Function(params, body) => {
                let params: Vec<String> = params.iter().map(|each| each.0.to_owned()).collect();
                let begin_index = self.opcodes.len();
                for stmt in body {
                    self.visit_stmt(stmt);
                }
                let preserved_opcodes = self.opcodes.drain(begin_index..).collect::<Vec<Opcode>>();
                let data = FunctionData {
                    parameters: params,
                    body: preserved_opcodes,
                };
                let idx = self.non_jit_functions.len();
                self.non_jit_functions.push(data);
                self.opcodes.push(Opcode::Lambda(idx));
            }
            Expression::Ternary(_) => {
                // ...
            }
        }
        Ok(())
    }

    fn visit_literal(&mut self, literal: &Literal) -> Res<()> {
        match literal {
            Literal::Nil => self.opcodes.push(Opcode::Nil),
            Literal::String(s) => {
                self.opcodes
                    .push(Opcode::StringConst(Arc::from(s.as_str())));
            }
            Literal::Number(n) => self.opcodes.push(Opcode::FloatConst(*n)),
            Literal::Boolean(b) => self.opcodes.push(Opcode::BoolConst(*b)),
            Literal::Reference(r) => self.opcodes.push(Opcode::LoadVar(Arc::from(r as &str))),
            Literal::Array(arr) => {
                self.opcodes.push(Opcode::InitArray);
                for (expr, _) in arr {
                    self.visit_expr(expr)?;
                    self.opcodes.push(Opcode::ArrayPush)
                }
            }
            Literal::Compound(map) => {
                self.opcodes.push(Opcode::InitArray);
                for (name, (expr, _)) in map {
                    self.visit_expr(expr)?;
                    self.opcodes.push(Opcode::ObjPush(Arc::from(name.as_str())))
                }
            }
            Literal::TypeDefinition(_) => todo!(),
        }
        Ok(())
    }
}

impl Default for Ast2OpTranslator {
    fn default() -> Self {
        Self::new()
    }
}
