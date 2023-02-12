use std::path::{Path, PathBuf};

use ahash::AHashMap;
use ariadne::{Fmt, ReportKind};

use crate::{
    ast::{
        Assignment, Expression, Literal, PathElement, Span, Statement, UnaryOperator, ValueBody,
        Visitor,
    },
    err::{error, CodeArea, CodeSource, ConductCache, ErrorReport, FancyColorGenerator},
    parser::Parser,
};

type Valid = Result<(), ValidationError>;

#[derive(Debug, Clone, Default)]
struct Variable {
    mutable: bool,
    defined_at: Span,
}

#[derive(Debug, Clone, Default)]
struct CodeScope {
    variables: AHashMap<String, Variable>,
}

#[derive(Debug, Clone)]
pub enum ValidationError {
    UndefinedReference {
        to: String,
        at: CodeArea,
    },
    InvalidUnaryOperator {
        operator: UnaryOperator,
        attempted: String,
        at: CodeArea,
    },
    UnresolvedImport {
        import: String,
        searched: Vec<PathBuf>,
        at: CodeArea,
    },
    InvalidAssignmentAssignee {
        assignee: Expression,
        at: CodeArea,
    },
    InvalidAssignmentOperator {
        operator: Assignment,
        at: CodeArea,
    },
    ConstantAssignment {
        name: String,
        at: CodeArea,
        constant_defined_at: CodeArea,
    },
    Unsupported {
        desc: String,
        at: CodeArea,
    },
    ConstantCondition {
        desc: String,
        at: CodeArea,
    },
    WarnEmptyType {
        at: CodeArea,
    },
    WarnConstantCondition {
        desc: String,
        tip: String,
        at: CodeArea,
    },
}

impl ValidationError {
    pub fn report(self) -> ErrorReport {
        match self {
            ValidationError::UndefinedReference { to, at } => error(
                "E00",
                at.clone(),
                "Use of undefined variable",
                &[(at, &format!("Variable `{to}` is undefined"))],
            ),
            ValidationError::WarnEmptyType { at } => {
                let err = error(
                    "W00",
                    at.clone(),
                    "Empty type definition",
                    &[(at, "This type has zero fields")],
                );
                err.builder(ReportKind::Warning)
                    .with_note("This type can be replaced with `core.Empty` for type checking.")
                    .finish()
                    .print(ConductCache::default())
                    .unwrap();
                err
            }
            ValidationError::InvalidUnaryOperator {
                operator,
                attempted,
                at,
            } => {
                let op_desc = match operator {
                    UnaryOperator::Bang => "!",
                    UnaryOperator::Minus => "-",
                    UnaryOperator::Increment => "++",
                    UnaryOperator::Decrement => "--",
                };
                error(
                    "E01",
                    at.clone(),
                    &format!("The {operator:?} (`{op_desc}`) operator may not be applied to the value of type {attempted}"),
                    &[(at, "Invalid unary operator here")],
                )
            }
            ValidationError::UnresolvedImport {
                import,
                searched,
                at,
            } => {
                let searched_str = searched
                    .iter()
                    .map(|each| format!("- '{each:?}'"))
                    .collect::<Vec<String>>()
                    .join(";\n");
                error(
                    "E02",
                    at.clone(),
                    &format!("Failed to resolve {import} import. Searched places:\n{searched_str}"),
                    &[(at, "Failed to resolve this import")],
                )
            }
            ValidationError::InvalidAssignmentAssignee { assignee, at } => error(
                "E03",
                at.clone(),
                &format!("Invalid assignee, can not assign a value to {assignee}"),
                &[(at, "This expression may not be assigned a value")],
            ),
            ValidationError::InvalidAssignmentOperator { operator, at } => error(
                "E04",
                at.clone(),
                &format!("Invalid assignment operator, The {operator:?} is unapplicable here."),
                &[(at, "The operator here")],
            ),
            ValidationError::ConstantAssignment {
                name,
                at,
                constant_defined_at,
            } => error(
                "E05",
                at.clone(),
                &format!("Can not reassign value of the `{name}` constant."),
                &[
                    (at, "Tried to reassign value here"),
                    (
                        constant_defined_at,
                        &format!("Constant `{name}` defined here"),
                    ),
                ],
            ),
            ValidationError::Unsupported { desc, at } => {
                error("E06", at.clone(), &desc, &[(at, "Invalid operation here")])
            }
            ValidationError::ConstantCondition { desc, at } => {
                error("E07", at.clone(), &desc, &[(at, "Constant condition here")])
            }
            ValidationError::WarnConstantCondition { desc, tip, at } => {
                let err = error("W01", at.clone(), &desc, &[(at, "Constant condition here")]);
                err.builder(ReportKind::Warning)
                    .with_note(tip)
                    .finish()
                    .print(ConductCache::default())
                    .unwrap();
                err
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Validator {
    module_name: String,
    project_root: PathBuf,
    src: CodeSource,
    scope: CodeScope,
    err_count: usize,
    warn_count: usize,
    colors: FancyColorGenerator,
}

impl Validator {
    pub fn from(parser: &Parser) -> Self {
        Self {
            module_name: "global".to_owned(),
            src: parser.source.clone(),
            scope: CodeScope::default(),
            err_count: 0,
            warn_count: 0,
            colors: FancyColorGenerator::default(),
            project_root: Path::new(".").canonicalize().unwrap(),
        }
    }

    pub fn new(origin: CodeSource) -> Self {
        Self {
            module_name: "global".to_owned(),
            src: origin,
            scope: CodeScope::default(),
            err_count: 0,
            warn_count: 0,
            colors: FancyColorGenerator::default(),
            project_root: Path::new(".").canonicalize().unwrap(),
        }
    }

    #[inline(always)]
    fn validate(&mut self, valid: Valid) {
        if let Err(err) = valid {
            err.report()
                .report()
                .print(ConductCache::default())
                .unwrap();
            self.err_count += 1;
        }
    }

    fn visit_expr(&mut self, expr: &Expression, span: Span) -> Valid {
        macro_rules! validate {
            ($expr:expr) => {{
                let res = $expr;
                self.validate(res);
            }};
        }
        match expr {
            Expression::Literal(literal) => {
                validate!(match (literal.operator, &literal.value) {
                    (Some(any), Literal::String(_)) => Err(ValidationError::InvalidUnaryOperator {
                        operator: any,
                        attempted: format!("{}", "string".fg(self.colors.next_color())),
                        at: CodeArea {
                            src: self.src.clone(),
                            span,
                        },
                    }),
                    (Some(any), Literal::Compound(_)) => {
                        Err(ValidationError::InvalidUnaryOperator {
                            operator: any,
                            attempted: format!("{}", "compound".fg(self.colors.next_color())),
                            at: CodeArea {
                                src: self.src.clone(),
                                span,
                            },
                        })
                    }
                    (Some(any), Literal::Array(_)) => Err(ValidationError::InvalidUnaryOperator {
                        operator: any,
                        attempted: format!("{}", "array".fg(self.colors.next_color())),
                        at: CodeArea {
                            src: self.src.clone(),
                            span,
                        },
                    }),
                    (Some(any), Literal::TypeDefinition(_)) => {
                        Err(ValidationError::InvalidUnaryOperator {
                            operator: any,
                            attempted: format!(
                                "{}",
                                "type definition".fg(self.colors.next_color())
                            ),
                            at: CodeArea {
                                src: self.src.clone(),
                                span,
                            },
                        })
                    }
                    _ => Ok(()),
                });
                validate!(self.visit_literal(&literal.value, span));
                Ok(())
            }
            Expression::BinaryOperation(bin) => {
                for (value, pos) in &bin.values {
                    validate!(self.visit_expr(value, *pos));
                }
                Ok(())
            }
            Expression::Function(_, body) => {
                for part in body {
                    self.visit_stmt(part);
                }
                Ok(())
            }
            Expression::Ternary(tern) => {
                validate!(self.visit_expr(&tern.condition.0, tern.condition.1));
                validate!(self.visit_expr(&tern.if_clause.0, tern.if_clause.1));
                validate!(self.visit_expr(&tern.else_clause.0, tern.else_clause.1));
                Ok(())
            }
            Expression::Path(path) => {
                validate!(self.visit_expr(&path.base, span));
                for element in &path.elements {
                    match element {
                        PathElement::Index((indexed, at)) => {
                            validate!(self.visit_expr(indexed, *at))
                        }
                        PathElement::Invoke(args) => {
                            for (arg, at) in args {
                                validate!(self.visit_expr(arg, *at))
                            }
                        }
                        PathElement::NullAssert => {
                            if matches!(
                                &*path.base,
                                Expression::Literal(ValueBody {
                                    value: Literal::Nil,
                                    ..
                                })
                            ) {
                                validate!(Err(ValidationError::ConstantCondition {
                                    desc: "Nil assertion on a nil field".to_owned(),
                                    at: CodeArea {
                                        src: self.src.clone(),
                                        span
                                    }
                                }));
                            }
                        }
                        _ => {}
                    }
                }
                Ok(())
            }
        }
    }

    fn visit_literal(&mut self, lit: &Literal, span: Span) -> Valid {
        macro_rules! validate {
            ($expr:expr) => {{
                let res = $expr;
                self.validate(res);
            }};
        }
        match lit {
            Literal::Reference(name) => {
                if !self.scope.variables.contains_key(name) {
                    validate!(Err(ValidationError::UndefinedReference {
                        to: name.to_owned(),
                        at: CodeArea {
                            src: self.src.clone(),
                            span,
                        },
                    }));
                }
                Ok(())
            }
            Literal::TypeDefinition(types) => {
                if types.is_empty() {
                    ValidationError::WarnEmptyType {
                        at: CodeArea {
                            src: self.src.clone(),
                            span,
                        },
                    }
                    .report();
                    self.warn_count += 1;
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

impl Visitor for Validator {
    fn visit_stmt(&mut self, tree: &crate::ast::Statement) {
        macro_rules! validate {
            ($expr:expr) => {{
                let res = $expr;
                self.validate(res);
            }};
        }
        match tree {
            Statement::Import((import, at)) => {
                let native_library_path = Path::new(import);
                if !native_library_path.exists() {
                    // since the library does not exist, we need to track
                    // the actual module location
                    if !import.starts_with("core") && !import.starts_with("std") {
                        // TODO: extract exports here too
                        // ensure that we are not looking for corelib/stdlib
                        let parts = import.split('.').collect::<Vec<&str>>();
                        if parts[0] != self.module_name {
                            // seems that we need to find an extern library
                            let path = self.project_root.join("bling/libs");
                            let needed_part = parts.join("/") + ".cd";
                            let needed_path = path.join(&needed_part);
                            if !needed_path.exists() {
                                // ok, maybe its not in the bling folder
                                let path = self.project_root.join("libs");
                                let second_needed_path = path.join(&needed_part);
                                if !second_needed_path.exists() {
                                    // invalid import!
                                    validate!(Err(ValidationError::UnresolvedImport {
                                        import: import.to_owned(),
                                        searched: vec![needed_path, second_needed_path],
                                        at: CodeArea {
                                            src: self.src.clone(),
                                            span: *at,
                                        },
                                    }));
                                }
                            }
                        }
                    } else {
                        // extracting exports from corelib/stdlib
                        let exports = match import as &str {
                            "core.intrinsics" => {
                                vec!["native_assert"]
                            }
                            "core" => {
                                vec![
                                    "Empty",
                                    "NullableBox",
                                    "NonNullBox",
                                    "debug",
                                    "panic",
                                    "assert",
                                    "unreachable",
                                ]
                            }
                            "std.io" => {
                                vec!["println", "readln", "print", "read"]
                            }
                            _ => {
                                validate!(Err(ValidationError::UnresolvedImport {
                                    import: import.clone(),
                                    searched: vec![],
                                    at: CodeArea {
                                        src: self.src.clone(),
                                        span: *at
                                    }
                                }));
                                vec![]
                            }
                        }
                        .into_iter()
                        .map(|each| {
                            (
                                each.to_owned(),
                                Variable {
                                    mutable: false,
                                    defined_at: *at,
                                },
                            )
                        });
                        self.scope.variables.extend(exports);
                    }
                }
            }
            Statement::Export((export, at)) => {
                if !self.scope.variables.contains_key(export) {
                    validate!(Err(ValidationError::UndefinedReference {
                        to: export.to_owned(),
                        at: CodeArea {
                            src: self.src.clone(),
                            span: *at,
                        },
                    }))
                }
            }
            Statement::Module((module_name, _)) => {
                self.module_name = module_name.to_owned();
            }
            Statement::Function((name, name_span), args, body) => {
                self.scope.variables.insert(
                    name.clone(),
                    Variable {
                        mutable: false,
                        defined_at: *name_span,
                    },
                );
                let mut prev = AHashMap::with_capacity(args.len());
                for (arg, at) in args {
                    if let Some(v) = self.scope.variables.insert(
                        arg.clone(),
                        Variable {
                            mutable: false,
                            defined_at: *at,
                        },
                    ) {
                        prev.insert(arg, v);
                    }
                }
                for stmt in body {
                    self.visit_stmt(stmt)
                }
                for (arg, _) in args {
                    self.scope.variables.remove(arg);
                    if prev.contains_key(arg) {
                        self.scope
                            .variables
                            .insert(arg.clone(), prev.remove(arg).unwrap());
                    }
                }
            }
            Statement::Variable((name, (name_start, _)), (value, at)) => {
                self.scope.variables.insert(
                    name.clone(),
                    Variable {
                        mutable: true,
                        defined_at: (*name_start, at.1),
                    },
                );
                validate!(self.visit_expr(value, *at));
            }
            Statement::Constant((name, (name_start, _)), (value, at)) => {
                self.scope.variables.insert(
                    name.clone(),
                    Variable {
                        mutable: false,
                        defined_at: (*name_start, at.1),
                    },
                );
                validate!(self.visit_expr(value, *at));
            }
            Statement::NativeConstant((native, at)) => {
                self.scope.variables.insert(
                    native.clone(),
                    Variable {
                        mutable: false,
                        defined_at: *at,
                    },
                );
            }
            Statement::NativeFunction((name, at), _) => {
                self.scope.variables.insert(
                    name.clone(),
                    Variable {
                        mutable: false,
                        defined_at: *at,
                    },
                );
            }
            Statement::AssignValue((assignee, assignee_at), _, (assigned, assigned_at)) => {
                match assignee {
                    Expression::Literal(ValueBody {
                        value: Literal::Reference(name),
                        ..
                    }) => {
                        if self.scope.variables.contains_key(name)
                            && !self.scope.variables[name].mutable
                        {
                            validate!(Err(ValidationError::ConstantAssignment {
                                name: name.to_owned(),
                                at: CodeArea {
                                    src: self.src.clone(),
                                    span: *assignee_at
                                },
                                constant_defined_at: CodeArea {
                                    src: self.src.clone(),
                                    span: self.scope.variables[name].defined_at
                                }
                            }))
                        }
                    }
                    Expression::Path(path) => {
                        if !matches!(
                            path.elements.last().unwrap(),
                            PathElement::AccessProperty(_)
                        ) {
                            validate!(Err(ValidationError::InvalidAssignmentAssignee {
                                assignee: assignee.clone(),
                                at: CodeArea {
                                    src: self.src.clone(),
                                    span: (assigned_at.0, assigned_at.1),
                                },
                            }));
                        }
                    }
                    other => {
                        validate!(Err(ValidationError::InvalidAssignmentAssignee {
                            assignee: other.clone(),
                            at: CodeArea {
                                src: self.src.clone(),
                                span: (assigned_at.0, assignee_at.1),
                            },
                        }));
                    }
                }
                validate!(self.visit_expr(assignee, *assignee_at));
                validate!(self.visit_expr(assigned, *assigned_at));
            }
            Statement::Return((ret, ret_at)) => {
                validate!(self.visit_expr(ret, *ret_at));
            }
            Statement::If(if_stmt) => {
                macro_rules! check_if_expr {
                    ($checked:expr,$location:expr) => {
                        match $checked {
                            Expression::Literal(ValueBody {
                                value: Literal::Boolean(bool),
                                ..
                            }) => {
                                if bool {
                                    ValidationError::WarnConstantCondition {
                                        desc: "This if body will always evaluate".to_owned(),
                                        tip: format!(
                                            "Consider removing the {}, and use the code by itself instead.",
                                            "if statement".fg(self.colors.next_color())
                                        ),
                                        at: CodeArea {
                                            src: self.src.clone(),
                                            span: $location,
                                        },
                                    }
                                    .report();
                                    self.warn_count += 1;
                                } else {
                                    ValidationError::WarnConstantCondition {
                                        desc: "This if body will never evaluate".to_owned(),
                                        tip: format!(
                                            "Consider removing the {}, and its underlying code.",
                                            "if statement".fg(self.colors.next_color())
                                        ),
                                        at: CodeArea {
                                            src: self.src.clone(),
                                            span: $location,
                                        },
                                    }
                                    .report();
                                    self.warn_count += 1;
                                }
                            }
                            _ => {}
                        }

                    };
                }

                check_if_expr!(if_stmt.condition.0, if_stmt.condition.1);
                validate!(self.visit_expr(&if_stmt.condition.0, if_stmt.condition.1));
                for part in &if_stmt.body {
                    self.visit_stmt(part);
                }
                for else_if in &if_stmt.else_ifs {
                    check_if_expr!(else_if.condition.0, else_if.condition.1);
                    validate!(self.visit_expr(&else_if.condition.0, else_if.condition.1));
                    for part in &else_if.body {
                        self.visit_stmt(part)
                    }
                }
                if let Some(else_body) = &if_stmt.else_body {
                    for part in else_body {
                        self.visit_stmt(part);
                    }
                }
            }
            Statement::WhileLoop((cond, cond_at), body) => {
                validate!(self.visit_expr(cond, *cond_at));
                for part in body {
                    self.visit_stmt(part)
                }
            }
            Statement::ForLoop(for_loop) => {
                let (iter, iter_at) = &for_loop.iterator;
                validate!(self.visit_expr(iter, *iter_at));
                for part in &for_loop.code {
                    self.visit_stmt(part)
                }
            }
            Statement::Throw((throwable, thr_at)) => {
                if matches!(
                    throwable,
                    Expression::Literal(ValueBody {
                        value: Literal::Nil,
                        ..
                    })
                ) {
                    validate!(Err(ValidationError::Unsupported {
                        desc: "Explicit nil throws are unsafe and not supported.\nReplace with `core.panic` for similar effect.".to_owned(),
                        at: CodeArea { src: self.src.clone(), span: *thr_at }
                    }))
                }
                validate!(self.visit_expr(throwable, *thr_at));
            }
            Statement::TryCatch(try_catch) => {
                for part in &try_catch.try_clause {
                    self.visit_stmt(part)
                }
                for catch in &try_catch.catch_clauses {
                    for part in &catch.body {
                        self.visit_stmt(part);
                    }
                }
            }
            Statement::Expression((expr, span)) => {
                validate!(self.visit_expr(expr, *span));
            }
            _ => {}
        }
    }
}
