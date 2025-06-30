use crate::parser::{Statement, StatementKind, Expression, ExpressionKind, LiteralValue};
use crate::lexer::{Span, TokenType, Token};
use crate::stdlib;
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

pub type KopiNativeFn = Rc<dyn Fn(Vec<KopiValue>) -> Result<KopiValue, String>>;

#[derive(Clone)]
pub enum KopiValue {
    String(String),
    Boolean(bool),
    Number(f64),
    Array(Rc<RefCell<Vec<KopiValue>>>),
    Module(Rc<RefCell<stdlib::KopiModule>>),
    Function { name: String, params: Vec<Token>, body: Vec<Statement> },
    NativeFunction { name: String, func: KopiNativeFn },
    Nil,
}

impl KopiValue {
    pub fn new_native_fn<F>(name: &str, func: F) -> Self
    where
        F: Fn(Vec<KopiValue>) -> Result<KopiValue, String> + 'static,
    {
        KopiValue::NativeFunction {
            name: name.to_string(),
            func: Rc::new(func),
        }
    }
}

impl std::fmt::Debug for KopiValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::String(v) => write!(f, "String({:?})", v),
            Self::Boolean(v) => write!(f, "Boolean({:?})", v),
            Self::Number(v) => write!(f, "Number({:?})", v),
            Self::Array(v) => write!(f, "Array({:?})", v.borrow()),
            Self::Module(v) => write!(f, "Module({:?})", v.borrow().keys()),
            Self::Function { name, .. } => write!(f, "Function {{ name: {:?} }}", name),
            Self::NativeFunction { name, .. } => write!(f, "NativeFunction {{ name: {:?} }}", name),
            Self::Nil => write!(f, "Nil"),
        }
    }
}

impl PartialEq for KopiValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::String(l), Self::String(r)) => l == r,
            (Self::Boolean(l), Self::Boolean(r)) => l == r,
            (Self::Number(l), Self::Number(r)) => l == r,
            (Self::Array(l), Self::Array(r)) => Rc::ptr_eq(l, r),
            (Self::Module(l), Self::Module(r)) => Rc::ptr_eq(l, r),
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

impl std::fmt::Display for KopiValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            KopiValue::String(s) => write!(f, "{}", s),
            KopiValue::Boolean(b) => write!(f, "{}", b),
            KopiValue::Number(n) => write!(f, "{}", n),
            KopiValue::Function { name, .. } => write!(f, "<fun {}>", name),
            KopiValue::NativeFunction { name, .. } => write!(f, "<native fun {}>", name),
            KopiValue::Array(arr) => {
                let elements: Vec<String> = arr.borrow().iter().map(|v| v.to_string()).collect();
                write!(f, "[{}]", elements.join(", "))
            },
            KopiValue::Module(m) => write!(f, "<module {:?}>", m.borrow().keys()),
            KopiValue::Nil => write!(f, "nil"),
        }
    }
}

enum ExecutionFlow { Normal, Return(KopiValue) }
pub struct Interpreter {
    environment: HashMap<String, KopiValue>,
    available_modules: HashMap<String, stdlib::KopiModule>,
}
impl Interpreter {
    pub fn new() -> Self {
        let mut available_modules = HashMap::new();
        available_modules.insert("os".to_string(), stdlib::create_os_module());
        available_modules.insert("io".to_string(), stdlib::create_io_module());
        available_modules.insert("color".to_string(), stdlib::create_color_module());

        let mut interpreter = Interpreter {
            environment: HashMap::new(),
            available_modules,
        };
        interpreter.load_builtins();
        interpreter
    }

    fn load_builtins(&mut self) {
        self.environment.insert("put".to_string(), KopiValue::new_native_fn("put", stdlib::put));
        self.environment.insert("len".to_string(), KopiValue::new_native_fn("len", stdlib::len));
    }

    pub fn run(&mut self, statements: Vec<Statement>) -> Result<(), (String, Span)> { self.execute_block(statements)?; Ok(()) }
    fn execute_block(&mut self, statements: Vec<Statement>) -> Result<ExecutionFlow, (String, Span)> { for statement in statements { match self.run_statement(statement)? { ExecutionFlow::Return(value) => return Ok(ExecutionFlow::Return(value)), ExecutionFlow::Normal => continue, } } Ok(ExecutionFlow::Normal) }

    fn run_statement(&mut self, statement: Statement) -> Result<ExecutionFlow, (String, Span)> {
        match statement.kind {
            StatementKind::Import { module_name } => {
                if let TokenType::Identifier(name) = module_name.kind {
                    if let Some(module) = self.available_modules.get(&name) {
                        let module_val = KopiValue::Module(Rc::new(RefCell::new(module.clone())));
                        self.environment.insert(name, module_val);
                    } else {
                        return Err((format!("Module '{}' not found.", name), module_name.span));
                    }
                }
            }
            StatementKind::Let { name, value } => { let value_to_store = self.evaluate(&value)?; if let TokenType::Identifier(name_str) = name.kind { self.environment.insert(name_str, value_to_store); } },
            StatementKind::FunctionDeclaration { name, params, body } => { if let TokenType::Identifier(name_str) = name.kind { let function = KopiValue::Function { name: name_str.clone(), params, body }; self.environment.insert(name_str, function); } },
            StatementKind::If { condition, consequence, alternative } => { let condition_value = self.evaluate(&condition)?; if self.is_truthy(condition_value) { return self.execute_block(consequence); } else if let Some(alt) = alternative { return self.execute_block(alt); } },
            StatementKind::While { condition, body } => { let mut condition_value = self.evaluate(&condition)?; while self.is_truthy(condition_value) { match self.execute_block(body.clone())? { ExecutionFlow::Return(val) => return Ok(ExecutionFlow::Return(val)), ExecutionFlow::Normal => (), } condition_value = self.evaluate(&condition)?; } },
            StatementKind::ExpressionStatement(expr) => { self.evaluate(&expr)?; },
            StatementKind::Return { keyword: _, value } => { let return_value = match value { Some(expr) => self.evaluate(&expr)?, None => KopiValue::Nil, }; return Ok(ExecutionFlow::Return(return_value)); }
        }
        Ok(ExecutionFlow::Normal)
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<KopiValue, (String, Span)> {
        match &expr.kind {
            ExpressionKind::Assignment { name, value } => { let value_to_assign = self.evaluate(value)?; if let TokenType::Identifier(name_str) = &name.kind { if self.environment.contains_key(name_str) { self.environment.insert(name_str.clone(), value_to_assign.clone()); return Ok(value_to_assign); } else { return Err((format!("Cannot assign to undefined variable '{}'.", name_str), name.span.clone())); } } unreachable!(); },
            ExpressionKind::Literal(value) => match value { LiteralValue::String(s) => Ok(KopiValue::String(s.clone())), LiteralValue::Boolean(b) => Ok(KopiValue::Boolean(*b)), LiteralValue::Number(n) => Ok(KopiValue::Number(*n)), },
            ExpressionKind::ArrayLiteral { elements } => { let mut values = Vec::new(); for element_expr in elements { values.push(self.evaluate(element_expr)?); } Ok(KopiValue::Array(Rc::new(RefCell::new(values)))) },
            ExpressionKind::Index { left, index } => { let left_val = self.evaluate(left)?; let index_val = self.evaluate(index)?; match (left_val, index_val) { (KopiValue::Array(arr), KopiValue::Number(idx)) => { let idx = idx as usize; let arr_borrow = arr.borrow(); if let Some(value) = arr_borrow.get(idx) { Ok(value.clone()) } else { Err((format!("Index {} out of bounds for array of length {}.", idx, arr_borrow.len()), expr.span.clone())) } } _ => Err(("Can only index arrays with numbers.".to_string(), expr.span.clone())) } },
            ExpressionKind::PropertyAccess { object, property } => {
                let object_val = self.evaluate(object)?;
                if let TokenType::Identifier(prop_name) = &property.kind {
                    if let KopiValue::Module(module) = object_val {
                        if let Some(member) = module.borrow().get(prop_name) {
                            return Ok(member.clone());
                        }
                    }
                    return Err((format!("Value does not have property '{}'", prop_name), property.span.clone()));
                }
                Err(("Invalid property name".to_string(), property.span.clone()))
            },
            ExpressionKind::Unary { operator, right } => { let right_val = self.evaluate(right)?; match operator.kind { TokenType::Not => Ok(KopiValue::Boolean(!self.is_truthy(right_val))), TokenType::Minus => { if let KopiValue::Number(n) = right_val { Ok(KopiValue::Number(-n)) } else { Err(("Unary '-' can only be applied to numbers.".to_string(), operator.span.clone())) } } _ => Err(("Unknown unary operator.".to_string(), operator.span.clone())), } },
            ExpressionKind::Variable(name) => { if let Some(value) = self.environment.get(name) { Ok(value.clone()) } else { Err((format!("Variable '{}' not found.", name), expr.span.clone())) } },
            ExpressionKind::Infix { left, operator, right } => { let left_val = self.evaluate(left)?; if operator.kind == TokenType::Or { if self.is_truthy(left_val.clone()) { return Ok(left_val); } return self.evaluate(right); } if operator.kind == TokenType::And { if !self.is_truthy(left_val.clone()) { return Ok(left_val); } return self.evaluate(right); } let right_val = self.evaluate(right)?; match operator.kind {
                TokenType::Plus => { match (left_val, right_val) { (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Number(l + r)), (KopiValue::String(l), KopiValue::String(r)) => Ok(KopiValue::String(format!("{}{}", l, r))), _ => Err(("The '+' operator can only be used on two numbers or two strings.".to_string(), operator.span.clone())) } },
                TokenType::EqualsEquals => Ok(KopiValue::Boolean(left_val == right_val)), TokenType::NotEquals => Ok(KopiValue::Boolean(left_val != right_val)),
                TokenType::LessThan | TokenType::LessThanOrEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual => { if let (KopiValue::Number(l), KopiValue::Number(r)) = (left_val, right_val) { match operator.kind { TokenType::LessThan => Ok(KopiValue::Boolean(l < r)), TokenType::LessThanOrEqual => Ok(KopiValue::Boolean(l <= r)), TokenType::GreaterThan => Ok(KopiValue::Boolean(l > r)), TokenType::GreaterThanOrEqual => Ok(KopiValue::Boolean(l >= r)), _ => unreachable!(), } } else { Err(("Comparison operators can only be used on numbers.".to_string(), operator.span.clone())) } },
                TokenType::Minus | TokenType::Star | TokenType::Slash => { if let (KopiValue::Number(l), KopiValue::Number(r)) = (left_val, right_val) { match operator.kind { TokenType::Minus => Ok(KopiValue::Number(l - r)), TokenType::Star  => Ok(KopiValue::Number(l * r)), TokenType::Slash => Ok(KopiValue::Number(l / r)), _ => unreachable!(), } } else { Err(("Arithmetic operators can only be used on numbers.".to_string(), operator.span.clone())) } },
                _ => Err(("Unknown infix operator.".to_string(), operator.span.clone())),
            } },
            ExpressionKind::FunctionCall { name, args } => { let callee = self.evaluate(name)?; match callee { KopiValue::NativeFunction { func, .. } => { let mut evaluated_args = Vec::new(); for arg_expr in args { evaluated_args.push(self.evaluate(arg_expr)?); } match func(evaluated_args) { Ok(val) => Ok(val), Err(e) => Err((e, name.span.clone())) } }, KopiValue::Function { name: _func_name, params, body } => { if args.len() != params.len() { return Err((format!("Expected {} arguments but got {}.", params.len(), args.len()), expr.span.clone())); } let mut new_scope = self.environment.clone(); for (param, arg) in params.iter().zip(args.iter()) { let arg_val = self.evaluate(arg)?; if let TokenType::Identifier(param_name) = &param.kind { new_scope.insert(param_name.clone(), arg_val); } } let old_env = self.environment.clone(); self.environment = new_scope; let flow = self.execute_block(body)?; self.environment = old_env; match flow { ExecutionFlow::Return(value) => Ok(value), ExecutionFlow::Normal => Ok(KopiValue::Nil), } } _ => Err(("Can only call functions.".to_string(), name.span.clone())), } },
        }
    }
    fn is_truthy(&self, value: KopiValue) -> bool { match value { KopiValue::Boolean(b) => b, KopiValue::String(s) => !s.is_empty(), KopiValue::Number(n) => n != 0.0, KopiValue::Nil => false, _ => true, } }
}
