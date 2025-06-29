use crate::parser::{Statement, StatementKind, Expression, ExpressionKind, LiteralValue};
use crate::lexer::{Span, TokenType, Token};
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum KopiValue {
    String(String),
    Boolean(bool),
    Number(f64),
    Function {
        name: String,
        params: Vec<Token>,
        body: Vec<Statement>,
    },
    Nil,
}

impl std::fmt::Display for KopiValue {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            KopiValue::String(s) => write!(f, "{}", s),
            KopiValue::Boolean(b) => write!(f, "{}", b),
            KopiValue::Number(n) => write!(f, "{}", n),
            KopiValue::Function { name, .. } => write!(f, "<fun {}>", name),
            KopiValue::Nil => write!(f, "nil"),
        }
    }
}

enum ExecutionFlow {
    Normal,
    Return(KopiValue),
}

pub struct Interpreter {
    environment: HashMap<String, KopiValue>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter { environment: HashMap::new() }
    }

    pub fn run(&mut self, statements: Vec<Statement>) -> Result<(), (String, Span)> {
        self.execute_block(statements)?;
        Ok(())
    }

    fn execute_block(&mut self, statements: Vec<Statement>) -> Result<ExecutionFlow, (String, Span)> {
        for statement in statements {
            match self.run_statement(statement)? {
                ExecutionFlow::Return(value) => return Ok(ExecutionFlow::Return(value)),
                ExecutionFlow::Normal => continue,
            }
        }
        Ok(ExecutionFlow::Normal)
    }

    fn run_statement(&mut self, statement: Statement) -> Result<ExecutionFlow, (String, Span)> {
        match statement.kind {
            StatementKind::Let { name, value } => {
                let value_to_store = self.evaluate(&value)?;
                if let TokenType::Identifier(name_str) = name.kind {
                     self.environment.insert(name_str, value_to_store);
                }
            }
            StatementKind::FunctionDeclaration { name, params, body } => {
                if let TokenType::Identifier(name_str) = name.kind {
                    let function = KopiValue::Function { name: name_str.clone(), params, body };
                    self.environment.insert(name_str, function);
                }
            }
            StatementKind::If { condition, consequence, alternative } => {
                let condition_value = self.evaluate(&condition)?;
                if self.is_truthy(condition_value) {
                    return self.execute_block(consequence);
                } else if let Some(alt) = alternative {
                    return self.execute_block(alt);
                }
            }
            StatementKind::While { condition, body } => {
                let mut condition_value = self.evaluate(&condition)?;
                while self.is_truthy(condition_value) {
                    match self.execute_block(body.clone())? {
                        ExecutionFlow::Return(val) => return Ok(ExecutionFlow::Return(val)),
                        ExecutionFlow::Normal => (),
                    }
                    condition_value = self.evaluate(&condition)?;
                }
            }
            StatementKind::ExpressionStatement(expr) => {
                self.evaluate(&expr)?;
            }
            StatementKind::Return { keyword: _, value } => {
                let return_value = match value {
                    Some(expr) => self.evaluate(&expr)?,
                    None => KopiValue::Nil,
                };
                return Ok(ExecutionFlow::Return(return_value));
            }
        }
        Ok(ExecutionFlow::Normal)
    }

    fn evaluate(&mut self, expr: &Expression) -> Result<KopiValue, (String, Span)> {
        match &expr.kind {
            ExpressionKind::Assignment { name, value } => {
                let value_to_assign = self.evaluate(value)?;
                if let TokenType::Identifier(name_str) = &name.kind {
                    if self.environment.contains_key(name_str) {
                        self.environment.insert(name_str.clone(), value_to_assign.clone());
                        return Ok(value_to_assign);
                    } else {
                        return Err((format!("Cannot assign to undefined variable '{}'.", name_str), name.span.clone()));
                    }
                }
                unreachable!();
            }
            ExpressionKind::Literal(value) => match value {
                LiteralValue::String(s) => Ok(KopiValue::String(s.clone())),
                LiteralValue::Boolean(b) => Ok(KopiValue::Boolean(*b)),
                LiteralValue::Number(n) => Ok(KopiValue::Number(*n)),
            },
            ExpressionKind::Unary { operator, right } => {
                let right_val = self.evaluate(right)?;
                match operator.kind {
                    TokenType::Not => Ok(KopiValue::Boolean(!self.is_truthy(right_val))),
                    TokenType::Minus => {
                        if let KopiValue::Number(n) = right_val {
                            Ok(KopiValue::Number(-n))
                        } else {
                            Err(("Unary '-' can only be applied to numbers.".to_string(), operator.span.clone()))
                        }
                    }
                    _ => Err(("Unknown unary operator.".to_string(), operator.span.clone())),
                }
            }
            ExpressionKind::Variable(name) => {
                if name == "put" {
                    return Ok(KopiValue::Function {
                        name: "put".to_string(),
                        params: vec![],
                        body: vec![],
                    });
                }
                if let Some(value) = self.environment.get(name) {
                    Ok(value.clone())
                } else {
                    Err((format!("Variable '{}' not found.", name), expr.span.clone()))
                }
            }
            ExpressionKind::Infix { left, operator, right } => {
                if operator.kind == TokenType::Or {
                    let left_val = self.evaluate(left)?;
                    if self.is_truthy(left_val.clone()) { return Ok(left_val); }
                    return self.evaluate(right);
                }
                
                if operator.kind == TokenType::And {
                    let left_val = self.evaluate(left)?;
                    if !self.is_truthy(left_val.clone()) { return Ok(left_val); }
                    return self.evaluate(right);
                }

                let left_val = self.evaluate(left)?;
                let right_val = self.evaluate(right)?;

                match operator.kind {
                    TokenType::EqualsEquals => Ok(KopiValue::Boolean(left_val == right_val)),
                    TokenType::NotEquals => Ok(KopiValue::Boolean(left_val != right_val)),
                    TokenType::LessThan | TokenType::LessThanOrEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual => {
                        if let (KopiValue::Number(l), KopiValue::Number(r)) = (left_val, right_val) {
                            match operator.kind {
                                TokenType::LessThan => Ok(KopiValue::Boolean(l < r)),
                                TokenType::LessThanOrEqual => Ok(KopiValue::Boolean(l <= r)),
                                TokenType::GreaterThan => Ok(KopiValue::Boolean(l > r)),
                                TokenType::GreaterThanOrEqual => Ok(KopiValue::Boolean(l >= r)),
                                _ => unreachable!(),
                            }
                        } else {
                            Err(("Comparison operators can only be used on numbers.".to_string(), operator.span.clone()))
                        }
                    }
                    TokenType::Plus | TokenType::Minus | TokenType::Star | TokenType::Slash => {
                        if let (KopiValue::Number(l), KopiValue::Number(r)) = (left_val, right_val) {
                            match operator.kind {
                                TokenType::Plus  => Ok(KopiValue::Number(l + r)),
                                TokenType::Minus => Ok(KopiValue::Number(l - r)),
                                TokenType::Star  => Ok(KopiValue::Number(l * r)),
                                TokenType::Slash => Ok(KopiValue::Number(l / r)),
                                _ => unreachable!(),
                            }
                        } else {
                            Err(("Arithmetic operators can only be used on numbers.".to_string(), operator.span.clone()))
                        }
                    }
                    _ => Err(("Unknown infix operator.".to_string(), operator.span.clone())),
                }
            }
            ExpressionKind::FunctionCall { name, args } => {
                let callee = self.evaluate(name)?;

                match callee {
                    KopiValue::Function { name: func_name, params, body } => {
                        if func_name == "put" {
                            if args.len() != 1 { return Err(("put() expects 1 argument".to_string(), expr.span.clone())); }
                            let arg_val = self.evaluate(&args[0])?;
                            println!("{}", arg_val);
                            return Ok(KopiValue::Nil);
                        }

                        if args.len() != params.len() {
                            return Err((format!("Expected {} arguments but got {}.", params.len(), args.len()), expr.span.clone()));
                        }

                        let mut new_scope = self.environment.clone();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            let arg_val = self.evaluate(arg)?;
                            if let TokenType::Identifier(param_name) = &param.kind {
                                new_scope.insert(param_name.clone(), arg_val);
                            }
                        }

                        let old_env = self.environment.clone();
                        self.environment = new_scope;
                        let flow = self.execute_block(body)?;
                        self.environment = old_env;

                        match flow {
                            ExecutionFlow::Return(value) => Ok(value),
                            ExecutionFlow::Normal => Ok(KopiValue::Nil),
                        }
                    }
                    _ => Err(("Can only call functions.".to_string(), name.span.clone())),
                }
            }
        }
    }

    fn is_truthy(&self, value: KopiValue) -> bool {
        match value {
            KopiValue::Boolean(b) => b,
            KopiValue::String(s) => !s.is_empty(),
            KopiValue::Number(n) => n != 0.0,
            KopiValue::Nil => false,
            _ => true,
        }
    }
}
