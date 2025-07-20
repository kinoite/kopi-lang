use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, Clone)]
pub enum KopiValue {
    Number(f64),
    String(String),
    Boolean(bool),
    Array(Rc<RefCell<Vec<KopiValue>>>),
    Function(Rc<KopiFunction>),
    NativeFunction(Rc<KopiNativeFunction>),
    Module(Rc<RefCell<HashMap<String, KopiValue>>>),
    Nil,
}

#[derive(Debug)]
pub struct KopiFunction {
    pub params: Vec<String>,
    pub body: Vec<Stmt>,
    pub closure: Rc<RefCell<Environment>>,
}

#[derive(Debug)]
pub struct KopiNativeFunction {
    pub name: String,
    pub func: Rc<dyn Fn(Vec<KopiValue>) -> Result<KopiValue, String>>,
}

impl PartialEq for KopiValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (KopiValue::Number(a), KopiValue::Number(b)) => a == b,
            (KopiValue::String(a), KopiValue::String(b)) => a == b,
            (KopiValue::Boolean(a), KopiValue::Boolean(b)) => a == b,
            (KopiValue::Nil, KopiValue::Nil) => true,
            (KopiValue::Array(a), KopiValue::Array(b)) => Rc::ptr_eq(a, b) || a.borrow().eq(&b.borrow()),
            (KopiValue::Function(a), KopiValue::Function(b)) => Rc::ptr_eq(a, b),
            (KopiValue::NativeFunction(a), KopiValue::NativeFunction(b)) => Rc::ptr_eq(a, b),
            (KopiValue::Module(a), KopiValue::Module(b)) => Rc::ptr_eq(a, b) || a.borrow().eq(&b.borrow()),
            _ => false,
        }
    }
}

impl KopiValue {
    pub fn new_native_fn(name: &str, func: impl Fn(Vec<KopiValue>) -> Result<KopiValue, String> + 'static) -> Self {
        KopiValue::NativeFunction(Rc::new(KopiNativeFunction {
            name: name.to_string(),
            func: Rc::new(func),
        }))
    }
}

impl std::fmt::Display for KopiValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KopiValue::Number(n) => write!(f, "{}", n),
            KopiValue::String(s) => write!(f, "{}", s),
            KopiValue::Boolean(b) => write!(f, "{}", b),
            KopiValue::Array(arr) => {
                let borrowed_arr = arr.borrow();
                let elements: Vec<String> = borrowed_arr.iter().map(|v| format!("{}", v)).collect();
                write!(f, "[{}]", elements.join(", "))
            },
            KopiValue::Function(_) => write!(f, "<function>"),
            KopiValue::NativeFunction(nf) => write!(f, "<native function {}>", nf.name),
            KopiValue::Module(_) => write!(f, "<module>"),
            KopiValue::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Literal(KopiValue),
    Variable(String),
    Binary(Box<Expr>, TokenKind, Box<Expr>),
    Unary(TokenKind, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Assign(String, Box<Expr>),
    Logical(Box<Expr>, TokenKind, Box<Expr>),
    Get(Box<Expr>, String),
    Set(Box<Expr>, String, Box<Expr>),
    Array(Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expression(Expr),
    Print(Expr),
    Var(String, Option<Expr>),
    Block(Vec<Stmt>),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Function(String, Vec<String>, Vec<Stmt>),
    Return(Option<Expr>),
    Import(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace, LeftBracket, RightBracket,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(f64),

    // Keywords.
    And, Class, Else, False, Fun, For, If, Nil, Or,
    Print, Return, Super, This, True, Var, While,
    Import,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: String, line: usize) -> Self {
        Token { kind, lexeme, line }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    enclosing: Option<Rc<RefCell<Environment>>>,
    values: HashMap<String, KopiValue>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    pub fn new_enclosed(enclosing: Rc<RefCell<Environment>>) -> Self {
        Environment {
            enclosing: Some(enclosing),
            values: HashMap::new(),
        }
    }

    pub fn define(&mut self, name: String, value: KopiValue) {
        self.values.insert(name, value);
    }

    pub fn get(&self, name: &str) -> Result<KopiValue, String> {
        if let Some(value) = self.values.get(name) {
            Ok(value.clone())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow().get(name)
        } else {
            Err(format!("Undefined variable '{}'.", name))
        }
    }

    pub fn assign(&mut self, name: &str, value: KopiValue) -> Result<(), String> {
        if self.values.contains_key(name) {
            self.values.insert(name.to_string(), value);
            Ok(())
        } else if let Some(enclosing) = &self.enclosing {
            enclosing.borrow_mut().assign(name, value)
        } else {
            Err(format!("Undefined variable '{}'.", name))
        }
    }
}

pub struct Interpreter {
    environment: Rc<RefCell<Environment>>,
    global_env: Rc<RefCell<Environment>>,
}

impl Interpreter {
    pub fn new(stdlib: HashMap<String, KopiValue>) -> Self {
        let global_env = Rc::new(RefCell::new(Environment::new()));
        for (name, value) in stdlib {
            global_env.borrow_mut().define(name, value);
        }
        Interpreter {
            environment: Rc::clone(&global_env),
            global_env,
        }
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Result<(), String> {
        for stmt in statements {
            self.execute(stmt)?;
        }
        Ok(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), String> {
        match stmt {
            Stmt::Expression(expr) => {
                self.evaluate(expr)?;
                Ok(())
            },
            Stmt::Print(expr) => {
                let value = self.evaluate(expr)?;
                println!("{}", value);
                Ok(())
            },
            Stmt::Var(name, initializer) => {
                let value = if let Some(expr) = initializer {
                    self.evaluate(expr)?
                } else {
                    KopiValue::Nil
                };
                self.environment.borrow_mut().define(name, value);
                Ok(())
            },
            Stmt::Block(statements) => {
                self.execute_block(statements, Environment::new_enclosed(Rc::clone(&self.environment)))
            },
            Stmt::If(condition, then_branch, else_branch) => {
                if self.is_truthy(self.evaluate(condition)?) {
                    self.execute(*then_branch)
                } else if let Some(else_stmt) = else_branch {
                    self.execute(*else_stmt)
                } else {
                    Ok(())
                }
            },
            Stmt::While(condition, body) => {
                while self.is_truthy(self.evaluate(condition.clone())?) {
                    self.execute(body.clone())?;
                }
                Ok(())
            },
            Stmt::Function(name, params, body) => {
                let function = KopiValue::Function(Rc::new(KopiFunction {
                    params,
                    body,
                    closure: Rc::clone(&self.environment),
                }));
                self.environment.borrow_mut().define(name, function);
                Ok(())
            },
            Stmt::Return(expr_opt) => {
                let value = if let Some(expr) = expr_opt {
                    self.evaluate(expr)?
                } else {
                    KopiValue::Nil
                };
                return Err(format!("RETURN_VALUE:{}", value.to_string()));
            },
            Stmt::Import(module_name) => {
                let module_value = self.global_env.borrow().get(&module_name)?;
                if let KopiValue::Module(module_rc) = module_value {
                    self.environment.borrow_mut().define(module_name, KopiValue::Module(Rc::clone(&module_rc)));
                    Ok(())
                } else {
                    Err(format!("'{}' is not a module.", module_name))
                }
            },
        }
    }

    fn execute_block(&mut self, statements: Vec<Stmt>, environment: Environment) -> Result<(), String> {
        let previous_env = Rc::clone(&self.environment);
        self.environment = Rc::new(RefCell::new(environment));

        let result = (|| {
            for stmt in statements {
                self.execute(stmt)?;
            }
            Ok(())
        })();

        self.environment = previous_env;
        result
    }

    fn evaluate(&mut self, expr: Expr) -> Result<KopiValue, String> {
        match expr {
            Expr::Literal(value) => Ok(value),
            Expr::Variable(name) => self.environment.borrow().get(&name),
            Expr::Binary(left, operator, right) => {
                let left_val = self.evaluate(*left)?;
                let right_val = self.evaluate(*right)?;

                match operator {
                    TokenKind::Minus => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Number(l - r)),
                        _ => Err("Operands for '-' must be numbers.".to_string()),
                    },
                    TokenKind::Plus => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Number(l + r)),
                        (KopiValue::String(l), KopiValue::String(r)) => Ok(KopiValue::String(l + &r)),
                        (KopiValue::String(l), r) => Ok(KopiValue::String(l + &r.to_string())),
                        (l, KopiValue::String(r)) => Ok(KopiValue::String(l.to_string() + &r)),
                        _ => Err("Operands for '+' must be numbers or strings.".to_string()),
                    },
                    TokenKind::Slash => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => {
                            if r == 0.0 {
                                Err("Division by zero.".to_string())
                            } else {
                                Ok(KopiValue::Number(l / r))
                            }
                        },
                        _ => Err("Operands for '/' must be numbers.".to_string()),
                    },
                    TokenKind::Star => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Number(l * r)),
                        _ => Err("Operands for '*' must be numbers.".to_string()),
                    },
                    TokenKind::Greater => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Boolean(l > r)),
                        _ => Err("Operands for '>' must be numbers.".to_string()),
                    },
                    TokenKind::GreaterEqual => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Boolean(l >= r)),
                        _ => Err("Operands for '>=' must be numbers.".to_string()),
                    },
                    TokenKind::Less => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Boolean(l < r)),
                        _ => Err("Operands for '<' must be numbers.".to_string()),
                    },
                    TokenKind::LessEqual => match (left_val, right_val) {
                        (KopiValue::Number(l), KopiValue::Number(r)) => Ok(KopiValue::Boolean(l <= r)),
                        _ => Err("Operands for '<=' must be numbers.".to_string()),
                    },
                    TokenKind::EqualEqual => Ok(KopiValue::Boolean(left_val == right_val)),
                    TokenKind::BangEqual => Ok(KopiValue::Boolean(left_val != right_val)),
                    _ => Err(format!("Unsupported binary operator: {:?}", operator)),
                }
            },
            Expr::Unary(operator, right) => {
                let right_val = self.evaluate(*right)?;
                match operator {
                    TokenKind::Minus => match right_val {
                        KopiValue::Number(n) => Ok(KopiValue::Number(-n)),
                        _ => Err("Operand for unary '-' must be a number.".to_string()),
                    },
                    TokenKind::Bang => Ok(KopiValue::Boolean(!self.is_truthy(right_val))),
                    _ => Err(format!("Unsupported unary operator: {:?}", operator)),
                }
            },
            Expr::Call(callee, arguments) => {
                let callee_val = self.evaluate(*callee)?;
                let mut arg_values = Vec::new();
                for arg in arguments {
                    arg_values.push(self.evaluate(arg)?);
                }

                match callee_val {
                    KopiValue::Function(func) => {
                        if arg_values.len() != func.params.len() {
                            return Err(format!("Expected {} arguments but got {}.", func.params.len(), arg_values.len()));
                        }
                        let mut call_env = Environment::new_enclosed(Rc::clone(&func.closure));
                        for (i, param) in func.params.iter().enumerate() {
                            call_env.define(param.clone(), arg_values[i].clone());
                        }
                        let previous_env = Rc::clone(&self.environment);
                        self.environment = Rc::new(RefCell::new(call_env));
                        let result = (|| {
                            for stmt in func.body.clone() {
                                self.execute(stmt)?;
                            }
                            Ok(KopiValue::Nil)
                        })();
                        self.environment = previous_env;

                        match result {
                            Err(e) if e.starts_with("RETURN_VALUE:") => {
                                let val_str = e.strip_prefix("RETURN_VALUE:").unwrap();
                                match val_str.parse::<f64>() {
                                    Ok(n) => Ok(KopiValue::Number(n)),
                                    Err(_) => Ok(KopiValue::String(val_str.to_string())),
                                }
                            },
                            _ => result,
                        }
                    },
                    KopiValue::NativeFunction(native_func) => {
                        (native_func.func)(arg_values)
                    },
                    _ => Err("Can only call functions and native functions.".to_string()),
                }
            },
            Expr::Assign(name, value_expr) => {
                let value = self.evaluate(*value_expr)?;
                self.environment.borrow_mut().assign(&name, value.clone())?;
                Ok(value)
            },
            Expr::Logical(left, operator, right) => {
                let left_val = self.evaluate(*left)?;
                match operator {
                    TokenKind::Or => {
                        if self.is_truthy(left_val.clone()) {
                            Ok(left_val)
                        } else {
                            self.evaluate(*right)
                        }
                    },
                    TokenKind::And => {
                        if !self.is_truthy(left_val.clone()) {
                            Ok(left_val)
                        } else {
                            self.evaluate(*right)
                        }
                    },
                    _ => Err(format!("Unsupported logical operator: {:?}", operator)),
                }
            },
            Expr::Get(object_expr, name) => {
                let object = self.evaluate(*object_expr)?;
                match object {
                    KopiValue::Module(module_rc) => {
                        module_rc.borrow().get(&name).map_err(|_| format!("Module '{}' does not have member '{}'.", object, name))
                    },
                    _ => Err(format!("Only modules have properties. Got: {}", object)),
                }
            },
            Expr::Set(object_expr, name, value_expr) => {
                let object = self.evaluate(*object_expr)?;
                let value = self.evaluate(*value_expr)?;
                match object {
                    KopiValue::Module(module_rc) => {
                        module_rc.borrow_mut().define(name, value.clone());
                        Ok(value)
                    },
                    _ => Err(format!("Only modules can have properties set. Got: {}", object)),
                }
            },
            Expr::Array(elements_expr) => {
                let mut elements = Vec::new();
                for expr in elements_expr {
                    elements.push(self.evaluate(expr)?);
                }
                Ok(KopiValue::Array(Rc::new(RefCell::new(elements))))
            },
            Expr::Index(array_expr, index_expr) => {
                let array_val = self.evaluate(*array_expr)?;
                let index_val = self.evaluate(*index_expr)?;

                match (array_val, index_val) {
                    (KopiValue::Array(arr_rc), KopiValue::Number(index_f64)) => {
                        let index = index_f64 as usize;
                        let arr = arr_rc.borrow();
                        if index >= arr.len() {
                            Err(format!("Index {} out of bounds for array of length {}.", index, arr.len()))
                        } else {
                            Ok(arr[index].clone())
                        }
                    },
                    (KopiValue::String(s), KopiValue::Number(index_f64)) => {
                        let index = index_f64 as usize;
                        if index >= s.len() {
                            Err(format!("Index {} out of bounds for string of length {}.", index, s.len()))
                        } else {
                            Ok(KopiValue::String(s.chars().nth(index).unwrap().to_string()))
                        }
                    },
                    (arr, idx) => Err(format!("Cannot index type {} with type {}.", arr, idx)),
                }
            },
        }
    }

    fn is_truthy(&self, value: KopiValue) -> bool {
        match value {
            KopiValue::Nil => false,
            KopiValue::Boolean(b) => b,
            _ => true,
        }
    }
}
