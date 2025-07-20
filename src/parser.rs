use crate::lexer::{Token, TokenType, Span};

#[derive(Debug, PartialEq, Clone)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionKind {
    Assignment { name: Token, value: Box<Expression> },
    Variable(String),
    Literal(LiteralValue),
    Unary { operator: Token, right: Box<Expression> },
    Infix { left: Box<Expression>, operator: Token, right: Box<Expression> },
    FunctionCall { name: Box<Expression>, args: Vec<Expression> },
    ArrayLiteral { elements: Vec<Expression> },
    Index { left: Box<Expression>, index: Box<Expression> },
    PropertyAccess { object: Box<Expression>, property: Token },
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue {
    String(String),
    Boolean(bool),
    Number(f64),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Statement {
    pub kind: StatementKind,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Let { name: Token, value: Expression },
    If { condition: Expression, consequence: Vec<Statement>, alternative: Option<Vec<Statement>> },
    While { condition: Expression, body: Vec<Statement> },
    FunctionDeclaration { name: Token, params: Vec<Token>, body: Vec<Statement> },
    Return { keyword: Token, value: Option<Expression> },
    Import { module_name: Token },
    ExpressionStatement(Expression),
}

pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
    errors: Vec<(String, Span)>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, position: 0, errors: Vec::new() }
    }

    pub fn parse(&mut self) -> (Vec<Statement>, Vec<(String, Span)>) {
        let mut statements = Vec::new();
        while self.peek() != &TokenType::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        (statements, self.errors.clone())
    }

    fn current(&self) -> &Token {
        self.tokens.get(self.position).unwrap_or_else(|| self.tokens.last().unwrap())
    }

    fn peek(&self) -> &TokenType {
        &self.current().kind
    }

    fn advance(&mut self) {
        if self.position < self.tokens.len() - 1 {
            self.position += 1;
        }
    }
    
    fn synchronize(&mut self) {
        self.advance();
        while self.peek() != &TokenType::Eof {
            if matches!(self.peek(), TokenType::Let | TokenType::Fun | TokenType::If | TokenType::While | TokenType::Return | TokenType::Import) {
                return;
            }
            self.advance();
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let result = match self.peek() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::Fun => self.parse_function_declaration(),
            TokenType::If => self.parse_if_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Return => self.parse_return_statement(),
            TokenType::Import => self.parse_import_statement(),
            _ => {
                let expr = self.parse_expression()?;
                let span = expr.span.clone();
                Some(Statement { kind: StatementKind::ExpressionStatement(expr), span })
            }
        };

        if result.is_none() {
            self.synchronize();
        }
        result
    }
    
    fn parse_import_statement(&mut self) -> Option<Statement> {
        let keyword = self.current().clone();
        self.advance();
        let module_name = self.current().clone();
        if !matches!(self.peek(), TokenType::Identifier(_)) {
            self.errors.push(("Expected module name after 'import'.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let span = keyword.span.start..module_name.span.end;
        Some(Statement { kind: StatementKind::Import { module_name }, span })
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let let_token = self.current().clone();
        self.advance();

        let name_token = self.current().clone();
        if !matches!(&name_token.kind, TokenType::Identifier(_)) {
            self.errors.push(("Expected variable name.".to_string(), name_token.span));
            return None;
        }
        self.advance();

        if self.peek() != &TokenType::Equals {
            self.errors.push(("Expected '=' after variable name.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();

        let value = self.parse_expression()?;
        let span = let_token.span.start..value.span.end;
        Some(Statement { kind: StatementKind::Let { name: name_token, value }, span })
    }

    fn parse_if_statement(&mut self) -> Option<Statement> {
        let if_token = self.current().clone();
        self.advance();
        let condition = self.parse_expression()?;
        if self.peek() != &TokenType::LBrace {
            self.errors.push(("Expected '{' after if condition.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let consequence = self.parse_block()?;
        let mut alternative = None;
        if self.peek() == &TokenType::Else {
            self.advance();
            if self.peek() != &TokenType::LBrace {
                self.errors.push(("Expected '{' after else.".to_string(), self.current().span.clone()));
                return None;
            }
            self.advance();
            alternative = self.parse_block();
        }
        let span = if_token.span.start..self.current().span.end;
        Some(Statement { kind: StatementKind::If { condition, consequence, alternative }, span })
    }

    fn parse_while_statement(&mut self) -> Option<Statement> {
        let while_token = self.current().clone();
        self.advance();
        let condition = self.parse_expression()?;
        if self.peek() != &TokenType::LBrace {
            self.errors.push(("Expected '{' after while condition.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let body = self.parse_block()?;
        let span = while_token.span.start..self.current().span.end;
        Some(Statement { kind: StatementKind::While { condition, body }, span })
    }

    fn parse_function_declaration(&mut self) -> Option<Statement> {
        let fun_token = self.current().clone();
        self.advance();
        let name = self.current().clone();
        if !matches!(self.peek(), TokenType::Identifier(_)) {
            self.errors.push(("Expected function name.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        if self.peek() != &TokenType::LParen {
            self.errors.push(("Expected '(' after function name.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let mut params = Vec::new();
        if self.peek() != &TokenType::RParen {
            loop {
                if !matches!(self.peek(), TokenType::Identifier(_)) {
                    self.errors.push(("Expected parameter name.".to_string(), self.current().span.clone()));
                    break;
                }
                params.push(self.current().clone());
                self.advance();
                if self.peek() != &TokenType::Comma {
                    break;
                }
                self.advance();
            }
        }
        if self.peek() != &TokenType::RParen {
            self.errors.push(("Expected ')' after parameters.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        if self.peek() != &TokenType::LBrace {
            self.errors.push(("Expected '{' before function body.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let body = self.parse_block()?;
        let span = fun_token.span.start..self.current().span.end;
        Some(Statement { kind: StatementKind::FunctionDeclaration { name, params, body }, span })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let keyword = self.current().clone();
        self.advance();
        let mut value = None;
        if self.peek() != &TokenType::RBrace && self.peek() != &TokenType::Eof {
            value = self.parse_expression();
        }
        let span = keyword.span.start..self.current().span.end;
        Some(Statement { kind: StatementKind::Return { keyword, value }, span })
    }

    fn parse_block(&mut self) -> Option<Vec<Statement>> {
        let mut statements = Vec::new();
        while self.peek() != &TokenType::RBrace && self.peek() != &TokenType::Eof {
            if let Some(stmt) = self.parse_statement() {
                statements.push(stmt);
            }
        }
        if self.peek() != &TokenType::RBrace {
            self.errors.push(("Expected '}' to close block.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        Some(statements)
    }

    fn parse_expression(&mut self) -> Option<Expression> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Option<Expression> {
        let left = self.parse_logic_or_expression()?;
        if self.peek() == &TokenType::Equals {
            self.advance();
            let value = self.parse_assignment_expression()?;
            if let ExpressionKind::Variable(name) = left.kind {
                let name_token = Token { kind: TokenType::Identifier(name), span: left.span.clone() };
                let span = left.span.start..self.current().span.end;
                return Some(Expression {
                    kind: ExpressionKind::Assignment { name: name_token, value: Box::new(value) },
                    span,
                });
            }
            self.errors.push(("Invalid assignment target.".to_string(), left.span));
            return None;
        }
        Some(left)
    }

    fn parse_logic_or_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_logic_and_expression()?;
        while self.peek() == &TokenType::Or {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_logic_and_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_logic_and_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_equality_expression()?;
        while self.peek() == &TokenType::And {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_equality_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_equality_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_comparison_expression()?;
        while matches!(self.peek(), TokenType::EqualsEquals | TokenType::NotEquals) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_comparison_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_comparison_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_term_expression()?;
        while matches!(self.peek(), TokenType::LessThan | TokenType::LessThanOrEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_term_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_term_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_factor_expression()?;
        while matches!(self.peek(), TokenType::Plus | TokenType::Minus) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_factor_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_factor_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_unary_expression()?;
        while matches!(self.peek(), TokenType::Star | TokenType::Slash) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_unary_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Some(expr)
    }

    fn parse_unary_expression(&mut self) -> Option<Expression> {
        if matches!(self.peek(), TokenType::Not | TokenType::Minus) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_unary_expression()?;
            let span = operator.span.start..right.span.end;
            return Some(Expression { kind: ExpressionKind::Unary { operator, right: Box::new(right) }, span });
        }
        self.parse_access_expression()
    }
    
    fn parse_access_expression(&mut self) -> Option<Expression> {
        let mut expr = self.parse_primary_expression()?;
        loop {
            match self.peek() {
                TokenType::LParen => {
                    expr = self.finish_parsing_call(expr)?;
                }
                TokenType::LBracket => {
                    self.advance();
                    let index_expr = self.parse_expression()?;
                    if self.peek() != &TokenType::RBracket {
                        self.errors.push(("Expected ']' after index.".to_string(), self.current().span.clone()));
                        return None;
                    }
                    let span = expr.span.start..self.current().span.end;
                    self.advance();
                    expr = Expression { kind: ExpressionKind::Index { left: Box::new(expr), index: Box::new(index_expr) }, span };
                }
                TokenType::Dot => {
                    self.advance();
                    let property = self.current().clone();
                    if !matches!(self.peek(), TokenType::Identifier(_)) {
                        self.errors.push(("Expected property name after '.'.".to_string(), self.current().span.clone()));
                        return None;
                    }
                    self.advance();
                    let span = expr.span.start..property.span.end;
                    expr = Expression { kind: ExpressionKind::PropertyAccess { object: Box::new(expr), property }, span };
                }
                _ => break,
            }
        }
        Some(expr)
    }

    fn finish_parsing_call(&mut self, callee: Expression) -> Option<Expression> {
        self.advance();
        let mut args = Vec::new();
        if self.peek() != &TokenType::RParen {
            args.push(self.parse_expression()?);
            while self.peek() == &TokenType::Comma {
                self.advance();
                args.push(self.parse_expression()?);
            }
        }
        if self.peek() != &TokenType::RParen {
            self.errors.push(("Expected ')' after function arguments.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        let span = callee.span.start..self.current().span.end;
        Some(Expression { span, kind: ExpressionKind::FunctionCall { name: Box::new(callee), args } })
    }

    fn parse_primary_expression(&mut self) -> Option<Expression> {
        if self.peek() == &TokenType::LBracket {
            return self.parse_array_literal();
        }
        
        if self.peek() == &TokenType::LParen {
            self.advance();
            let expr = self.parse_expression();
            if self.peek() != &TokenType::RParen {
                self.errors.push(("Expected ')' after expression in parentheses.".to_string(), self.current().span.clone()));
                return None;
            }
            self.advance();
            return expr;
        }

        let token = self.current().clone();
        let expr = match token.kind {
            TokenType::Identifier(name) => Some(Expression { kind: ExpressionKind::Variable(name), span: token.span }),
            TokenType::String(value) => Some(Expression { kind: ExpressionKind::Literal(LiteralValue::String(value)), span: token.span }),
            TokenType::True => Some(Expression { kind: ExpressionKind::Literal(LiteralValue::Boolean(true)), span: token.span }),
            TokenType::False => Some(Expression { kind: ExpressionKind::Literal(LiteralValue::Boolean(false)), span: token.span }),
            TokenType::Number(value) => Some(Expression { kind: ExpressionKind::Literal(LiteralValue::Number(value)), span: token.span }),
            _ => {
                self.errors.push((format!("Expected an expression, found {:?}", token.kind), token.span));
                return None;
            }
        };
        self.advance();
        expr
    }
    
    fn parse_array_literal(&mut self) -> Option<Expression> {
        let start_token = self.current().clone();
        self.advance();
        let mut elements = Vec::new();
        if self.peek() == &TokenType::RBracket {
            self.advance();
            return Some(Expression {
                kind: ExpressionKind::ArrayLiteral { elements },
                span: start_token.span.start..self.current().span.end,
            });
        }
        elements.push(self.parse_expression()?);
        while self.peek() == &TokenType::Comma {
            self.advance();
            elements.push(self.parse_expression()?);
        }
        if self.peek() != &TokenType::RBracket {
            self.errors.push(("Expected ']' after array elements.".to_string(), self.current().span.clone()));
            return None;
        }
        self.advance();
        Some(Expression {
            kind: ExpressionKind::ArrayLiteral { elements },
            span: start_token.span.start..self.current().span.end,
        })
    }
}
