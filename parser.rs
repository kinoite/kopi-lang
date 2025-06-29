// src/parser.rs

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
}

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralValue { String(String), Boolean(bool), Number(f64) }

#[derive(Debug, PartialEq, Clone)]
pub struct Statement { pub kind: StatementKind, pub span: Span }

#[derive(Debug, PartialEq, Clone)]
pub enum StatementKind {
    Let { name: Token, value: Expression },
    If { condition: Expression, consequence: Vec<Statement>, alternative: Option<Vec<Statement>> },
    While { condition: Expression, body: Vec<Statement> },
    FunctionDeclaration { name: Token, params: Vec<Token>, body: Vec<Statement> },
    Return { keyword: Token, value: Option<Expression> },
    ExpressionStatement(Expression),
}


pub struct Parser<'a> {
    tokens: &'a [Token],
    position: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self { Parser { tokens, position: 0 } }
    fn current(&self) -> &Token { self.tokens.get(self.position).unwrap_or_else(|| self.tokens.last().unwrap()) }
    fn peek(&self) -> &TokenType { &self.current().kind }
    fn advance(&mut self) { if self.position < self.tokens.len() - 1 { self.position += 1; } }

    fn parse_expression(&mut self) -> Result<Expression, (String, Span)> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> Result<Expression, (String, Span)> {
        let left = self.parse_logic_or_expression()?;
        if self.peek() == &TokenType::Equals {
            self.advance(); // consume '='
            let value = self.parse_assignment_expression()?;
            if let ExpressionKind::Variable(name) = left.kind {
                let name_token = Token { kind: TokenType::Identifier(name), span: left.span.clone() };
                return Ok(Expression {
                    kind: ExpressionKind::Assignment { name: name_token, value: Box::new(value) },
                    span: left.span.start..self.current().span.end,
                });
            }
            return Err(("Invalid assignment target.".to_string(), left.span));
        }
        Ok(left)
    }
    
    fn parse_logic_or_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_logic_and_expression()?;
        while self.peek() == &TokenType::Or {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_logic_and_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }

    fn parse_logic_and_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_equality_expression()?;
        while self.peek() == &TokenType::And {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_equality_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }

    fn parse_equality_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_comparison_expression()?;
        while matches!(self.peek(), TokenType::EqualsEquals | TokenType::NotEquals) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_comparison_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }
    
    fn parse_comparison_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_term_expression()?;
        while matches!(self.peek(), TokenType::LessThan | TokenType::LessThanOrEqual | TokenType::GreaterThan | TokenType::GreaterThanOrEqual) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_term_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }

    fn parse_term_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_factor_expression()?;
        while matches!(self.peek(), TokenType::Plus | TokenType::Minus) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_factor_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }

    fn parse_factor_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_unary_expression()?;
        while matches!(self.peek(), TokenType::Star | TokenType::Slash) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_unary_expression()?;
            let span = expr.span.start..right.span.end;
            expr = Expression { kind: ExpressionKind::Infix { left: Box::new(expr), operator, right: Box::new(right) }, span };
        }
        Ok(expr)
    }

    fn parse_unary_expression(&mut self) -> Result<Expression, (String, Span)> {
        if matches!(self.peek(), TokenType::Not | TokenType::Minus) {
            let operator = self.current().clone();
            self.advance();
            let right = self.parse_unary_expression()?;
            let span = operator.span.start..right.span.end;
            return Ok(Expression { kind: ExpressionKind::Unary { operator, right: Box::new(right) }, span });
        }
        self.parse_call_expression()
    }
    
    fn parse_call_expression(&mut self) -> Result<Expression, (String, Span)> {
        let mut expr = self.parse_primary_expression()?;
        if self.peek() == &TokenType::LParen { expr = self.finish_parsing_call(expr)?; }
        Ok(expr)
    }

    fn finish_parsing_call(&mut self, callee: Expression) -> Result<Expression, (String, Span)> {
        self.advance();
        let mut args = Vec::new();
        if self.peek() != &TokenType::RParen { args.push(self.parse_expression()?); }
        if self.peek() != &TokenType::RParen { return Err(("Expected ')' after function arguments.".to_string(), self.current().span.clone())); }
        self.advance();
        let span = callee.span.start..self.current().span.end;
        Ok(Expression { span, kind: ExpressionKind::FunctionCall { name: Box::new(callee), args } })
    }

    fn parse_primary_expression(&mut self) -> Result<Expression, (String, Span)> {
        let token = self.current().clone();
        let expr = match token.kind {
            TokenType::Identifier(name) => Expression { kind: ExpressionKind::Variable(name), span: token.span },
            TokenType::String(value) => Expression { kind: ExpressionKind::Literal(LiteralValue::String(value)), span: token.span },
            TokenType::True => Expression { kind: ExpressionKind::Literal(LiteralValue::Boolean(true)), span: token.span },
            TokenType::False => Expression { kind: ExpressionKind::Literal(LiteralValue::Boolean(false)), span: token.span },
            TokenType::Number(value) => Expression { kind: ExpressionKind::Literal(LiteralValue::Number(value)), span: token.span },
            _ => return Err((format!("Expected an expression, found {:?}", token.kind), token.span)),
        };
        self.advance(); Ok(expr)
    }

    fn parse_statement(&mut self) -> Result<Statement, (String, Span)> {
        match self.peek() {
            TokenType::Let => self.parse_let_statement(),
            TokenType::If => self.parse_if_statement(),
            TokenType::While => self.parse_while_statement(),
            TokenType::Fun => self.parse_function_declaration(),
            TokenType::Return => self.parse_return_statement(),
            _ => { 
                let expr = self.parse_expression()?; 
                let span = expr.span.clone(); 
                Ok(Statement { kind: StatementKind::ExpressionStatement(expr), span }) 
            }
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, (String, Span)> {
        let mut statements = Vec::new();
        while self.peek() != &TokenType::Eof {
            statements.push(self.parse_statement()?);
        }
        Ok(statements)
    }
    
    fn parse_block(&mut self) -> Result<Vec<Statement>, (String, Span)> { 
        let mut statements = Vec::new(); 
        while self.peek() != &TokenType::RBrace && self.peek() != &TokenType::Eof { 
            statements.push(self.parse_statement()?); 
        } 
        if self.peek() != &TokenType::RBrace { 
            return Err(("Expected '}' to close block.".to_string(), self.current().span.clone())); 
        } 
        self.advance(); 
        Ok(statements) 
    }

    fn parse_let_statement(&mut self) -> Result<Statement, (String, Span)> { 
        let let_token = self.current().clone(); 
        self.advance(); 
        let name_token = self.current().clone(); 
        if !matches!(&name_token.kind, TokenType::Identifier(_)) { return Err(("Expected variable name.".to_string(), name_token.span)); } 
        self.advance(); 
        if self.peek() != &TokenType::Equals { return Err(("Expected '='.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        let value = self.parse_expression()?; 
        Ok(Statement { kind: StatementKind::Let { name: name_token, value: value.clone() }, span: let_token.span.start..value.span.end }) 
    }

    fn parse_if_statement(&mut self) -> Result<Statement, (String, Span)> { 
        let if_token = self.current().clone(); 
        self.advance(); 
        let condition = self.parse_expression()?; 
        if self.peek() != &TokenType::LBrace { return Err(("Expected '{'.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        let consequence = self.parse_block()?; 
        let mut alternative = None; 
        if self.peek() == &TokenType::Else { 
            self.advance(); 
            if self.peek() != &TokenType::LBrace { return Err(("Expected '{' after else.".to_string(), self.current().span.clone())); } 
            self.advance(); 
            alternative = Some(self.parse_block()?); 
        } 
        Ok(Statement { kind: StatementKind::If { condition, consequence, alternative }, span: if_token.span.start..self.current().span.end }) 
    }

    fn parse_while_statement(&mut self) -> Result<Statement, (String, Span)> { 
        let while_token = self.current().clone(); 
        self.advance(); 
        let condition = self.parse_expression()?; 
        if self.peek() != &TokenType::LBrace { return Err(("Expected '{' after while condition.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        let body = self.parse_block()?; 
        Ok(Statement { kind: StatementKind::While { condition, body }, span: while_token.span.start..self.current().span.end }) 
    }

    fn parse_function_declaration(&mut self) -> Result<Statement, (String, Span)> { 
        let fun_token = self.current().clone(); 
        self.advance(); 
        let name = self.current().clone(); 
        if !matches!(self.peek(), TokenType::Identifier(_)) { return Err(("Expected function name.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        if self.peek() != &TokenType::LParen { return Err(("Expected '(' after function name.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        let mut params = Vec::new(); 
        while self.peek() != &TokenType::RParen { 
            if !matches!(self.peek(), TokenType::Identifier(_)) { return Err(("Expected parameter name.".to_string(), self.current().span.clone())); } 
            params.push(self.current().clone()); 
            self.advance(); 
            if self.peek() == &TokenType::Comma { self.advance(); } 
        } 
        self.advance(); 
        if self.peek() != &TokenType::LBrace { return Err(("Expected '{' before function body.".to_string(), self.current().span.clone())); } 
        self.advance(); 
        let body = self.parse_block()?; 
        Ok(Statement { kind: StatementKind::FunctionDeclaration { name, params, body }, span: fun_token.span.start..self.current().span.end }) 
    }

    fn parse_return_statement(&mut self) -> Result<Statement, (String, Span)> { 
        let keyword = self.current().clone(); 
        self.advance(); 
        let mut value = None; 
        if self.peek() != &TokenType::RBrace && self.peek() != &TokenType::Eof { 
            value = Some(self.parse_expression()?); 
        } 
        Ok(Statement { span: keyword.span.start..self.current().span.end, kind: StatementKind::Return { keyword, value } }) 
    }
}
