use std::ops::Range;

pub type Span = Range<usize>;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Import,
    Dot,
    LBracket, RBracket, Not, NotEquals, GreaterThan, GreaterThanOrEqual, LessThanOrEqual, And, Or,
    While, Return, If, Else, Fun, Let, True, False, Number(f64), Identifier(String), String(String),
    LessThan, Plus, Minus, Star, Slash, Equals, EqualsEquals, Comma, LParen, RParen, LBrace, RBrace, Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token { pub kind: TokenType, pub span: Span }

pub fn tokenize(input: &str) -> Result<Vec<Token>, (String, Span)> {
    let mut tokens = Vec::new();
    let mut chars = input.char_indices().peekable();

    while let Some((start, ch)) = chars.next() {
        let mut end = start + 1;
        let kind = match ch {
            '.' => TokenType::Dot,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '<' => { if let Some((_, '=')) = chars.peek() { chars.next(); end += 1; TokenType::LessThanOrEqual } else { TokenType::LessThan } },
            '>' => { if let Some((_, '=')) = chars.peek() { chars.next(); end += 1; TokenType::GreaterThanOrEqual } else { TokenType::GreaterThan } },
            '=' => { if let Some((_, '=')) = chars.peek() { chars.next(); end += 1; TokenType::EqualsEquals } else { TokenType::Equals } },
            '!' => { if let Some((_, '=')) = chars.peek() { chars.next(); end += 1; TokenType::NotEquals } else { return Err(("Unexpected character: !".to_string(), start..end)); } },
            '-' => {
                if let Some((_, '-')) = chars.peek() {
                    chars.next();
                    if let Some((_, '[')) = chars.peek() {
                        chars.next();
                        'comment_loop: loop {
                            match chars.next() {
                                Some((_, ']')) => {
                                    if let Some((_, '-')) = chars.peek() {
                                        let mut ahead = chars.clone(); ahead.next();
                                        if let Some((_, '-')) = ahead.peek() {
                                            chars.next(); chars.next(); break 'comment_loop;
                                        }
                                    }
                                }
                                Some(_) => continue,
                                None => return Err(("Unclosed block comment".to_string(), start..start + 3)),
                            }
                        }
                    } else {
                        while let Some((_, next_ch)) = chars.peek() { if *next_ch == '\n' { break; } chars.next(); }
                    }
                    continue;
                } else { TokenType::Minus }
            },
            '+' => TokenType::Plus,
            '*' => TokenType::Star,
            '/' => TokenType::Slash,
            ',' => TokenType::Comma,
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            '"' => {
                let mut value = String::new();
                while let Some(&(_, inner_ch)) = chars.peek() { if inner_ch == '"' { break; } value.push(chars.next().unwrap().1); }
                end = chars.peek().map_or(input.len(), |(i, _)| *i) + 1;
                chars.next(); TokenType::String(value)
            },
            c if c.is_digit(10) => {
                let mut num_str = String::new();
                num_str.push(c);
                while let Some(&(_, next_ch)) = chars.peek() { if next_ch.is_digit(10) || next_ch == '.' { num_str.push(chars.next().unwrap().1); } else { break; } }
                end = chars.peek().map_or(input.len(), |(i, _)| *i);
                match num_str.parse::<f64>() { Ok(num) => TokenType::Number(num), Err(_) => return Err((format!("Invalid number format: '{}'", num_str), start..end)), }
            },
            c if c.is_alphabetic() => {
                let mut value = String::new();
                value.push(c);
                while let Some(&(_, next_ch)) = chars.peek() { if next_ch.is_alphanumeric() || next_ch == '_' { value.push(chars.next().unwrap().1); } else { break; } }
                end = chars.peek().map_or(input.len(), |(i, _)| *i);
                match value.as_str() {
                    "import" => TokenType::Import, "and" => TokenType::And, "or" => TokenType::Or, "not" => TokenType::Not, "while" => TokenType::While,
                    "return" => TokenType::Return, "if" => TokenType::If, "else" => TokenType::Else,
                    "true" => TokenType::True, "false" => TokenType::False, "let" => TokenType::Let,
                    "fun" => TokenType::Fun, _ => TokenType::Identifier(value),
                }
            },
            c if c.is_whitespace() => continue,
            _ => return Err((format!("Unexpected character: {}", ch), start..end)),
        };
        tokens.push(Token { kind, span: start..end });
    }
    tokens.push(Token { kind: TokenType::Eof, span: input.len()..input.len() });
    Ok(tokens)
}
