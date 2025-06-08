use crate::hydrac::parse::lexer::token::{Token, TokenType};

// ======================================
// LEXER
// ======================================

pub struct Lexer {
    input: Vec<char>,
    current: usize,
    line: usize,
    column: usize,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            current: 0,
            line: 1,
            column: 1,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut tokens = Vec::new();

        while !self.is_at_end() {
            self.skip_whitespace();

            if self.is_at_end() {
                break;
            }

            let start_line = self.line;
            let start_column = self.column;

            if let Some(token) = self.scan_token()? {
                tokens.push(Token {
                    token_type: token.0,
                    lexeme: token.1,
                    line: start_line,
                    column: start_column,
                });
            }
        }

        tokens.push(Token {
            token_type: TokenType::Eof,
            lexeme: String::new(),
            line: self.line,
            column: self.column,
        });

        Ok(tokens)
    }

    fn scan_token(&mut self) -> Result<Option<(TokenType, String)>, String> {
        let c = self.advance();

        match c {
            '(' => Ok(Some((TokenType::LeftParen, c.to_string()))), 
            ')' => Ok(Some((TokenType::RightParen, c.to_string()))),
            '{' => Ok(Some((TokenType::LeftBrace, c.to_string()))),
            '}' => Ok(Some((TokenType::RightBrace, c.to_string()))),
            '[' => Ok(Some((TokenType::LeftBracket, c.to_string()))),
            ']' => Ok(Some((TokenType::RightBracket, c.to_string()))),      
            ';' => Ok(Some((TokenType::Semicolon, c.to_string()))),
            ',' => Ok(Some((TokenType::Comma, c.to_string()))),
            
            ':' => {
                if self.match_char(':') {
                    Ok(Some((TokenType::DoubleColon, "::".to_string())))
                }
                else {
                    Ok(Some((TokenType::Colon, c.to_string())))
                }
            }
            '+' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::PlusAssign, "+=".to_string())))
                } 
                else if self.match_char('+') {
                    Ok(Some((TokenType::Increment, "++".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Plus, c.to_string())))
                }
            },
            '-' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::MinusAssign, "-=".to_string())))
                } 
                else if self.match_char('-') {
                    Ok(Some((TokenType::Decrement, "--".to_string())))
                } 
                else if self.match_char('>') {
                    Ok(Some((TokenType::Arrow, "->".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Minus, c.to_string())))
                }
            },
            '*' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::MultiplyAssign, "*=".to_string())))
                } 
                else if self.match_char('*') {
                    Ok(Some((TokenType::Power, "**".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Multiply, c.to_string())))
                }
            },
            '/' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::DivideAssign, "/=".to_string())))
                } 
                else if self.match_char('/') {
                    // Single line comment
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }

                    Ok(None)
                } 
                else if self.match_char('*') {
                    // Multi-line comment
                    while !self.is_at_end() {
                        if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance(); // consume '*'
                            self.advance(); // consume '/'

                            break;
                        }

                        if self.peek() == '\n' {
                            self.line += 1;
                            self.column = 0;
                        }

                        self.advance();
                    }

                    Ok(None)
                } 
                else {
                    Ok(Some((TokenType::Divide, c.to_string())))
                }
            },
            '%' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::ModuloAssign, "%=".to_string())))
                }
                else {
                    Ok(Some((TokenType::Modulo, c.to_string())))
                }
            }
            '=' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::Equal, "==".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Assign, c.to_string())))
                }
            },
            '!' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::NotEqual, "!=".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Not, c.to_string())))
                }
            },
            '<' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::LessEqual, "<=".to_string())))
                } 
                else {
                    Ok(Some((TokenType::LeftAngle, c.to_string())))
                }
            },
            '>' => {
                if self.match_char('=') {
                    Ok(Some((TokenType::GreaterEqual, ">=".to_string())))
                } 
                else {
                    Ok(Some((TokenType::RightAngle, c.to_string())))
                }
            },
            '&' => {
                if self.match_char('&') {
                    Ok(Some((TokenType::And, "&&".to_string())))
                } 
                else {
                    Ok(Some((TokenType::Reference, "&".to_string())))
                }
            },
            '?' => {
                Ok(Some((TokenType::Optional, "?".to_string())))
            }
            '|' => {
                if self.match_char('|') {
                    Ok(Some((TokenType::Or, "||".to_string())))
                } 
                else {
                    Ok(Some((TokenType::HeapPointerBar, "|".to_string())))
                }
            },
            '"' => self.scan_string(),
            '\'' => self.scan_char(),
            '\n' => {
                self.line += 1;
                self.column = 0;

                Ok(Some((TokenType::Newline, c.to_string())))
            },
            _ => {
                if c.is_ascii_digit() {
                    self.scan_number(c)
                } 
                else if c.is_alphabetic() || c == '_' {
                    self.scan_identifier(c)
                } 
                else {
                    Err(format!("Unexpected character '{}' at line {}, column {}", c, self.line, self.column))
                }
            }
        }
    }

    fn scan_string(&mut self) -> Result<Option<(TokenType, String)>, String> {
        let mut value = String::new();
        let mut lexeme = String::from("\"");

        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
                self.column = 0;
            }

            let c = self.advance();

            if c == '\\' && !self.is_at_end() {
                let escaped = self.advance();

                match escaped {
                    'n' => {
                        value.push('\n');
                        lexeme.push('\\');
                        lexeme.push('n');
                    }
                    't' => {
                        value.push('\t');
                        lexeme.push('\\');
                        lexeme.push('t');
                    },
                    'r' => {
                        value.push('\r');
                        lexeme.push('\\');
                        lexeme.push('r');
                    },
                    '\\' => {
                        value.push('\\');
                        lexeme.push('\\');
                        lexeme.push('\\');
                    },
                    '"' => {
                        value.push('"');
                        lexeme.push('\\');
                        lexeme.push('"');
                    },
                    '%' => {
                        value.push('%');
                        lexeme.push('\\');
                        lexeme.push('%');
                    },
                    _ => {
                        value.push(escaped);
                        lexeme.push('\\');
                        lexeme.push(escaped);
                    }
                }
            }
            else {
                value.push(c);
                lexeme.push(c);
            }
        }

        if self.is_at_end() {
            return Err("Unterminated string".to_string());
        }

        self.advance();
        lexeme.push('"');

        Ok(Some((TokenType::StringLiteral(value), lexeme)))
    }

    fn scan_char(&mut self) -> Result<Option<(TokenType, String)>, String> {
        if self.is_at_end() {
            return Err("Unterminated character literal".to_string());
        }
        
        let c = self.advance();
        let mut lexeme = String::from("'");
        lexeme.push(c);
        
        if self.peek() != '\'' {
            return Err("Character literal must contain exactly one character".to_string());
        }
        
        self.advance(); // consume closing '
        lexeme.push('\'');
        
        Ok(Some((TokenType::CharLiteral(c), lexeme)))
    }

    fn scan_number(&mut self, first_digit: char) -> Result<Option<(TokenType, String)>, String> {
        let mut lexeme = String::new();
        lexeme.push(first_digit);
        
        while self.peek().is_ascii_digit() {
            lexeme.push(self.advance());
        }
        
        // Check for decimal point
        if self.peek() == '.' && self.peek_next().is_ascii_digit() {
            lexeme.push(self.advance()); // consume '.'
            
            while self.peek().is_ascii_digit() {
                lexeme.push(self.advance());
            }
            
            let value: f64 = lexeme.parse().map_err(|_| "Invalid float literal")?;
            Ok(Some((TokenType::FloatLiteral(value), lexeme)))
        } else {
            let value: i64 = lexeme.parse().map_err(|_| "Invalid integer literal")?;
            Ok(Some((TokenType::IntLiteral(value), lexeme)))
        }
    }
    
    fn scan_identifier(&mut self, first_char: char) -> Result<Option<(TokenType, String)>, String> {
        let mut lexeme = String::new();
        lexeme.push(first_char);
        
        while self.peek().is_alphanumeric() || self.peek() == '_' {
            lexeme.push(self.advance());
        }
        
        let token_type = self.get_keyword_or_identifier(&lexeme);
        Ok(Some((token_type, lexeme)))
    }

    fn get_keyword_or_identifier(&self, text: &str) -> TokenType {
        match text {
            "null" => TokenType::Null,
            "let" => TokenType::Let,
            "true" => TokenType::BoolLiteral(true),
            "false" => TokenType::BoolLiteral(false),
            "if" => TokenType::If,
            "else" => TokenType::Else,
            "in" => TokenType::In,
            "is" => TokenType::Is,
            "break" => TokenType::Break,
            "return" => TokenType::Return,
            "skip" => TokenType::Skip,
            "struct" => TokenType::Struct,
            "fn" => TokenType::Function,
            "while" => TokenType::While,
            "for" => TokenType::For,
            "forEach" => TokenType::ForEach,
            "const" => TokenType::Const,
            "i32" => TokenType::I32,
            "i64" => TokenType::I64,
            "f32" => TokenType::F32,
            "f64" => TokenType::F64,
            "string" => TokenType::StringType,
            "char" => TokenType::CharType,
            "boolean" => TokenType::BooleanType,
            "void" => TokenType::VoidType,
             _ => TokenType::Identifier(text.to_string()),
        }
    }

    fn advance(&mut self) -> char {
        let c = self.input[self.current];
        self.current += 1;
        self.column += 1;
        
        c
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.input[self.current] != expected {
            false
        }
        else {
            self.current += 1;
            self.column += 1;

            true
        }
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        }
        else {
            self.input[self.current]
        }
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.input.len() {
            '\0'
        }
        else {
            self.input[self.current + 1]
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.input.len()
    }

    fn skip_whitespace(&mut self) {
        while !self.is_at_end() {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                },
                _ => break,
            }
        }
    }
}