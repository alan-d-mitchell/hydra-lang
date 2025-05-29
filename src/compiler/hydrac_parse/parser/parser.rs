use crate::compiler::hydrac_parse::lexer::token::{Token, TokenType};
use crate::compiler::hydrac_parse::parser::ast::*;

pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, current: 0 }
    }
    
    pub fn parse(&mut self) -> Result<Program, String> {
        let mut items = Vec::new();
        
        while !self.is_at_end() {
            // Skip newlines at top level
            if self.check(&TokenType::Newline) {
                self.advance();
                continue;
            }
            
            items.push(self.parse_item()?);
        }
        
        Ok(Program { items })
    }
    
    fn parse_item(&mut self) -> Result<Item, String> {
        if self.check(&TokenType::Global) {
            Ok(Item::GlobalVariable(self.parse_global_variable()?))
        } else if self.check(&TokenType::Func) {
            Ok(Item::Function(self.parse_function()?))
        } else {
            Err(format!("Expected 'global' or 'func' at line {}", self.peek().line))
        }
    }
    
    fn parse_global_variable(&mut self) -> Result<GlobalVariable, String> {
        self.consume(&TokenType::Global, "Expected 'global'")?;
        
        let is_const = if self.check(&TokenType::Const) {
            self.advance();
            true
        } else {
            false
        };
        
        let var_type = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name")?;
        
        let initializer = if self.check(&TokenType::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after global variable declaration")?;
        
        Ok(GlobalVariable {
            is_const,
            var_type,
            name,
            initializer,
        })
    }
    
    fn parse_function(&mut self) -> Result<Function, String> {
        self.consume(&TokenType::Func, "Expected 'func'")?;
        let name = self.consume_identifier("Expected function name")?;
        
        self.consume(&TokenType::LeftParen, "Expected '(' after function name")?;
        
        let mut parameters = Vec::new();
        if !self.check(&TokenType::RightParen) {
            loop {
                let param_type = self.parse_type()?;
                let param_name = self.consume_identifier("Expected parameter name")?;
                parameters.push(Parameter {
                    name: param_name,
                    param_type,
                });
                
                if !self.check(&TokenType::Comma) {
                    break;
                }
                self.advance();
            }
        }
        
        self.consume(&TokenType::RightParen, "Expected ')' after parameters")?;
        self.consume(&TokenType::Arrow, "Expected '->' after parameters")?;
        
        let return_type = self.parse_type()?;
        let body = self.parse_block()?;
        
        Ok(Function {
            name,
            parameters,
            return_type,
            body,
        })
    }
    
    fn parse_type(&mut self) -> Result<Type, String> {
        let base_type = match &self.peek().token_type {
            TokenType::IntType => {
                self.advance();
                Type::Int
            },
            TokenType::FloatType => {
                self.advance();
                Type::Float
            },
            TokenType::StringType => {
                self.advance();
                Type::String
            },
            TokenType::CharType => {
                self.advance();
                Type::Char
            },
            TokenType::BooleanType => {
                self.advance();
                Type::Boolean
            },
            TokenType::VoidType => {
                self.advance();
                Type::Void
            },
            _ => return Err(format!("Expected type at line {}", self.peek().line)),
        };
        
        if self.check(&TokenType::LeftBracket) {
            self.advance();
            self.consume(&TokenType::RightBracket, "Expected ']' after '['")?;
            Ok(Type::Array(Box::new(base_type)))
        } else {
            Ok(base_type)
        }
    }
    
    fn parse_block(&mut self) -> Result<Block, String> {
        self.consume(&TokenType::LeftBrace, "Expected '{'")?;
        
        let mut statements = Vec::new();
        
        while !self.check(&TokenType::RightBrace) && !self.is_at_end() {
            // Skip newlines in blocks
            if self.check(&TokenType::Newline) {
                self.advance();
                continue;
            }
            
            statements.push(self.parse_statement()?);
        }
        
        self.consume(&TokenType::RightBrace, "Expected '}'")?;
        
        Ok(Block { statements })
    }
    
    fn parse_statement(&mut self) -> Result<Statement, String> {
        if self.check(&TokenType::Const) || self.check(&TokenType::IntType) || 
           self.check(&TokenType::FloatType) || self.check(&TokenType::StringType) ||
           self.check(&TokenType::CharType) || self.check(&TokenType::BooleanType) {
            Ok(Statement::VarDecl(self.parse_var_decl()?))
        } else if self.check(&TokenType::If) {
            Ok(Statement::If(self.parse_if_stmt()?))
        } else if self.check(&TokenType::While) {
            Ok(Statement::While(self.parse_while_stmt()?))
        } else if self.check(&TokenType::For) {
            Ok(Statement::For(self.parse_for_stmt()?))
        } else if self.check(&TokenType::ForEach) {
            Ok(Statement::ForEach(self.parse_foreach_stmt()?))
        } else if self.check(&TokenType::Return) {
            Ok(Statement::Return(self.parse_return_stmt()?))
        } else if self.check(&TokenType::Break) {
            Ok(Statement::Break(self.parse_break_stmt()?))
        } else if self.check(&TokenType::Skip) {
            self.advance();
            self.consume(&TokenType::Semicolon, "Expected ';' after 'skip'")?;
            Ok(Statement::Skip(SkipStmt))
        } else {
            let expr = self.parse_expression()?;
            self.consume(&TokenType::Semicolon, "Expected ';' after expression")?;
            Ok(Statement::Expression(expr))
        }
    }
    
    fn parse_var_decl(&mut self) -> Result<VarDecl, String> {
        let is_const = if self.check(&TokenType::Const) {
            self.advance();
            true
        } else {
            false
        };
        
        let var_type = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name")?;
        
        let initializer = if self.check(&TokenType::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after variable declaration")?;
        
        Ok(VarDecl {
            is_const,
            var_type,
            name,
            initializer,
        })
    }
    
    fn parse_if_stmt(&mut self) -> Result<IfStmt, String> {
        self.consume(&TokenType::If, "Expected 'if'")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'if'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after if condition")?;
        
        let then_branch = self.parse_block()?;
        
        let else_branch = if self.check(&TokenType::Else) {
            self.advance();
            if self.check(&TokenType::If) {
                Some(Box::new(Statement::If(self.parse_if_stmt()?)))
            } else {
                let else_block = self.parse_block()?;
                // Convert block to a statement - we need a proper block statement type
                // For now, we'll use the first statement in the block or create an empty expression
                if else_block.statements.is_empty() {
                    None
                } else {
                    Some(Box::new(else_block.statements[0].clone()))
                }
            }
        } else {
            None
        };
        
        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
        })
    }
    
    fn parse_while_stmt(&mut self) -> Result<WhileStmt, String> {
        self.consume(&TokenType::While, "Expected 'while'")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'while'")?;
        let condition = self.parse_expression()?;
        self.consume(&TokenType::RightParen, "Expected ')' after while condition")?;
        
        let body = self.parse_block()?;
        
        Ok(WhileStmt { condition, body })
    }
    
    fn parse_for_stmt(&mut self) -> Result<ForStmt, String> {
        self.consume(&TokenType::For, "Expected 'for'")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'for'")?;
        
        // Parse initializer (variable declaration or expression, no semicolon expected)
        let initializer = if self.check(&TokenType::Semicolon) {
            None
        } else {
            // Parse as variable declaration or expression, but don't expect semicolon
            if self.check(&TokenType::Const) || self.check(&TokenType::IntType) || 
               self.check(&TokenType::FloatType) || self.check(&TokenType::StringType) ||
               self.check(&TokenType::CharType) || self.check(&TokenType::BooleanType) {
                Some(Box::new(Statement::VarDecl(self.parse_for_var_decl()?)))
            } else {
                Some(Box::new(Statement::Expression(self.parse_expression()?)))
            }
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after for initializer")?;
        
        let condition = if self.check(&TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.consume(&TokenType::Semicolon, "Expected ';' after for condition")?;
        
        let increment = if self.check(&TokenType::RightParen) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        
        self.consume(&TokenType::RightParen, "Expected ')' after for clauses")?;
        
        let body = self.parse_block()?;
        
        Ok(ForStmt {
            initializer,
            condition,
            increment,
            body,
        })
    }
    
    fn parse_for_var_decl(&mut self) -> Result<VarDecl, String> {
        let is_const = if self.check(&TokenType::Const) {
            self.advance();
            true
        } else {
            false
        };
        
        let var_type = self.parse_type()?;
        let name = self.consume_identifier("Expected variable name")?;
        
        let initializer = if self.check(&TokenType::Assign) {
            self.advance();
            Some(self.parse_expression()?)
        } else {
            None
        };
        
        // Note: No semicolon consumption here - that's handled by the for loop parser
        
        Ok(VarDecl {
            is_const,
            var_type,
            name,
            initializer,
        })
    }
    
    fn parse_foreach_stmt(&mut self) -> Result<ForEachStmt, String> {
        self.consume(&TokenType::ForEach, "Expected 'forEach'")?;
        self.consume(&TokenType::LeftParen, "Expected '(' after 'forEach'")?;
        
        let var_type = self.parse_type()?;
        let var_name = self.consume_identifier("Expected variable name")?;
        self.consume(&TokenType::In, "Expected 'in' after forEach variable")?;
        let iterable = self.parse_expression()?;
        
        self.consume(&TokenType::RightParen, "Expected ')' after forEach")?;
        
        let body = self.parse_block()?;
        
        Ok(ForEachStmt {
            var_type,
            var_name,
            iterable,
            body,
        })
    }
    
    fn parse_return_stmt(&mut self) -> Result<ReturnStmt, String> {
        self.consume(&TokenType::Return, "Expected 'return'")?;
        
        let value = if self.check(&TokenType::Semicolon) {
            None
        } else {
            Some(self.parse_expression()?)
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after return")?;
        
        Ok(ReturnStmt { value })
    }
    
    fn parse_break_stmt(&mut self) -> Result<BreakStmt, String> {
        self.consume(&TokenType::Break, "Expected 'break'")?;
        
        let condition = if self.check(&TokenType::If) {
            self.advance();
            self.consume(&TokenType::LeftParen, "Expected '(' after 'break if'")?;
            let cond = self.parse_expression()?;
            self.consume(&TokenType::RightParen, "Expected ')' after break if condition")?;
            Some(cond)
        } else {
            None
        };
        
        self.consume(&TokenType::Semicolon, "Expected ';' after break")?;
        
        Ok(BreakStmt { condition })
    }
    
    fn parse_expression(&mut self) -> Result<Expression, String> {
        self.parse_assignment()
    }
    
    fn parse_assignment(&mut self) -> Result<Expression, String> {
        let expr = self.parse_logical_or()?;
        
        if self.match_any(&[TokenType::Assign, TokenType::PlusAssign, TokenType::MinusAssign, 
                           TokenType::MultiplyAssign, TokenType::DivideAssign]) {
            let operator = match self.previous().token_type {
                TokenType::Assign => BinaryOperator::Assign,
                TokenType::PlusAssign => BinaryOperator::AddAssign,
                TokenType::MinusAssign => BinaryOperator::SubAssign,
                TokenType::MultiplyAssign => BinaryOperator::MulAssign,
                TokenType::DivideAssign => BinaryOperator::DivAssign,
                _ => unreachable!(),
            };
            let right = self.parse_assignment()?; // Right-associative
            return Ok(Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            }));
        }
        
        Ok(expr)
    }
    
    fn parse_logical_or(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_logical_and()?;
        
        while self.check(&TokenType::Or) {
            self.advance();
            let right = self.parse_logical_and()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_logical_and(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_equality()?;
        
        while self.check(&TokenType::And) {
            self.advance();
            let right = self.parse_equality()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_equality(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;
        
        while self.match_any(&[TokenType::Equal, TokenType::NotEqual]) {
            let operator = match self.previous().token_type {
                TokenType::Equal => BinaryOperator::Equal,
                TokenType::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_comparison(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_term()?;
        
        while self.match_any(&[TokenType::Greater, TokenType::GreaterEqual, TokenType::Less, TokenType::LessEqual]) {
            let operator = match self.previous().token_type {
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_term(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_factor()?;
        
        while self.match_any(&[TokenType::Minus, TokenType::Plus]) {
            let operator = match self.previous().token_type {
                TokenType::Minus => BinaryOperator::Sub,
                TokenType::Plus => BinaryOperator::Add,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_factor(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_unary()?;
        
        while self.match_any(&[TokenType::Divide, TokenType::Multiply, TokenType::Power]) {
            let operator = match self.previous().token_type {
                TokenType::Divide => BinaryOperator::Div,
                TokenType::Multiply => BinaryOperator::Mul,
                TokenType::Power => BinaryOperator::Pow,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_unary(&mut self) -> Result<Expression, String> {
        if self.match_any(&[TokenType::Not, TokenType::Minus, TokenType::Increment, TokenType::Decrement]) {
            let operator = match self.previous().token_type {
                TokenType::Not => UnaryOperator::Not,
                TokenType::Minus => UnaryOperator::Minus,
                TokenType::Increment => UnaryOperator::PreIncrement,
                TokenType::Decrement => UnaryOperator::PreDecrement,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            return Ok(Expression::Unary(UnaryExpr {
                operator,
                operand: Box::new(right),
            }));
        }
        
        self.parse_postfix()
    }
    
    fn parse_postfix(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;
        
        while self.match_any(&[TokenType::Increment, TokenType::Decrement]) {
            let operator = match self.previous().token_type {
                TokenType::Increment => UnaryOperator::PostIncrement,
                TokenType::Decrement => UnaryOperator::PostDecrement,
                _ => unreachable!(),
            };
            expr = Expression::Unary(UnaryExpr {
                operator,
                operand: Box::new(expr),
            });
        }
        
        Ok(expr)
    }
    
    fn parse_primary(&mut self) -> Result<Expression, String> {
        match &self.peek().token_type {
            TokenType::BoolLiteral(b) => {
                let value = *b;
                self.advance();
                Ok(Expression::Literal(Literal::Bool(value)))
            },
            TokenType::NullType => {
                self.advance();
                Ok(Expression::Literal(Literal::Null))
            },
            TokenType::IntLiteral(n) => {
                let value = *n;
                self.advance();
                Ok(Expression::Literal(Literal::Int(value)))
            },
            TokenType::FloatLiteral(f) => {
                let value = *f;
                self.advance();
                Ok(Expression::Literal(Literal::Float(value)))
            },
            TokenType::StringLiteral(s) => {
                let value = s.clone();
                self.advance();
                Ok(Expression::Literal(Literal::String(value)))
            },
            TokenType::CharLiteral(c) => {
                let value = *c;
                self.advance();
                Ok(Expression::Literal(Literal::Char(value)))
            },
            TokenType::Identifier(name) => {
                let name = name.clone();
                self.advance();
                
                // Check for function call
                if self.check(&TokenType::LeftParen) {
                    self.advance();
                    let mut arguments = Vec::new();
                    
                    if !self.check(&TokenType::RightParen) {
                        loop {
                            arguments.push(self.parse_expression()?);
                            if !self.check(&TokenType::Comma) {
                                break;
                            }
                            self.advance();
                        }
                    }
                    
                    self.consume(&TokenType::RightParen, "Expected ')' after arguments")?;
                    
                    Ok(Expression::FunctionCall(FunctionCallExpr {
                        name,
                        arguments,
                    }))
                } else {
                    Ok(Expression::Identifier(name))
                }
            },
            TokenType::LeftParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.consume(&TokenType::RightParen, "Expected ')' after expression")?;
                Ok(expr)
            },
            TokenType::LeftBrace => {
                self.advance();
                let mut elements = Vec::new();
                
                if !self.check(&TokenType::RightBrace) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.check(&TokenType::Comma) {
                            break;
                        }
                        self.advance();
                    }
                }
                
                self.consume(&TokenType::RightBrace, "Expected '}' after array elements")?;
                Ok(Expression::Array(ArrayExpr { elements }))
            },
            _ => Err(format!("Unexpected token {:?} at line {}", self.peek().token_type, self.peek().line)),
        }
    }
    
    // Helper methods
    fn match_any(&mut self, types: &[TokenType]) -> bool {
        for token_type in types {
            if self.check(token_type) {
                self.advance();
                return true;
            }
        }
        false
    }
    
    fn check(&self, token_type: &TokenType) -> bool {
        if self.is_at_end() {
            false
        } else {
            std::mem::discriminant(&self.peek().token_type) == std::mem::discriminant(token_type)
        }
    }
    
    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }
    
    fn is_at_end(&self) -> bool {
        matches!(self.peek().token_type, TokenType::Eof)
    }
    
    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }
    
    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
    
    fn consume(&mut self, token_type: &TokenType, message: &str) -> Result<&Token, String> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(format!("{} at line {}", message, self.peek().line))
        }
    }
    
    fn consume_identifier(&mut self, message: &str) -> Result<String, String> {
        if let TokenType::Identifier(name) = &self.peek().token_type {
            let name = name.clone();
            self.advance();
            Ok(name)
        } else {
            Err(format!("{} at line {}", message, self.peek().line))
        }
    }
}