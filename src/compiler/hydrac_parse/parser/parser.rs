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
        let mut imports = Vec::new();
        let mut items = Vec::new();
        
        // Parse imports first
        while self.check(&TokenType::Import) && !self.is_at_end() {
            imports.push(self.parse_import()?);
            self.skip_newlines();
        }
        
        // Parse rest of program
        while !self.is_at_end() {
            // Skip newlines at top level
            if self.check(&TokenType::Newline) {
                self.advance();
                continue;
            }
            
            items.push(self.parse_item()?);
        }
        
        Ok(Program { imports, items })
    }
    
    fn parse_import(&mut self) -> Result<ImportStmt, String> {
        self.consume(&TokenType::Import, "Expected 'import'")?;
        
        let mut path = Vec::new();
        let mut is_wildcard = false;
        
        // Parse first identifier
        let first_part = self.consume_identifier("Expected import path")?;
        path.push(first_part);
        
        // Parse remaining path segments with ::
        while self.check(&TokenType::DoubleColon) {
            self.advance(); // consume ::
            let part = self.consume_identifier("Expected identifier after '::'")?;
            path.push(part);
        }
        
        // Check if this is a wildcard import (just the namespace without specific function)
        // If we only have one part or the last part looks like a namespace, it's wildcard
        is_wildcard = path.len() <= 2; // e.g., "stdlib" or "stdlib::io"
        
        self.consume(&TokenType::Semicolon, "Expected ';' after import")?;
        
        Ok(ImportStmt { path, is_wildcard })
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
        let mut is_variadic = false;
        
        if !self.check(&TokenType::RightParen) {
            loop {
                // Check for variadic parameter
                if self.check(&TokenType::DotDotDot) {
                    self.advance();
                    is_variadic = true;
                    break;
                }
                
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
            is_variadic,
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
        } else if self.check(&TokenType::DotDotDot) {
            self.advance();
            Ok(Type::Variadic(Box::new(base_type)))
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
                           TokenType::MultiplyAssign, TokenType::DivideAssign, TokenType::ModuloAssign]) {
            let operator = match self.previous().token_type {
                TokenType::Assign => BinaryOperator::Assign,
                TokenType::PlusAssign => BinaryOperator::AddAssign,
                TokenType::MinusAssign => BinaryOperator::SubAssign,
                TokenType::MultiplyAssign => BinaryOperator::MulAssign,
                TokenType::DivideAssign => BinaryOperator::DivAssign,
                TokenType::ModuloAssign => BinaryOperator::ModAssign,
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
        let mut expr = self.parse_is_in()?;
        
        while self.match_any(&[TokenType::Equal, TokenType::NotEqual]) {
            let operator = match self.previous().token_type {
                TokenType::Equal => BinaryOperator::Equal,
                TokenType::NotEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_is_in()?;
            expr = Expression::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }
        
        Ok(expr)
    }
    
    // NEW: Handle "is in" expressions
    fn parse_is_in(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_comparison()?;
        
        // Handle "is in" construct
        while self.check(&TokenType::Is) {
            self.advance(); // consume 'is'
            if self.check(&TokenType::In) {
                self.advance(); // consume 'in'
                let right = self.parse_comparison()?;
                expr = Expression::IsIn(IsInExpr {
                    value: Box::new(expr),
                    collection: Box::new(right),
                });
            } else {
                return Err(format!("Expected 'in' after 'is' at line {}", self.peek().line));
            }
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
        
        while self.match_any(&[TokenType::Divide, TokenType::Multiply, TokenType::Modulo, TokenType::Power]) {
            let operator = match self.previous().token_type {
                TokenType::Divide => BinaryOperator::Div,
                TokenType::Multiply => BinaryOperator::Mul,
                TokenType::Modulo => BinaryOperator::Mod,
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
        let mut expr = self.parse_namespaced_call()?;
        
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
    
    fn parse_namespaced_call(&mut self) -> Result<Expression, String> {
        let mut expr = self.parse_primary()?;
        
        // Handle namespaced method calls: expr::method()
        while self.check(&TokenType::DoubleColon) {
            self.advance(); // consume ::
            let method = self.consume_identifier("Expected method name after '::'")?;
            
            if self.check(&TokenType::LeftParen) {
                self.advance(); // consume (
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
                
                expr = Expression::NamespacedCall(NamespacedCallExpr {
                    object: Box::new(expr),
                    method,
                    arguments,
                });
            } else {
                return Err(format!("Expected '(' after method name '{}' at line {}", method, self.peek().line));
            }
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
                
                // Check if this is a format string (contains % placeholders)
                if value.contains('%') {
                    // This might be a format string, but for now treat as regular string
                    // Format string parsing would happen in semantic analysis
                }
                
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
                
                // Check for namespaced function call
                if self.check(&TokenType::DoubleColon) {
                    self.advance(); // consume ::
                    
                    // Parse the rest of the namespace path
                    let mut namespace = vec![name];
                    
                    while !self.check(&TokenType::LeftParen) {
                        let part = self.consume_identifier("Expected identifier in namespace path")?;
                        namespace.push(part);
                        
                        if self.check(&TokenType::DoubleColon) {
                            self.advance(); // consume ::
                        } else {
                            break;
                        }
                    }
                    
                    // The last part is the function name
                    let function_name = namespace.pop().unwrap();
                    
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
                            name: function_name,
                            namespace: if namespace.is_empty() { None } else { Some(namespace) },
                            arguments,
                        }))
                    } else {
                        Err(format!("Expected '(' after namespaced function name at line {}", self.peek().line))
                    }
                }
                // Check for regular function call
                else if self.check(&TokenType::LeftParen) {
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
                        namespace: None,
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
                
                // Check if this is array initialization syntax: {type, size}
                if self.is_array_init_syntax() {
                    let element_type = self.parse_type()?;
                    self.consume(&TokenType::Comma, "Expected ',' after element type in array initialization")?;
                    let size = self.parse_expression()?;
                    self.consume(&TokenType::RightBrace, "Expected '}' after array initialization")?;
                    
                    Ok(Expression::ArrayInit(ArrayInitExpr {
                        element_type,
                        size: Box::new(size),
                    }))
                } else {
                    // Regular array literal
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
                }
            },
            _ => Err(format!("Unexpected token {:?} at line {}", self.peek().token_type, self.peek().line)),
        }
    }
    
    fn is_array_init_syntax(&self) -> bool {
        // Look ahead to see if this looks like {type, expression}
        // This is a simplified check - in a real implementation you'd want more sophisticated lookahead
        let mut i = 0;
        let mut brace_count = 1;
        
        while self.current + i < self.tokens.len() && brace_count > 0 {
            match &self.tokens[self.current + i].token_type {
                TokenType::LeftBrace => brace_count += 1,
                TokenType::RightBrace => brace_count -= 1,
                TokenType::IntType | TokenType::FloatType | TokenType::StringType |
                TokenType::CharType | TokenType::BooleanType => {
                    // Found a type token, check if followed by comma
                    if self.current + i + 1 < self.tokens.len() {
                        if let TokenType::Comma = self.tokens[self.current + i + 1].token_type {
                            return true;
                        }
                    }
                },
                _ => {}
            }
            i += 1;
        }
        false
    }
    
    fn skip_newlines(&mut self) {
        while self.check(&TokenType::Newline) && !self.is_at_end() {
            self.advance();
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