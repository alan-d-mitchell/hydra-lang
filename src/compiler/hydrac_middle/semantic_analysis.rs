use crate::compiler::hydrac_parse::parser::ast::*;
use std::collections::HashMap;

// ============================================================================
// SEMANTIC ANALYSIS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: String,
    pub symbol_type: Type,
    pub is_const: bool,
    pub is_global: bool,
    pub is_function: bool,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Scope {
    pub symbols: HashMap<String, Symbol>,
    pub parent: Option<Box<Scope>>,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            symbols: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_parent(parent: Box<Scope>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent: Some(parent),
        }
    }

    pub fn declare(&mut self, symbol: Symbol) -> Result<(), String> {
        if self.symbols.contains_key(&symbol.name) {
            return Err(format!(
                "Symbol '{}' is already declared in this scope at line {}",
                symbol.name, symbol.line
            ));
        }
        self.symbols.insert(symbol.name.clone(), symbol);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            Some(symbol)
        } else if let Some(ref parent) = self.parent {
            parent.lookup(name)
        } else {
            None
        }
    }

    pub fn lookup_current_scope(&self, name: &str) -> Option<&Symbol> {
        self.symbols.get(name)
    }
}

pub struct SemanticAnalyzer {
    pub global_scope: Scope,
    pub current_scope: Option<Box<Scope>>,
    pub errors: Vec<String>,
    pub current_function: Option<String>,
    pub in_loop: bool,
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        Self {
            global_scope: Scope::new(),
            current_scope: None,
            errors: Vec::new(),
            current_function: None,
            in_loop: false,
        }
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: collect all global declarations and function signatures
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let symbol = Symbol {
                        name: func.name.clone(),
                        symbol_type: func.return_type.clone(),
                        is_const: false,
                        is_global: true,
                        is_function: true,
                        line: 0, // We don't have line info in AST yet
                        column: 0,
                    };

                    if let Err(e) = self.global_scope.declare(symbol) {
                        self.errors.push(e);
                    }
                }
                Item::GlobalVariable(var) => {
                    let symbol = Symbol {
                        name: var.name.clone(),
                        symbol_type: var.var_type.clone(),
                        is_const: var.is_const,
                        is_global: true,
                        is_function: false,
                        line: 0,
                        column: 0,
                    };

                    if let Err(e) = self.global_scope.declare(symbol) {
                        self.errors.push(e);
                    }
                }
            }
        }

        // Second pass: analyze function bodies and global variable initializers
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.analyze_function(func);
                }
                Item::GlobalVariable(var) => {
                    if let Some(ref expr) = var.initializer {
                        self.analyze_expression(expr);
                    }
                }
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_function(&mut self, func: &Function) {
        self.current_function = Some(func.name.clone());
        
        // Save the current scope (should be None for top-level functions)
        let saved_scope = self.current_scope.take();
        
        // Create new function scope
        let mut function_scope = Scope::new();
        
        // Add parameters to function scope
        for param in &func.parameters {
            let symbol = Symbol {
                name: param.name.clone(),
                symbol_type: param.param_type.clone(),
                is_const: false,
                is_global: false,
                is_function: false,
                line: 0,
                column: 0,
            };

            if let Err(e) = function_scope.declare(symbol) {
                self.errors.push(e);
            }
        }

        // Set function scope as current
        self.current_scope = Some(Box::new(function_scope));
        
        // Analyze each statement in the function body
        for stmt in &func.body.statements {
            self.analyze_statement(stmt);
        }

        // Restore previous scope
        self.current_scope = saved_scope;
        self.current_function = None;
    }

    fn analyze_block(&mut self, block: &Block) {
        // Create new scope for block as a child of current scope
        let parent_scope = std::mem::replace(&mut self.current_scope, None);
        let block_scope = if let Some(parent) = parent_scope {
            Scope::with_parent(parent)
        } else {
            Scope::new()
        };

        self.current_scope = Some(Box::new(block_scope));

        for stmt in &block.statements {
            self.analyze_statement(stmt);
        }

        // Restore parent scope
        self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
    }

    fn analyze_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => {
                self.analyze_var_decl(var_decl);
            }
            Statement::If(if_stmt) => {
                self.analyze_expression(&if_stmt.condition);
                self.analyze_block(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.analyze_statement(else_branch);
                }
            }
            Statement::While(while_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                self.analyze_expression(&while_stmt.condition);
                self.analyze_block(&while_stmt.body);
                self.in_loop = prev_in_loop;
            }
            Statement::For(for_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                
                // Create new scope for the for loop (to handle variable declarations in initializer)
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let for_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };
                self.current_scope = Some(Box::new(for_scope));
                
                // Now analyze all parts of the for loop in this scope
                if let Some(ref init) = for_stmt.initializer {
                    self.analyze_statement(init);
                }
                if let Some(ref cond) = for_stmt.condition {
                    self.analyze_expression(cond);
                }
                if let Some(ref inc) = for_stmt.increment {
                    self.analyze_expression(inc);
                }
                self.analyze_block(&for_stmt.body);
                
                // Restore parent scope
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                self.in_loop = prev_in_loop;
            }
            Statement::ForEach(foreach_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                
                // First analyze the iterable expression in the current scope
                self.analyze_expression(&foreach_stmt.iterable);
                
                // Create new scope for foreach variable
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let mut foreach_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };

                let symbol = Symbol {
                    name: foreach_stmt.var_name.clone(),
                    symbol_type: foreach_stmt.var_type.clone(),
                    is_const: false,
                    is_global: false,
                    is_function: false,
                    line: 0,
                    column: 0,
                };

                if let Err(e) = foreach_scope.declare(symbol) {
                    self.errors.push(e);
                }

                self.current_scope = Some(Box::new(foreach_scope));
                self.analyze_block(&foreach_stmt.body);
                
                // Restore parent scope
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                self.in_loop = prev_in_loop;
            }
            Statement::Return(return_stmt) => {
                if self.current_function.is_none() {
                    self.errors.push("Return statement outside of function".to_string());
                }
                if let Some(ref expr) = return_stmt.value {
                    self.analyze_expression(expr);
                }
            }
            Statement::Break(break_stmt) => {
                if !self.in_loop {
                    self.errors.push("Break statement outside of loop".to_string());
                }
                if let Some(ref cond) = break_stmt.condition {
                    self.analyze_expression(cond);
                }
            }
            Statement::Skip(_) => {
                if !self.in_loop {
                    self.errors.push("Skip statement outside of loop".to_string());
                }
            }
            Statement::Expression(expr) => {
                self.analyze_expression(expr);
            }
        }
    }

    fn analyze_var_decl(&mut self, var_decl: &VarDecl) {
        if let Some(ref expr) = var_decl.initializer {
            self.analyze_expression(expr);
        }

        let symbol = Symbol {
            name: var_decl.name.clone(),
            symbol_type: var_decl.var_type.clone(),
            is_const: var_decl.is_const,
            is_global: false,
            is_function: false,
            line: 0,
            column: 0,
        };

        if let Some(ref mut scope) = self.current_scope {
            if let Err(e) = scope.declare(symbol) {
                self.errors.push(e);
            }
        } else {
            self.errors.push(format!("Variable declaration '{}' outside of any scope", var_decl.name));
        }
    }

    fn analyze_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Binary(binary_expr) => {
                self.analyze_expression(&binary_expr.left);
                self.analyze_expression(&binary_expr.right);
            }
            Expression::Unary(unary_expr) => {
                self.analyze_expression(&unary_expr.operand);
            }
            Expression::Literal(_) => {
                // Literals don't need analysis
            }
            Expression::Identifier(name) => {
                if !self.is_symbol_defined(name) {
                    // Debug: Let's see what symbols are available
                    let mut available_symbols = Vec::new();
                    if let Some(ref scope) = self.current_scope {
                        self.collect_symbols_in_scope(scope, &mut available_symbols);
                    }
                    for (symbol_name, _) in &self.global_scope.symbols {
                        available_symbols.push(symbol_name.clone());
                    }
                    
                    self.errors.push(format!(
                        "Undefined symbol '{}' (available symbols: {})", 
                        name, 
                        available_symbols.join(", ")
                    ));
                }
            }
            Expression::Array(array_expr) => {
                for element in &array_expr.elements {
                    self.analyze_expression(element);
                }
            }
            Expression::FunctionCall(call_expr) => {
                // Check if function exists
                if !self.is_function_defined(&call_expr.name) {
                    self.errors.push(format!("Undefined function '{}'", call_expr.name));
                }
                
                // Analyze arguments
                for arg in &call_expr.arguments {
                    self.analyze_expression(arg);
                }
            }
            Expression::IsIn(is_in_expr) => {
                self.analyze_expression(&is_in_expr.value);
                self.analyze_expression(&is_in_expr.collection);
            }
        }
    }

    fn collect_symbols_in_scope(&self, scope: &Scope, symbols: &mut Vec<String>) {
        for (name, _) in &scope.symbols {
            symbols.push(name.clone());
        }
        if let Some(ref parent) = scope.parent {
            self.collect_symbols_in_scope(parent, symbols);
        }
    }

    fn is_symbol_defined(&self, name: &str) -> bool {
        // First check current scope chain
        if let Some(ref scope) = self.current_scope {
            if scope.lookup(name).is_some() {
                return true;
            }
        }
        
        // Then check global scope
        self.global_scope.lookup(name).is_some()
    }

    fn is_function_defined(&self, name: &str) -> bool {
        // Check global scope for functions
        if let Some(symbol) = self.global_scope.lookup(name) {
            symbol.is_function
        } else {
            false
        }
    }

    pub fn analyze_with_type_checking(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First pass: collect all global declarations and function signatures
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    let symbol = Symbol {
                        name: func.name.clone(),
                        symbol_type: func.return_type.clone(),
                        is_const: false,
                        is_global: true,
                        is_function: true,
                        line: 0,
                        column: 0,
                    };

                    if let Err(e) = self.global_scope.declare(symbol) {
                        self.errors.push(e);
                    }
                }
                Item::GlobalVariable(var) => {
                    let symbol = Symbol {
                        name: var.name.clone(),
                        symbol_type: var.var_type.clone(),
                        is_const: var.is_const,
                        is_global: true,
                        is_function: false,
                        line: 0,
                        column: 0,
                    };

                    if let Err(e) = self.global_scope.declare(symbol) {
                        self.errors.push(e);
                    }
                }
            }
        }

        // Second pass: analyze function bodies and global variable initializers WITH type checking
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.analyze_function_with_types(func);
                }
                Item::GlobalVariable(var) => {
                    if let Some(ref expr) = var.initializer {
                        // Type check the global variable initializer
                        let expr_type = self.infer_expression_type(expr);
                        if let Some(expr_type) = expr_type {
                            if !self.types_compatible(&var.var_type, &expr_type) {
                                self.errors.push(format!(
                                    "Type mismatch in global variable '{}': expected {}, found {}",
                                    var.name, var.var_type, expr_type
                                ));
                            }
                        }
                        
                        self.analyze_expression(expr);
                    }
                }
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn analyze_function_with_types(&mut self, func: &Function) {
        self.current_function = Some(func.name.clone());
        
        // Save the current scope (should be None for top-level functions)
        let saved_scope = self.current_scope.take();
        
        // Create new function scope
        let mut function_scope = Scope::new();
        
        // Add parameters to function scope
        for param in &func.parameters {
            let symbol = Symbol {
                name: param.name.clone(),
                symbol_type: param.param_type.clone(),
                is_const: false,
                is_global: false,
                is_function: false,
                line: 0,
                column: 0,
            };

            if let Err(e) = function_scope.declare(symbol) {
                self.errors.push(e);
            }
        }

        // Set function scope as current
        self.current_scope = Some(Box::new(function_scope));
        
        // Analyze each statement in the function body WITH type checking
        for stmt in &func.body.statements {
            self.analyze_statement_with_types(stmt, &func.return_type);
        }

        // Restore previous scope
        self.current_scope = saved_scope;
        self.current_function = None;
    }

    fn analyze_statement_with_types(&mut self, stmt: &Statement, function_return_type: &Type) {
        // Do semantic analysis and type checking together
        match stmt {
            Statement::VarDecl(var_decl) => {
                // Type check initializer before declaring variable
                if let Some(ref expr) = var_decl.initializer {
                    let expr_type = self.infer_expression_type(expr);
                    if let Some(expr_type) = expr_type {
                        if !self.types_compatible(&var_decl.var_type, &expr_type) {
                            self.errors.push(format!(
                                "Type mismatch in variable '{}': expected {}, found {}",
                                var_decl.name, var_decl.var_type, expr_type
                            ));
                        }
                    }
                }
                
                // Now declare the variable
                self.analyze_var_decl(var_decl);
            }
            Statement::If(if_stmt) => {
                // Type check condition
                let cond_type = self.infer_expression_type(&if_stmt.condition);
                if let Some(cond_type) = cond_type {
                    if !matches!(cond_type, Type::Boolean) {
                        self.errors.push(format!(
                            "If condition must be boolean, found {}",
                            cond_type
                        ));
                    }
                }
                
                // Analyze branches
                self.analyze_expression(&if_stmt.condition);
                
                // Create scope for then branch
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let then_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };
                self.current_scope = Some(Box::new(then_scope));
                
                for stmt in &if_stmt.then_branch.statements {
                    self.analyze_statement_with_types(stmt, function_return_type);
                }
                
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.analyze_statement_with_types(else_branch, function_return_type);
                }
            }
            Statement::While(while_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                
                // Type check condition
                let cond_type = self.infer_expression_type(&while_stmt.condition);
                if let Some(cond_type) = cond_type {
                    if !matches!(cond_type, Type::Boolean) {
                        self.errors.push(format!(
                            "While condition must be boolean, found {}",
                            cond_type
                        ));
                    }
                }
                
                self.analyze_expression(&while_stmt.condition);
                
                // Create scope for body
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let body_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };
                self.current_scope = Some(Box::new(body_scope));
                
                for stmt in &while_stmt.body.statements {
                    self.analyze_statement_with_types(stmt, function_return_type);
                }
                
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                self.in_loop = prev_in_loop;
            }
            Statement::For(for_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                
                // Create new scope for the for loop
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let for_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };
                self.current_scope = Some(Box::new(for_scope));
                
                // Analyze initializer (this might declare a variable)
                if let Some(ref init) = for_stmt.initializer {
                    self.analyze_statement_with_types(init, function_return_type);
                }
                
                // Type check condition
                if let Some(ref cond) = for_stmt.condition {
                    let cond_type = self.infer_expression_type(cond);
                    if let Some(cond_type) = cond_type {
                        if !matches!(cond_type, Type::Boolean) {
                            self.errors.push(format!(
                                "For condition must be boolean, found {}",
                                cond_type
                            ));
                        }
                    }
                    self.analyze_expression(cond);
                }
                
                // Analyze increment
                if let Some(ref inc) = for_stmt.increment {
                    self.infer_expression_type(inc); // Type check
                    self.analyze_expression(inc);    // Semantic check
                }
                
                // Analyze body
                for stmt in &for_stmt.body.statements {
                    self.analyze_statement_with_types(stmt, function_return_type);
                }
                
                // Restore parent scope
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                self.in_loop = prev_in_loop;
            }
            Statement::ForEach(foreach_stmt) => {
                let prev_in_loop = self.in_loop;
                self.in_loop = true;
                
                // First analyze the iterable expression in the current scope
                self.analyze_expression(&foreach_stmt.iterable);
                
                // Create new scope for foreach variable
                let parent_scope = std::mem::replace(&mut self.current_scope, None);
                let mut foreach_scope = if let Some(parent) = parent_scope {
                    Scope::with_parent(parent)
                } else {
                    Scope::new()
                };

                let symbol = Symbol {
                    name: foreach_stmt.var_name.clone(),
                    symbol_type: foreach_stmt.var_type.clone(),
                    is_const: false,
                    is_global: false,
                    is_function: false,
                    line: 0,
                    column: 0,
                };

                if let Err(e) = foreach_scope.declare(symbol) {
                    self.errors.push(e);
                }

                self.current_scope = Some(Box::new(foreach_scope));
                
                for stmt in &foreach_stmt.body.statements {
                    self.analyze_statement_with_types(stmt, function_return_type);
                }
                
                // Restore parent scope
                self.current_scope = self.current_scope.take().and_then(|scope| scope.parent);
                self.in_loop = prev_in_loop;
            }
            Statement::Return(return_stmt) => {
                if self.current_function.is_none() {
                    self.errors.push("Return statement outside of function".to_string());
                }
                
                if let Some(ref expr) = return_stmt.value {
                    let return_type = self.infer_expression_type(expr);
                    if let Some(return_type) = return_type {
                        if !self.types_compatible(function_return_type, &return_type) {
                            self.errors.push(format!(
                                "Return type mismatch: expected {}, found {}",
                                function_return_type, return_type
                            ));
                        }
                    }
                    self.analyze_expression(expr);
                } else if !matches!(function_return_type, Type::Void) {
                    self.errors.push(format!(
                        "Function expects return value of type {}, but none provided",
                        function_return_type
                    ));
                }
            }
            Statement::Break(break_stmt) => {
                if !self.in_loop {
                    self.errors.push("Break statement outside of loop".to_string());
                }
                if let Some(ref cond) = break_stmt.condition {
                    let cond_type = self.infer_expression_type(cond);
                    if let Some(cond_type) = cond_type {
                        if !matches!(cond_type, Type::Boolean) {
                            self.errors.push(format!(
                                "Break condition must be boolean, found {}",
                                cond_type
                            ));
                        }
                    }
                    self.analyze_expression(cond);
                }
            }
            Statement::Skip(_) => {
                if !self.in_loop {
                    self.errors.push("Skip statement outside of loop".to_string());
                }
            }
            Statement::Expression(expr) => {
                self.infer_expression_type(expr); // Type check
                self.analyze_expression(expr);    // Semantic check
            }
        }
    }

    // Add type checking methods to SemanticAnalyzer
    fn infer_expression_type(&mut self, expr: &Expression) -> Option<Type> {
        match expr {
            Expression::Literal(literal) => Some(self.literal_type(literal)),
            Expression::Identifier(name) => {
                if let Some(symbol) = self.get_symbol(name) {
                    Some(symbol.symbol_type.clone())
                } else {
                    // Debug: Let's see what symbols are available
                    let mut available_symbols = Vec::new();
                    
                    // Collect symbols from current scope chain
                    if let Some(ref scope) = self.current_scope {
                        self.collect_symbols_in_scope_debug(scope, &mut available_symbols);
                    }
                    
                    // Collect global symbols
                    for (symbol_name, _) in &self.global_scope.symbols {
                        available_symbols.push(symbol_name.clone());
                    }
                    
                    self.errors.push(format!(
                        "Undefined identifier '{}' (available symbols: {})", 
                        name, 
                        available_symbols.join(", ")
                    ));
                    None
                }
            }
            Expression::Binary(binary_expr) => {
                self.infer_binary_type(binary_expr)
            }
            Expression::Unary(unary_expr) => {
                self.infer_unary_type(unary_expr)
            }
            Expression::Array(array_expr) => {
                self.infer_array_type(array_expr)
            }
            Expression::FunctionCall(call_expr) => {
                self.infer_function_call_type(call_expr)
            }
            Expression::IsIn(_) => {
                Some(Type::Boolean)
            }
        }
    }

    fn collect_symbols_in_scope_debug(&self, scope: &Scope, symbols: &mut Vec<String>) {
        for (name, _) in &scope.symbols {
            symbols.push(name.clone());
        }
        if let Some(ref parent) = scope.parent {
            self.collect_symbols_in_scope_debug(parent, symbols);
        }
    }

    fn literal_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::String(_) => Type::String,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Boolean,
            Literal::Null => Type::Void,
        }
    }

    fn infer_binary_type(&mut self, binary_expr: &BinaryExpr) -> Option<Type> {
        let left_type = self.infer_expression_type(&binary_expr.left)?;
        let right_type = self.infer_expression_type(&binary_expr.right)?;

        match binary_expr.operator {
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    // For modulo operation, ensure both operands are integers
                    if matches!(binary_expr.operator, BinaryOperator::Mod) {
                        if !matches!(left_type, Type::Int) || !matches!(right_type, Type::Int) {
                            self.errors.push(format!(
                                "Modulo operation requires integer types, found {} and {}",
                                left_type, right_type
                            ));
                            return None;
                        }
                        Some(Type::Int)
                    } else {
                        // For other arithmetic operations, allow float promotion
                        if matches!(left_type, Type::Float) || matches!(right_type, Type::Float) {
                            Some(Type::Float)
                        } else {
                            Some(Type::Int)
                        }
                    }
                } else {
                    self.errors.push(format!(
                        "Arithmetic operation requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::Equal | BinaryOperator::NotEqual => {
                if self.types_compatible(&left_type, &right_type) {
                    Some(Type::Boolean)
                } else {
                    self.errors.push(format!(
                        "Comparison requires compatible types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::Less | BinaryOperator::LessEqual | 
            BinaryOperator::Greater | BinaryOperator::GreaterEqual => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Some(Type::Boolean)
                } else {
                    self.errors.push(format!(
                        "Numeric comparison requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::And | BinaryOperator::Or => {
                if matches!(left_type, Type::Boolean) && matches!(right_type, Type::Boolean) {
                    Some(Type::Boolean)
                } else {
                    self.errors.push(format!(
                        "Logical operation requires boolean types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::Assign => {
                if self.types_compatible(&left_type, &right_type) {
                    Some(left_type)
                } else {
                    self.errors.push(format!(
                        "Assignment type mismatch: {} = {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::AddAssign | BinaryOperator::SubAssign | 
            BinaryOperator::MulAssign | BinaryOperator::DivAssign | BinaryOperator::ModAssign => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    // For modulo assignment, ensure both operands are integers
                    if matches!(binary_expr.operator, BinaryOperator::ModAssign) {
                        if !matches!(left_type, Type::Int) || !matches!(right_type, Type::Int) {
                            self.errors.push(format!(
                                "Modulo assignment requires integer types, found {} and {}",
                                left_type, right_type
                            ));
                            return None;
                        }
                    }
                    Some(left_type)
                } else {
                    self.errors.push(format!(
                        "Compound assignment requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            _ => Some(left_type), // For other operators
        }
    }

    fn infer_unary_type(&mut self, unary_expr: &UnaryExpr) -> Option<Type> {
        let operand_type = self.infer_expression_type(&unary_expr.operand)?;

        match unary_expr.operator {
            UnaryOperator::Not => {
                if matches!(operand_type, Type::Boolean) {
                    Some(Type::Boolean)
                } else {
                    self.errors.push(format!("Logical NOT requires boolean type, found {}", operand_type));
                    None
                }
            }
            UnaryOperator::Minus => {
                if self.is_numeric_type(&operand_type) {
                    Some(operand_type)
                } else {
                    self.errors.push(format!("Unary minus requires numeric type, found {}", operand_type));
                    None
                }
            }
            UnaryOperator::PreIncrement | UnaryOperator::PreDecrement |
            UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => {
                if self.is_numeric_type(&operand_type) {
                    Some(operand_type)
                } else {
                    self.errors.push(format!("Increment/decrement requires numeric type, found {}", operand_type));
                    None
                }
            }
        }
    }

    fn infer_array_type(&mut self, array_expr: &ArrayExpr) -> Option<Type> {
        if array_expr.elements.is_empty() {
            self.errors.push("Cannot infer type of empty array".to_string());
            return None;
        }

        let first_type = self.infer_expression_type(&array_expr.elements[0])?;
        
        for (i, element) in array_expr.elements.iter().enumerate().skip(1) {
            let element_type = self.infer_expression_type(element)?;
            if !self.types_compatible(&first_type, &element_type) {
                self.errors.push(format!(
                    "Array element {} has type {}, expected {}",
                    i, element_type, first_type
                ));
                return None;
            }
        }

        Some(Type::Array(Box::new(first_type)))
    }

    fn infer_function_call_type(&mut self, call_expr: &FunctionCallExpr) -> Option<Type> {
        if let Some(symbol) = self.get_symbol(&call_expr.name) {
            if symbol.is_function {
                Some(symbol.symbol_type.clone())
            } else {
                self.errors.push(format!("'{}' is not a function", call_expr.name));
                None
            }
        } else {
            self.errors.push(format!("Undefined function '{}'", call_expr.name));
            None
        }
    }

    fn is_numeric_type(&self, t: &Type) -> bool {
        matches!(t, Type::Int | Type::Float)
    }

    fn types_compatible(&self, expected: &Type, actual: &Type) -> bool {
        match (expected, actual) {
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Float, Type::Int) => true, // Allow int to float promotion
            (Type::String, Type::String) => true,
            (Type::Char, Type::Char) => true,
            (Type::Boolean, Type::Boolean) => true,
            (Type::Void, Type::Void) => true,
            (Type::Array(expected_inner), Type::Array(actual_inner)) => {
                self.types_compatible(expected_inner, actual_inner)
            }
            _ => false,
        }
    }

    pub fn get_symbol(&self, name: &str) -> Option<&Symbol> {
        // First check current scope chain
        if let Some(ref scope) = self.current_scope {
            if let Some(symbol) = scope.lookup(name) {
                return Some(symbol);
            }
        }
        
        // Then check global scope
        self.global_scope.lookup(name)
    }
}