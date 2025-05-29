use crate::compiler::hydrac_parse::parser::ast::*;
use crate::compiler::hydrac_middle::semantic_analysis::{SemanticAnalyzer, Symbol};

// ============================================================================
// TYPE CHECKING
// ============================================================================

pub struct TypeChecker<'a> {
    pub analyzer: &'a SemanticAnalyzer,
    pub errors: Vec<String>,
    pub current_function_return_type: Option<Type>,
}

impl<'a> TypeChecker<'a> {
    pub fn new(analyzer: &'a SemanticAnalyzer) -> Self {
        Self {
            analyzer,
            errors: Vec::new(),
            current_function_return_type: None,
        }
    }

    pub fn check(&mut self, program: &Program) -> Result<(), Vec<String>> {
        for item in &program.items {
            match item {
                Item::Function(func) => {
                    self.check_function(func);
                }
                Item::GlobalVariable(var) => {
                    self.check_global_variable(var);
                }
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(self.errors.clone())
        }
    }

    fn check_function(&mut self, func: &Function) {
        self.current_function_return_type = Some(func.return_type.clone());
        self.check_block(&func.body);
        self.current_function_return_type = None;
    }

    fn check_global_variable(&mut self, var: &GlobalVariable) {
        if let Some(ref expr) = var.initializer {
            let expr_type = self.infer_expression_type(expr);
            if let Some(expr_type) = expr_type {
                if !self.types_compatible(&var.var_type, &expr_type) {
                    self.errors.push(format!(
                        "Type mismatch in global variable '{}': expected {}, found {}",
                        var.name, var.var_type, expr_type
                    ));
                }
            }
        }
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.statements {
            self.check_statement(stmt);
        }
    }

    fn check_statement(&mut self, stmt: &Statement) {
        match stmt {
            Statement::VarDecl(var_decl) => {
                self.check_var_decl(var_decl);
            }
            Statement::If(if_stmt) => {
                let cond_type = self.infer_expression_type(&if_stmt.condition);
                if let Some(cond_type) = cond_type {
                    if !matches!(cond_type, Type::Boolean) {
                        self.errors.push(format!(
                            "If condition must be boolean, found {}",
                            cond_type
                        ));
                    }
                }
                self.check_block(&if_stmt.then_branch);
                if let Some(ref else_branch) = if_stmt.else_branch {
                    self.check_statement(else_branch);
                }
            }
            Statement::While(while_stmt) => {
                let cond_type = self.infer_expression_type(&while_stmt.condition);
                if let Some(cond_type) = cond_type {
                    if !matches!(cond_type, Type::Boolean) {
                        self.errors.push(format!(
                            "While condition must be boolean, found {}",
                            cond_type
                        ));
                    }
                }
                self.check_block(&while_stmt.body);
            }
            Statement::For(for_stmt) => {
                if let Some(ref init) = for_stmt.initializer {
                    self.check_statement(init);
                }
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
                }
                if let Some(ref inc) = for_stmt.increment {
                    self.infer_expression_type(inc); // Just check for validity
                }
                self.check_block(&for_stmt.body);
            }
            Statement::ForEach(foreach_stmt) => {
                let iterable_type = self.infer_expression_type(&foreach_stmt.iterable);
                if let Some(iterable_type) = iterable_type {
                    match iterable_type {
                        Type::Array(ref inner_type) => {
                            if !self.types_compatible(&foreach_stmt.var_type, inner_type) {
                                self.errors.push(format!(
                                    "ForEach variable type mismatch: expected {}, array contains {}",
                                    foreach_stmt.var_type, inner_type
                                ));
                            }
                        }
                        _ => {
                            self.errors.push(format!(
                                "ForEach requires array type, found {}",
                                iterable_type
                            ));
                        }
                    }
                }
                self.check_block(&foreach_stmt.body);
            }
            Statement::Return(return_stmt) => {
                if let Some(expected_type) = self.current_function_return_type.clone() {
                    if let Some(ref expr) = return_stmt.value {
                        let return_type = self.infer_expression_type(expr);
                        if let Some(return_type) = return_type {
                            if !self.types_compatible(&expected_type, &return_type) {
                                self.errors.push(format!(
                                    "Return type mismatch: expected {}, found {}",
                                    expected_type, return_type
                                ));
                            }
                        }
                    } else if !matches!(expected_type, Type::Void) {
                        self.errors.push(format!(
                            "Function expects return value of type {}, but none provided",
                            expected_type
                        ));
                    }
                }
            }
            Statement::Break(break_stmt) => {
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
                }
            }
            Statement::Skip(_) => {
                // No type checking needed for skip
            }
            Statement::Expression(expr) => {
                self.infer_expression_type(expr); // Just validate the expression
            }
        }
    }

    fn check_var_decl(&mut self, var_decl: &VarDecl) {
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
    }

    pub fn infer_expression_type(&mut self, expr: &Expression) -> Option<Type> {
        match expr {
            Expression::Literal(literal) => Some(self.literal_type(literal)),
            Expression::Identifier(name) => {
                if let Some(symbol) = self.analyzer.get_symbol(name) {
                    Some(symbol.symbol_type.clone())
                } else {
                    // Debug: Let's see what symbols are available
                    let mut available_symbols = Vec::new();
                    
                    // Collect symbols from current scope chain
                    if let Some(ref scope) = self.analyzer.current_scope {
                        self.collect_symbols_in_scope(scope, &mut available_symbols);
                    }
                    
                    // Collect global symbols
                    for (symbol_name, _) in &self.analyzer.global_scope.symbols {
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
                // "is in" expressions always return boolean
                Some(Type::Boolean)
            }
        }
    }

    fn collect_symbols_in_scope(&self, scope: &Box<crate::compiler::hydrac_middle::semantic_analysis::Scope>, symbols: &mut Vec<String>) {
        for (name, _) in &scope.symbols {
            symbols.push(name.clone());
        }
        if let Some(ref parent) = scope.parent {
            self.collect_symbols_in_scope(parent, symbols);
        }
    }

    fn infer_binary_type(&mut self, binary_expr: &BinaryExpr) -> Option<Type> {
        let left_type = self.infer_expression_type(&binary_expr.left)?;
        let right_type = self.infer_expression_type(&binary_expr.right)?;

        match binary_expr.operator {
            // Arithmetic operators
            BinaryOperator::Add | BinaryOperator::Sub | BinaryOperator::Mul | BinaryOperator::Div => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    // Promote to float if either operand is float
                    if matches!(left_type, Type::Float) || matches!(right_type, Type::Float) {
                        Some(Type::Float)
                    } else {
                        Some(Type::Int)
                    }
                } else {
                    self.errors.push(format!(
                        "Arithmetic operation requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::Pow => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Some(Type::Float) // Power operation always returns float
                } else {
                    self.errors.push(format!(
                        "Power operation requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            // Comparison operators
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
            // Logical operators
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
            // Assignment operators
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
            BinaryOperator::MulAssign | BinaryOperator::DivAssign => {
                if self.is_numeric_type(&left_type) && self.is_numeric_type(&right_type) {
                    Some(left_type)
                } else {
                    self.errors.push(format!(
                        "Compound assignment requires numeric types, found {} and {}",
                        left_type, right_type
                    ));
                    None
                }
            }
            BinaryOperator::In => {
                // Handled by IsIn expression, shouldn't reach here
                Some(Type::Boolean)
            }
        }
    }

    fn infer_unary_type(&mut self, unary_expr: &UnaryExpr) -> Option<Type> {
        let operand_type = self.infer_expression_type(&unary_expr.operand)?;

        match unary_expr.operator {
            UnaryOperator::Not => {
                if matches!(operand_type, Type::Boolean) {
                    Some(Type::Boolean)
                } else {
                    self.errors.push(format!(
                        "Logical NOT requires boolean type, found {}",
                        operand_type
                    ));
                    None
                }
            }
            UnaryOperator::Minus => {
                if self.is_numeric_type(&operand_type) {
                    Some(operand_type)
                } else {
                    self.errors.push(format!(
                        "Unary minus requires numeric type, found {}",
                        operand_type
                    ));
                    None
                }
            }
            UnaryOperator::PreIncrement | UnaryOperator::PreDecrement |
            UnaryOperator::PostIncrement | UnaryOperator::PostDecrement => {
                if self.is_numeric_type(&operand_type) {
                    Some(operand_type)
                } else {
                    self.errors.push(format!(
                        "Increment/decrement requires numeric type, found {}",
                        operand_type
                    ));
                    None
                }
            }
        }
    }

    fn infer_array_type(&mut self, array_expr: &ArrayExpr) -> Option<Type> {
        if array_expr.elements.is_empty() {
            // Empty array - we can't infer the type without more context
            self.errors.push("Cannot infer type of empty array".to_string());
            return None;
        }

        let first_type = self.infer_expression_type(&array_expr.elements[0])?;
        
        // Check that all elements have the same type
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
        if let Some(symbol) = self.analyzer.get_symbol(&call_expr.name) {
            if symbol.is_function {
                // TODO: Check argument types against function signature
                // For now, just return the function's return type
                Some(symbol.symbol_type.clone())
            } else {
                self.errors.push(format!(
                    "'{}' is not a function",
                    call_expr.name
                ));
                None
            }
        } else {
            self.errors.push(format!(
                "Undefined function '{}'",
                call_expr.name
            ));
            None
        }
    }

    fn literal_type(&self, literal: &Literal) -> Type {
        match literal {
            Literal::Int(_) => Type::Int,
            Literal::Float(_) => Type::Float,
            Literal::String(_) => Type::String,
            Literal::Char(_) => Type::Char,
            Literal::Bool(_) => Type::Boolean,
            Literal::Null => Type::Void, // Or we could have a separate Null type
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
}

// Helper function to run both semantic analysis and type checking
pub fn analyze_and_check(program: &Program) -> Result<(), Vec<String>> {
    let mut semantic_analyzer = SemanticAnalyzer::new();
    
    // Run integrated semantic analysis and type checking
    semantic_analyzer.analyze_with_type_checking(program)
}