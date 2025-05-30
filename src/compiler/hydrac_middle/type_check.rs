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
        
        // For variadic functions, we need special handling
        if func.is_variadic {
            self.check_variadic_function(func);
        } else {
            self.check_block(&func.body);
        }
        
        self.current_function_return_type = None;
    }
    
    fn check_variadic_function(&mut self, func: &Function) {
        // Variadic functions need special validation
        // For now, we'll just check the body normally
        self.check_block(&func.body);
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
                } else if self.analyzer.imported_symbols.contains_key(name) {
                    // For imported symbols, we need to determine their type
                    // For now, assume functions return void (this would be improved with proper import metadata)
                    Some(Type::Void)
                } else {
                    self.errors.push(format!("Undefined identifier '{}'", name));
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
            Expression::ArrayInit(array_init) => {
                self.infer_array_init_type(array_init)
            }
            Expression::FunctionCall(call_expr) => {
                self.infer_function_call_type(call_expr)
            }
            Expression::NamespacedCall(namespaced_call) => {
                self.infer_namespaced_call_type(namespaced_call)
            }
            Expression::IsIn(_) => {
                Some(Type::Boolean)
            }
            Expression::FormatString(format_expr) => {
                self.check_format_string(format_expr);
                Some(Type::Void) // format functions typically return void
            }
        }
    }
    
    fn infer_array_init_type(&mut self, array_init: &ArrayInitExpr) -> Option<Type> {
        // Check that size is an integer
        let size_type = self.infer_expression_type(&array_init.size);
        if let Some(size_type) = size_type {
            if !matches!(size_type, Type::Int) {
                self.errors.push(format!(
                    "Array size must be of type int, found {}",
                    size_type
                ));
            }
        }
        
        Some(Type::Array(Box::new(array_init.element_type.clone())))
    }
    
    fn infer_namespaced_call_type(&mut self, namespaced_call: &NamespacedCallExpr) -> Option<Type> {
        // First, get the type of the object
        let object_type = self.infer_expression_type(&namespaced_call.object)?;
        
        // Check if the method is available for this type
        let type_name = format!("{}", object_type);
        let available_methods = self.analyzer.get_available_methods_for_type(&type_name);
        
        if !available_methods.contains(&namespaced_call.method) {
            self.errors.push(format!(
                "Method '{}' is not available for type {}. Available methods: {}",
                namespaced_call.method, type_name, available_methods.join(", ")
            ));
            return None;
        }
        
        // Analyze arguments
        for arg in &namespaced_call.arguments {
            self.infer_expression_type(arg);
        }
        
        // Return type depends on the method
        match namespaced_call.method.as_str() {
            "length" => Some(Type::Int),
            "toString" => Some(Type::String),
            "toCharArray" => Some(Type::Array(Box::new(Type::Char))),
            "toLowerCase" | "toUpperCase" => Some(Type::String),
            "equals" => Some(Type::Boolean),
            "isUpper" | "isLower" => Some(Type::Boolean),
            _ => {
                // For unknown methods, try to infer from stdlib
                self.infer_stdlib_method_type(&namespaced_call.method)
            }
        }
    }
    
    fn infer_stdlib_method_type(&self, method_name: &str) -> Option<Type> {
        // This would be expanded based on the actual stdlib methods
        match method_name {
            "parseInt" => Some(Type::Int),
            "parseFloat" => Some(Type::Float),
            _ => Some(Type::Void), // Default for unknown methods
        }
    }
    
    fn check_format_string(&mut self, format_expr: &FormatStringExpr) {
        let (specifiers, format_errors) = self.analyzer.validate_format_string(&format_expr.format_string);
        
        // Add any format string parsing errors to our errors
        self.errors.extend(format_errors);
        
        // Check that the number of arguments matches the number of specifiers
        if specifiers.len() != format_expr.arguments.len() {
            self.errors.push(format!(
                "Format string expects {} arguments, but {} provided",
                specifiers.len(), format_expr.arguments.len()
            ));
            return;
        }
        
        // Check that each argument type matches the corresponding format specifier
        for (i, (spec, arg)) in specifiers.iter().zip(&format_expr.arguments).enumerate() {
            let arg_type = self.infer_expression_type(arg);
            if let Some(arg_type) = arg_type {
                let expected_type = spec.expected_type();
                if !self.types_compatible(&expected_type, &arg_type) {
                    self.errors.push(format!(
                        "Format argument {} type mismatch: expected {} for %{:?}, found {}",
                        i + 1, expected_type, spec, arg_type
                    ));
                }
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
                        // For other arithmetic operations, promote to float if either operand is float
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
        // Handle namespaced function calls
        if let Some(ref namespace) = call_expr.namespace {
            // Check if it's a valid imported function
            let namespaced_name = format!("{}::{}", namespace.join("::"), call_expr.name);
            
            // For stdlib functions, we can hardcode some return types
            if namespace.get(0) == Some(&"stdlib".to_string()) {
                match namespace.get(1).map(|s| s.as_str()) {
                    Some("math") => {
                        match call_expr.name.as_str() {
                            "sqrt" | "pow" | "ceil" | "floor" => return Some(Type::Float),
                            _ => {}
                        }
                    }
                    Some("string") => {
                        match call_expr.name.as_str() {
                            "length" => return Some(Type::Int),
                            "equals" => return Some(Type::Boolean),
                            "parseFloat" => return Some(Type::Float),
                            "parseInt" => return Some(Type::Int),
                            "toCharArray" => return Some(Type::Array(Box::new(Type::Char))),
                            "toLowerCase" | "toUpperCase" => return Some(Type::String),
                            _ => {}
                        }
                    }
                    Some("array") => {
                        match call_expr.name.as_str() {
                            "len" => return Some(Type::Int),
                            "toString" => return Some(Type::String),
                            _ => {}
                        }
                    }
                    Some("io") => {
                        match call_expr.name.as_str() {
                            "stdout" | "stdin" => return Some(Type::Void),
                            _ => {}
                        }
                    }
                    _ => {}
                }
            }
            
            self.errors.push(format!("Unknown namespaced function '{}'", namespaced_name));
            return None;
        }
        
        // Handle local function calls
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
        } else if self.analyzer.imported_symbols.contains_key(&call_expr.name) {
            // Handle imported functions
            // For now, assume they return void (this would be improved with proper metadata)
            Some(Type::Void)
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
            (Type::Variadic(expected_inner), actual) => {
                // Variadic types can accept the base type or arrays of the base type
                self.types_compatible(expected_inner, actual) ||
                self.types_compatible(&Type::Array(expected_inner.clone()), actual)
            }
            _ => false,
        }
    }
}

// Enhanced helper function to run both semantic analysis and type checking with import support
pub fn analyze_and_check(program: &Program) -> Result<(), Vec<String>> {
    let mut semantic_analyzer = SemanticAnalyzer::new();
    
    // Run semantic analysis (which now includes import processing)
    semantic_analyzer.analyze(program)?;
    
    // Run type checking
    let mut type_checker = TypeChecker::new(&semantic_analyzer);
    type_checker.check(program)
}