use crate::compiler::hydrac_parse::parser::ast::*;
use std::collections::HashMap;
use std::path::Path;

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
    pub is_variadic: bool,
    pub line: usize,
    pub column: usize,
    pub namespace: Option<Vec<String>>,
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

#[derive(Debug, Clone)]
pub struct ImportResolver {
    pub stdlib_path: String,
    pub project_root: String,
}

impl ImportResolver {
    pub fn new(stdlib_path: &str, project_root: &str) -> Self {
        Self {
            stdlib_path: stdlib_path.to_string(),
            project_root: project_root.to_string(),
        }
    }
    
    pub fn resolve_import(&self, import: &ImportStmt) -> Result<Vec<String>, String> {
        let path_str = import.path.join("::");
        
        // Try as stdlib import first
        if import.path.get(0) == Some(&"stdlib".to_string()) {
            let file_path = format!("{}/{}.hydra", self.stdlib_path, import.path[1..].join("/"));
            if Path::new(&file_path).exists() {
                return Ok(vec![file_path]);
            }
            
            // Try as directory
            let dir_path = format!("{}/{}", self.stdlib_path, import.path[1..].join("/"));
            if Path::new(&dir_path).is_dir() {
                return self.get_files_in_directory(&dir_path);
            }
        }
        
        // Try as project-relative import
        let file_path = format!("{}/{}.hydra", self.project_root, import.path.join("/"));
        if Path::new(&file_path).exists() {
            return Ok(vec![file_path]);
        }
        
        // Try as directory
        let dir_path = format!("{}/{}", self.project_root, import.path.join("/"));
        if Path::new(&dir_path).is_dir() {
            return self.get_files_in_directory(&dir_path);
        }
        
        Err(format!("Import not found: {}", path_str))
    }
    
    fn get_files_in_directory(&self, dir_path: &str) -> Result<Vec<String>, String> {
        let mut files = Vec::new();
        
        match std::fs::read_dir(dir_path) {
            Ok(entries) => {
                for entry in entries {
                    if let Ok(entry) = entry {
                        let path = entry.path();
                        if path.extension().and_then(|s| s.to_str()) == Some("hydra") {
                            if let Some(path_str) = path.to_str() {
                                files.push(path_str.to_string());
                            }
                        }
                    }
                }
            }
            Err(_) => return Err(format!("Cannot read directory: {}", dir_path)),
        }
        
        if files.is_empty() {
            Err(format!("No .hydra files found in directory: {}", dir_path))
        } else {
            Ok(files)
        }
    }
}

pub struct SemanticAnalyzer {
    pub global_scope: Scope,
    pub current_scope: Option<Box<Scope>>,
    pub errors: Vec<String>,
    pub current_function: Option<String>,
    pub in_loop: bool,
    pub imports: Vec<ImportStmt>,
    pub imported_symbols: HashMap<String, ImportedSymbol>,
    pub import_resolver: ImportResolver,
    pub type_methods: HashMap<String, Vec<String>>, // type -> available methods
}

impl SemanticAnalyzer {
    pub fn new() -> Self {
        let mut analyzer = Self {
            global_scope: Scope::new(),
            current_scope: None,
            errors: Vec::new(),
            current_function: None,
            in_loop: false,
            imports: Vec::new(),
            imported_symbols: HashMap::new(),
            import_resolver: ImportResolver::new("src/stdlib", "."),
            type_methods: HashMap::new(),
        };
        
        // Initialize built-in type methods
        analyzer.init_builtin_type_methods();
        
        analyzer
    }
    
    fn init_builtin_type_methods(&mut self) {
        // String methods
        self.type_methods.insert("string".to_string(), vec![
            "length".to_string(),
            "toCharArray".to_string(),
            "toLowerCase".to_string(),
            "toUpperCase".to_string(),
            "equals".to_string(),
            "toString".to_string(),
        ]);
        
        // Array methods
        self.type_methods.insert("int[]".to_string(), vec![
            "length".to_string(),
            "toString".to_string(),
        ]);
        self.type_methods.insert("float[]".to_string(), vec![
            "length".to_string(),
            "toString".to_string(),
        ]);
        self.type_methods.insert("string[]".to_string(), vec![
            "length".to_string(),
            "toString".to_string(),
        ]);
        self.type_methods.insert("char[]".to_string(), vec![
            "length".to_string(),
            "toString".to_string(),
            "isUpper".to_string(),
            "isLower".to_string(),
        ]);
        self.type_methods.insert("boolean[]".to_string(), vec![
            "length".to_string(),
            "toString".to_string(),
        ]);
    }

    pub fn analyze(&mut self, program: &Program) -> Result<(), Vec<String>> {
        // First, process all imports
        for import in &program.imports {
            self.process_import(import)?;
        }
        
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
                        is_variadic: func.is_variadic,
                        line: 0, // We don't have line info in AST yet
                        column: 0,
                        namespace: None,
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
                        is_variadic: false,
                        line: 0,
                        column: 0,
                        namespace: None,
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
    
    fn process_import(&mut self, import: &ImportStmt) -> Result<(), Vec<String>> {
        self.imports.push(import.clone());
        
        // For now, we'll simulate the import resolution
        // In a real implementation, you'd parse the imported files
        match self.import_resolver.resolve_import(import) {
            Ok(files) => {
                for file_path in files {
                    self.process_imported_file(&file_path, import)?;
                }
            }
            Err(e) => {
                self.errors.push(format!("Import error: {}", e));
            }
        }
        
        Ok(())
    }
    
    fn process_imported_file(&mut self, _file_path: &str, import: &ImportStmt) -> Result<(), Vec<String>> {
        // For now, we'll add some standard library functions based on the import path
        // In a real implementation, you'd parse the imported file and extract its symbols
        
        if import.path.get(0) == Some(&"stdlib".to_string()) {
            match import.path.get(1).map(|s| s.as_str()) {
                Some("io") => {
                    if import.is_wildcard || import.path.get(2) == Some(&"stdout".to_string()) {
                        self.imported_symbols.insert("stdout".to_string(), ImportedSymbol {
                            name: "stdout".to_string(),
                            namespace: vec!["stdlib".to_string(), "io".to_string()],
                            symbol_type: ImportedSymbolType::Function,
                        });
                    }
                    if import.is_wildcard || import.path.get(2) == Some(&"stdin".to_string()) {
                        self.imported_symbols.insert("stdin".to_string(), ImportedSymbol {
                            name: "stdin".to_string(),
                            namespace: vec!["stdlib".to_string(), "io".to_string()],
                            symbol_type: ImportedSymbolType::Function,
                        });
                    }
                }
                Some("math") => {
                    let math_functions = vec!["sqrt", "pow", "ceil", "floor"];
                    for func in math_functions {
                        if import.is_wildcard || import.path.get(2) == Some(&func.to_string()) {
                            self.imported_symbols.insert(func.to_string(), ImportedSymbol {
                                name: func.to_string(),
                                namespace: vec!["stdlib".to_string(), "math".to_string()],
                                symbol_type: ImportedSymbolType::Function,
                            });
                        }
                    }
                }
                Some("string") => {
                    let string_functions = vec!["length", "equals", "parseFloat", "parseInt", 
                                              "toCharArray", "toLowerCase", "toUpperCase"];
                    for func in string_functions {
                        if import.is_wildcard || import.path.get(2) == Some(&func.to_string()) {
                            self.imported_symbols.insert(func.to_string(), ImportedSymbol {
                                name: func.to_string(),
                                namespace: vec!["stdlib".to_string(), "string".to_string()],
                                symbol_type: ImportedSymbolType::Function,
                            });
                        }
                    }
                }
                Some("array") => {
                    let array_functions = vec!["len", "toString"];
                    for func in array_functions {
                        if import.is_wildcard || import.path.get(2) == Some(&func.to_string()) {
                            self.imported_symbols.insert(func.to_string(), ImportedSymbol {
                                name: func.to_string(),
                                namespace: vec!["stdlib".to_string(), "array".to_string()],
                                symbol_type: ImportedSymbolType::Function,
                            });
                        }
                    }
                }
                _ => {}
            }
        }
        
        Ok(())
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
                is_variadic: false,
                line: 0,
                column: 0,
                namespace: None,
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
                    is_variadic: false,
                    line: 0,
                    column: 0,
                    namespace: None,
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
            is_variadic: false,
            line: 0,
            column: 0,
            namespace: None,
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
                    for (symbol_name, _) in &self.imported_symbols {
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
            Expression::ArrayInit(array_init) => {
                // Validate that size expression is an integer
                self.analyze_expression(&array_init.size);
            }
            Expression::FunctionCall(call_expr) => {
                // Check if function exists (either local or imported)
                let is_defined = if let Some(ref namespace) = call_expr.namespace {
                    self.is_namespaced_function_defined(namespace, &call_expr.name)
                } else {
                    self.is_function_defined(&call_expr.name) || 
                    self.imported_symbols.contains_key(&call_expr.name)
                };
                
                if !is_defined {
                    if let Some(ref namespace) = call_expr.namespace {
                        self.errors.push(format!("Undefined function '{}::{}' ", 
                                               namespace.join("::"), call_expr.name));
                    } else {
                        self.errors.push(format!("Undefined function '{}'", call_expr.name));
                    }
                }
                
                // Analyze arguments
                for arg in &call_expr.arguments {
                    self.analyze_expression(arg);
                }
            }
            Expression::NamespacedCall(namespaced_call) => {
                // Analyze the object expression first
                self.analyze_expression(&namespaced_call.object);
                
                // For now, we assume this is a method call transformation
                // In type checking, we'll validate that the method exists for the object's type
                
                // Analyze arguments
                for arg in &namespaced_call.arguments {
                    self.analyze_expression(arg);
                }
            }
            Expression::IsIn(is_in_expr) => {
                self.analyze_expression(&is_in_expr.value);
                self.analyze_expression(&is_in_expr.collection);
            }
            Expression::FormatString(format_expr) => {
                // Validate format string structure
                self.validate_format_string(&format_expr.format_string);
                
                // Analyze all arguments
                for arg in &format_expr.arguments {
                    self.analyze_expression(arg);
                }
            }
        }
    }
    
    fn validate_format_string(&mut self, format_str: &str) -> Vec<FormatSpecifier> {
        let mut specifiers = Vec::new();
        let mut chars = format_str.chars().peekable();
        
        while let Some(ch) = chars.next() {
            if ch == '%' {
                if let Some(&next_ch) = chars.peek() {
                    if next_ch == '%' {
                        // Escaped %
                        chars.next();
                        continue;
                    }
                    
                    // Find the format specifier
                    if let Some(spec_ch) = chars.next() {
                        if let Some(spec) = FormatSpecifier::from_str(&spec_ch.to_string()) {
                            specifiers.push(spec);
                        } else {
                            self.errors.push(format!("Unknown format specifier '%{}'", spec_ch));
                        }
                    }
                }
            }
        }
        
        specifiers
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
        if self.global_scope.lookup(name).is_some() {
            return true;
        }
        
        // Finally check imported symbols
        self.imported_symbols.contains_key(name)
    }

    fn is_function_defined(&self, name: &str) -> bool {
        // Check global scope for functions
        if let Some(symbol) = self.global_scope.lookup(name) {
            symbol.is_function
        } else {
            false
        }
    }
    
    fn is_namespaced_function_defined(&self, namespace: &[String], name: &str) -> bool {
        // Check if the function exists in the given namespace
        for (_, imported_symbol) in &self.imported_symbols {
            if imported_symbol.name == name && imported_symbol.namespace == namespace {
                return matches!(imported_symbol.symbol_type, ImportedSymbolType::Function);
            }
        }
        false
    }
    
    pub fn get_available_methods_for_type(&self, type_name: &str) -> Vec<String> {
        self.type_methods.get(type_name).cloned().unwrap_or_default()
    }
    
    pub fn transform_namespaced_call(&self, call: &NamespacedCallExpr, object_type: &Type) -> FunctionCallExpr {
        // Transform var::method() to type::method(var)
        let type_name = format!("{}", object_type);
        let namespace = vec!["stdlib".to_string(), type_name];
        
        let mut arguments = vec![*call.object.clone()];
        arguments.extend(call.arguments.clone());
        
        FunctionCallExpr {
            name: call.method.clone(),
            namespace: Some(namespace),
            arguments,
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