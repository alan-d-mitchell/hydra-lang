mod compiler;

use compiler::{Lexer, Parser, ast};
use compiler::hydrac_middle::analyze_and_check;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <file.hydra>", args[0]);
        eprintln!("Example: {} tests/test.hydra", args[0]);
        process::exit(1);
    }
    
    let filename = &args[1];
    
    // Read the file
    let contents = match fs::read_to_string(filename) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    
    println!("=== HYDRA LANGUAGE COMPILER v2.0 ===");
    println!("✨ Now with Import & Namespace Support! ✨");
    println!("Compiling file: {}", filename);
    println!("File size: {} bytes\n", contents.len());
    
    // Phase 1: Lexical Analysis
    println!("=== PHASE 1: LEXICAL ANALYSIS (TOKENIZING) ===");
    let mut lexer = Lexer::new(&contents);
    let tokens = match lexer.tokenize() {
        Ok(tokens) => tokens,
        Err(err) => {
            eprintln!("❌ Lexer error: {}", err);
            process::exit(1);
        }
    };
    
    println!("✅ Tokenization successful!");
    println!("Tokens found: {}", tokens.len());
    
    // Count new token types
    let import_tokens = tokens.iter().filter(|t| matches!(t.token_type, compiler::TokenType::Import)).count();
    let namespace_tokens = tokens.iter().filter(|t| matches!(t.token_type, compiler::TokenType::DoubleColon)).count();
    let variadic_tokens = tokens.iter().filter(|t| matches!(t.token_type, compiler::TokenType::DotDotDot)).count();
    
    if import_tokens > 0 || namespace_tokens > 0 || variadic_tokens > 0 {
        println!("🆕 New language features detected:");
        if import_tokens > 0 {
            println!("   📦 Import statements: {}", import_tokens);
        }
        if namespace_tokens > 0 {
            println!("   🔗 Namespace references: {}", namespace_tokens);
        }
        if variadic_tokens > 0 {
            println!("   📋 Variadic functions: {}", variadic_tokens);
        }
    }
    
    // Optionally print all tokens (useful for debugging)
    if env::var("HYDRA_DEBUG").is_ok() {
        println!("\nDebug: All tokens:");
        for (i, token) in tokens.iter().enumerate() {
            println!("  {:3}: {:?}", i, token);
        }
    }
    
    // Phase 2: Syntax Analysis
    println!("\n=== PHASE 2: SYNTAX ANALYSIS (PARSING) ===");
    let mut parser = Parser::new(tokens);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("❌ Parser error: {}", err);
            process::exit(1);
        }
    };
    
    println!("✅ Parsing successful!");
    println!("Program structure:");
    println!("  Imports: {}", ast.imports.len());
    println!("  Functions: {}", 
        ast.items.iter().filter(|item| matches!(item, ast::Item::Function(_))).count());
    println!("  Global variables: {}", 
        ast.items.iter().filter(|item| matches!(item, ast::Item::GlobalVariable(_))).count());
    
    // Print import summary
    if !ast.imports.is_empty() {
        println!("\n📦 Import Summary:");
        for (i, import) in ast.imports.iter().enumerate() {
            let import_type = if import.is_wildcard { "wildcard" } else { "specific" };
            println!("  {}. {} import: {}", i + 1, import_type, import.path.join("::"));
        }
    }
    
    // Print AST summary
    println!("\n=== ABSTRACT SYNTAX TREE (AST) SUMMARY ===");
    print_ast_summary(&ast);
    
    // Phase 3: Semantic Analysis and Type Checking
    println!("\n=== PHASE 3: SEMANTIC ANALYSIS & TYPE CHECKING ===");
    println!("🔍 Analyzing imports and namespaces...");
    println!("🔍 Validating array initializations...");
    println!("🔍 Checking method calls and transformations...");
    
    match analyze_and_check(&ast) {
        Ok(()) => {
            println!("✅ Semantic analysis and type checking successful!");
            println!("   - All symbols properly declared and referenced");
            println!("   - All types are compatible");
            println!("   - Control flow statements are valid");
            println!("   - Import dependencies resolved");
            println!("   - Namespace method calls validated");
            println!("   - Array initializations type-checked");
        }
        Err(errors) => {
            eprintln!("❌ Semantic/Type checking errors found:");
            for (i, error) in errors.iter().enumerate() {
                eprintln!("   {}. {}", i + 1, error);
            }
            
            // Provide helpful hints for common errors
            provide_error_hints(&errors);
            
            process::exit(1);
        }
    }
    
    // Optionally print full AST (can be very verbose)
    if env::var("HYDRA_VERBOSE").is_ok() {
        println!("\n=== FULL AST (VERBOSE) ===");
        println!("{:#?}", ast);
    }
    
    println!("\n=== COMPILATION COMPLETE ===");
    println!("🎉 File '{}' compiled successfully!", filename);
    println!("\nAll compilation phases completed:");
    println!("   ✅ Lexical Analysis");
    println!("   ✅ Syntax Analysis");
    println!("   ✅ Semantic Analysis");
    println!("   ✅ Type Checking");
    println!("   ✅ Import Resolution");
    println!("   ✅ Namespace Validation");
    
    println!("\nNext phases (not yet implemented):");
    println!("   ⏳ Code Generation");
    println!("   ⏳ Optimization");
    println!("   ⏳ Linking");
    
    println!("\nLanguage Features Status:");
    println!("   ✅ Basic types and operations");
    println!("   ✅ Control flow statements");
    println!("   ✅ Functions and recursion");
    println!("   ✅ Arrays and iterations");
    println!("   ✅ Import system");
    println!("   ✅ Namespace support");
    println!("   ✅ Array initialization syntax");
    println!("   ✅ Method call transformations");
    println!("   🔄 Format strings (partial)");
    println!("   🔄 Variadic functions (partial)");
    println!("   ⏳ Standard library implementation");
    
    println!("\nTip: Set environment variables for more output:");
    println!("  HYDRA_DEBUG=1   - Show all tokens");
    println!("  HYDRA_VERBOSE=1 - Show full AST");
}

fn print_ast_summary(program: &ast::Program) {
    // Print imports first
    if !program.imports.is_empty() {
        println!("📦 Imports:");
        for (i, import) in program.imports.iter().enumerate() {
            let import_type = if import.is_wildcard { " (wildcard)" } else { "" };
            println!("  {}. {}{}", i + 1, import.path.join("::"), import_type);
        }
        println!();
    }
    
    // Print items
    for (i, item) in program.items.iter().enumerate() {
        match item {
            ast::Item::Function(func) => {
                let variadic_str = if func.is_variadic { " (variadic)" } else { "" };
                println!("  {}. Function '{}' -> {}{}", 
                    i + 1, func.name, func.return_type, variadic_str);
                println!("     Parameters: {}", func.parameters.len());
                if !func.parameters.is_empty() {
                    for (j, param) in func.parameters.iter().enumerate() {
                        println!("       {}: {} {}", j + 1, param.param_type, param.name);
                    }
                }
                println!("     Statements in body: {}", func.body.statements.len());
                
                // Print statement types
                let mut stmt_counts = std::collections::HashMap::new();
                count_statements(&func.body.statements, &mut stmt_counts);
                if !stmt_counts.is_empty() {
                    println!("     Statement breakdown:");
                    for (stmt_type, count) in stmt_counts {
                        println!("       {}: {}", stmt_type, count);
                    }
                }
            },
            ast::Item::GlobalVariable(var) => {
                let const_str = if var.is_const { "const " } else { "" };
                let init_str = if var.initializer.is_some() { " (initialized)" } else { "" };
                println!("  {}. Global variable {}{} {}{}", 
                    i + 1, const_str, var.var_type, var.name, init_str);
            }
        }
    }
}

fn count_statements(statements: &[ast::Statement], counts: &mut std::collections::HashMap<&str, usize>) {
    for stmt in statements {
        let stmt_type = match stmt {
            ast::Statement::VarDecl(_) => "Variable declarations",
            ast::Statement::If(if_stmt) => {
                count_statements(&if_stmt.then_branch.statements, counts);
                if let Some(else_branch) = &if_stmt.else_branch {
                    count_statements(&vec![(**else_branch).clone()], counts);
                }
                "If statements"
            },
            ast::Statement::While(while_stmt) => {
                count_statements(&while_stmt.body.statements, counts);
                "While loops"
            },
            ast::Statement::For(for_stmt) => {
                count_statements(&for_stmt.body.statements, counts);
                "For loops"
            },
            ast::Statement::ForEach(foreach_stmt) => {
                count_statements(&foreach_stmt.body.statements, counts);
                "ForEach loops"
            },
            ast::Statement::Return(_) => "Return statements",
            ast::Statement::Break(_) => "Break statements",
            ast::Statement::Skip(_) => "Skip statements",
            ast::Statement::Expression(_) => "Expression statements",
        };
        
        *counts.entry(stmt_type).or_insert(0) += 1;
    }
}

fn provide_error_hints(errors: &[String]) {
    println!("\n💡 Helpful hints:");
    
    for error in errors {
        if error.contains("Import not found") {
            println!("   📦 Import Error: Make sure the stdlib directory exists and contains .hydra files");
            println!("      Try: mkdir -p src/stdlib/io && touch src/stdlib/io/stdout.hydra");
        }
        
        if error.contains("Method") && error.contains("not available") {
            println!("   🔗 Method Error: Check if you're calling the correct method for the type");
            println!("      Example: Only strings have toCharArray(), arrays have length(), etc.");
        }
        
        if error.contains("Array size must be of type int") {
            println!("   📋 Array Init Error: Use integer literals for array size");
            println!("      Correct: int[] arr = {{int, 5}}");
            println!("      Wrong: int[] arr = {{int, \"5\"}}");
        }
        
        if error.contains("cannot initialize") && error.contains("with") {
            println!("   🎯 Type Mismatch: Make sure array element type matches declaration");
            println!("      Correct: char[] arr = {{char, 5}}");
            println!("      Wrong: char[] arr = {{string, 5}}");
        }
        
        if error.contains("Format") {
            println!("   📝 Format String Error: Match format specifiers with argument types");
            println!("      %d for int, %s for string, %f for float, %c for char, %b for boolean");
        }
    }
}