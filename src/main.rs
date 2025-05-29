mod compiler;

use compiler::{Lexer, Parser, ast};
use compiler::hydrac_middle::analyze_and_check;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <file.hyd>", args[0]);
        eprintln!("Example: {} tests/test.hyd", args[0]);
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
    
    println!("=== HYDRA LANGUAGE COMPILER ===");
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
    println!("  Functions: {}", 
        ast.items.iter().filter(|item| matches!(item, ast::Item::Function(_))).count());
    println!("  Global variables: {}", 
        ast.items.iter().filter(|item| matches!(item, ast::Item::GlobalVariable(_))).count());
    
    // Print AST summary
    println!("\n=== ABSTRACT SYNTAX TREE (AST) SUMMARY ===");
    print_ast_summary(&ast);
    
    // Phase 3: Semantic Analysis and Type Checking
    println!("\n=== PHASE 3: SEMANTIC ANALYSIS & TYPE CHECKING ===");
    match analyze_and_check(&ast) {
        Ok(()) => {
            println!("✅ Semantic analysis and type checking successful!");
            println!("   - All symbols properly declared and referenced");
            println!("   - All types are compatible");
            println!("   - Control flow statements are valid");
        }
        Err(errors) => {
            eprintln!("❌ Semantic/Type checking errors found:");
            for (i, error) in errors.iter().enumerate() {
                eprintln!("   {}. {}", i + 1, error);
            }
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
    println!("\nNext phases (not yet implemented):");
    println!("   ⏳ Code Generation");
    println!("   ⏳ Optimization");
    println!("   ⏳ Linking");
    
    println!("\nTip: Set environment variables for more output:");
    println!("  HYDRA_DEBUG=1   - Show all tokens");
    println!("  HYDRA_VERBOSE=1 - Show full AST");
}

fn print_ast_summary(program: &ast::Program) {
    for (i, item) in program.items.iter().enumerate() {
        match item {
            ast::Item::Function(func) => {
                println!("  {}. Function '{}' -> {}", 
                    i + 1, func.name, func.return_type);
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