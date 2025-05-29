mod compiler;

use parser::{Lexer, Parser, ast};
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
    
    println!("=== HYDRA LANGUAGE PARSER ===");
    println!("Parsing file: {}", filename);
    println!("File size: {} bytes\n", contents.len());
    
    // Tokenize
    println!("=== LEXICAL ANALYSIS (TOKENIZING) ===");
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
    
    // Parse
    println!("\n=== SYNTAX ANALYSIS (PARSING) ===");
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
    
    // Print AST structure
    println!("\n=== ABSTRACT SYNTAX TREE (AST) ===");
    print_ast_summary(&ast);
    
    // Optionally print full AST (can be very verbose)
    if env::var("HYDRA_VERBOSE").is_ok() {
        println!("\nFull AST:");
        println!("{:#?}", ast);
    }
    
    println!("\n=== PARSING COMPLETE ===");
    println!("🎉 File '{}' parsed successfully!", filename);
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
                println!("     Statements in body: {}", func.body.statements.len());
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