pub mod hydrac_parse;
pub mod hydrac_middle;

// Re-export the main types for easier access
pub use hydrac_parse::lexer::{Lexer, Token, TokenType};
pub use hydrac_parse::parser::{Parser, ast};
pub use hydrac_parse::parser::ast::*;
pub use hydrac_middle::{SemanticAnalyzer, TypeChecker, analyze_and_check};