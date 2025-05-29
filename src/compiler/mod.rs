pub mod token;
pub mod ast;
pub mod lexer;
pub mod parser;

// Re-export the main types for easier access
pub use token::{Token, TokenType};
pub use ast::*;
pub use lexer::Lexer;
pub use parser::Parser;