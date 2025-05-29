pub mod semantic_analysis;
pub mod type_check;

// Re-export for easier access
pub use semantic_analysis::{SemanticAnalyzer, Symbol, Scope};
pub use type_check::{TypeChecker, analyze_and_check};