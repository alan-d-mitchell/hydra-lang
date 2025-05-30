use std::fmt;

// ============================================================================
// TOKEN DEFINITIONS
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),
    
    // Identifiers
    Identifier(String),
    
    // Keywords
    Import,
    If,
    ElseIf,
    Else,
    In,
    Is,
    Break,
    Return,
    Skip,
    BreakIf,
    Func,
    While,
    For,
    ForEach,
    Global,
    Const,
    
    // Types
    IntType,
    StringType,
    CharType,
    FloatType,
    BooleanType,
    VoidType,
    NullType,
    
    // Operators
    Assign,           // =
    Equal,            // ==
    NotEqual,         // !=
    Less,             // <
    LessEqual,        // <=
    Greater,          // >
    GreaterEqual,     // >=
    Plus,             // +
    Minus,            // -
    Multiply,         // *
    Divide,           // /
    Modulo,           // %
    Power,            // **
    Increment,        // ++
    Decrement,        // --
    PlusAssign,       // +=
    MinusAssign,      // -=
    MultiplyAssign,   // *=
    DivideAssign,     // /=
    ModuloAssign,     // %=
    And,              // &&
    Or,               // ||
    Not,              // !
    Arrow,            // ->
    
    // Punctuation
    LeftParen,        // (
    RightParen,       // )
    LeftBrace,        // {
    RightBrace,       // }
    LeftBracket,      // [
    RightBracket,     // ]
    Semicolon,        // ;
    Comma,            // ,
    
    // Special
    Newline,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
}