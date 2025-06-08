
use std::fmt;

// ===========================================================================
// TOKEN DEFINITIONS
// ===========================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Literals
    IntLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    CharLiteral(char),
    BoolLiteral(bool),

    // Identifer
    Identifier(String),

    // Keywords
    If,
    ElseIf,                 
    Else,                  
    In,                    
    Is,                     
    Break,
    Return,
    Skip,
    Function,
    While,
    For,
    ForEach,
    Const,
    Let,
    Null,

    // Types
    Struct,
    I32,        // 32-bit int -> default for int
    I64,        // 64-bit int
    F32,        // 32-bit float -> default for float
    F64,        // 64-bit float
    CharType,   
    StringType,     
    BooleanType,
    VoidType,

    // Operators
    Assign,           // =
    Equal,            // ==
    NotEqual,         // !=
    // Less,             // <
    LessEqual,        // <=
    // Greater,          // >
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
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    LeftAngle,
    RightAngle,
    HeapPointerBar,     // |
    Semicolon,
    Comma,
    Colon,              // :
    DoubleColon,        // ::
    Optional,           // ?
    Reference,          // &

    // Special
    Newline,
    Eof,
}

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub lexeme: &'a str,
    pub line: usize,
    pub column: usize,
}