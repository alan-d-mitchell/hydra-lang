
use std::fmt;

// ============================================================================
// AST DEFINITIONS
// ============================================================================

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
    GlobalVariable(GlobalVariable),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub parameters: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

#[derive(Debug, Clone)]
pub struct GlobalVariable {
    pub is_const: bool,
    pub var_type: Type,
    pub name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub enum Statement {
    VarDecl(VarDecl),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    ForEach(ForEachStmt),
    Return(ReturnStmt),
    Break(BreakStmt),
    Skip(SkipStmt),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub is_const: bool,
    pub var_type: Type,
    pub name: String,
    pub initializer: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct IfStmt {
    pub condition: Expression,
    pub then_branch: Block,
    pub else_branch: Option<Box<Statement>>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Expression,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub initializer: Option<Box<Statement>>,
    pub condition: Option<Expression>,
    pub increment: Option<Expression>,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ForEachStmt {
    pub var_type: Type,
    pub var_name: String,
    pub iterable: Expression,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct BreakStmt {
    pub condition: Option<Expression>, // for "break if"
}

#[derive(Debug, Clone)]
pub struct SkipStmt;

#[derive(Debug, Clone)]
pub enum Expression {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Literal(Literal),
    Identifier(String),
    Array(ArrayExpr),
    FunctionCall(FunctionCallExpr),
    IsIn(IsInExpr),
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub left: Box<Expression>,
    pub operator: BinaryOperator,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct ArrayExpr {
    pub elements: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct FunctionCallExpr {
    pub name: String,
    pub arguments: Vec<Expression>,
}

#[derive(Debug, Clone)]
pub struct IsInExpr {
    pub value: Box<Expression>,
    pub collection: Box<Expression>,
}

#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Null,
}

#[derive(Debug, Clone)]
pub enum BinaryOperator {
    Add, Sub, Mul, Div, Pow,
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    And, Or,
    Assign, AddAssign, SubAssign, MulAssign, DivAssign,
    In,
}

#[derive(Debug, Clone)]
pub enum UnaryOperator {
    Not, Minus, PreIncrement, PreDecrement, PostIncrement, PostDecrement,
}

#[derive(Debug, Clone)]
pub enum Type {
    Int,
    Float,
    String,
    Char,
    Boolean,
    Void,
    Array(Box<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::String => write!(f, "string"),
            Type::Char => write!(f, "char"),
            Type::Boolean => write!(f, "boolean"),
            Type::Void => write!(f, "void"),
            Type::Array(inner) => write!(f, "{}[]", inner),
        }
    }
}