use std::fmt;

// ============================================
// AST DEFINITIONS
// ============================================

#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone)]
pub enum Item {
    Function(function),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Type,
    pub body: Block,
}

#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
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
    pub init: Option<Expression>,
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
    ArrayInit(ArrayInitExpr),
    FunctionCall(FunctionCallExpr),
    IsIn(IsInExpr),
    FormatString(FormatStringExpr),
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
pub struct ArrayInitExpr {
    pub element_type: Type,
    pub size: Box<Expression>,
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
pub struct FormatStringExpr {
    pub format_string: String,
    pub arguments: Vec<Expression>,
}

