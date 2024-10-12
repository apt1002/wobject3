use welly_parser::welly::{Op, AssignOp};

/// A Literal expression, representing a constant value.
pub enum Literal {
    Integer(u64),
    Character(char),
    String(String),
}

/// An identifier.
pub struct Name(String);

/// An expression that can appear on the left-hand side of an assignment.
pub enum LExpr {
    Name(Name),
    Literal(Literal),
    Field(Box<LExpr>, String),
    Call(Name, Vec<Expr>),
    Tuple(Vec<LExpr>),
    Cast(Box<LExpr>, Box<Expr>),
}

/// An expression.
pub enum Expr {
    Name(Name),
    Literal(Literal),
    Field(Box<Expr>, String),
    Call(Box<Expr>, Vec<Expr>),
    Tuple(Vec<Expr>),
    Cast(Box<LExpr>, Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Function(Option<Name>, Vec<LExpr>, Option<Box<Type>>, Block),
}

/// An [`Expr`] used as a type.
type Type = Expr;

/// An `else` clause.
pub struct Case(LExpr, Block);

/// An `else` clause.
pub struct Else(Block);

/// A statement.
pub enum Stmt {
    Expr(Expr),
    Assign(LExpr, AssignOp, Expr),
    If(Expr, Block, Option<Else>),
    While(Expr, Block, Option<Else>),
    For(LExpr, Expr, Block, Option<Else>),
    Switch(Expr, Vec<Case>, Option<Else>),
}

/// A block of [`Stmt`]s.
pub struct Block(Vec<Stmt>);
 