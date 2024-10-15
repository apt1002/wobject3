use welly_parser::{welly, Location, Tree};
use welly::{Op, AssignOp};
use super::{Invalid, AST};

/// Validates an optional `tree`, given a callback that validates a `tree`.
fn optional<T, A>(
    tree: &Option<T>,
    validate: impl FnOnce(&T) -> Result<A, Invalid>,
) -> Result<Option<A>, Invalid> {
    Ok(if let Some(tree) = tree { Some(validate(tree)?) } else { None })
}

/// Reports an error if `tree` is `None`.
fn compulsory<T>(
    tree: &Option<T>,
    missing: impl FnOnce(),
) -> Result<&T, Invalid> {
    Ok(tree.as_ref().ok_or_else(missing)?)
}

// ----------------------------------------------------------------------------

/// A Literal expression, representing a constant value.
pub enum Literal {
    Int(u64),
    Char(char),
    Str(String),
}

impl Literal {
    /// Returns `value` as `Self` if possible, otherwise reports an error.
    fn validate_integer(report: &mut impl FnMut(Location, &str), loc: Location, value: &str)
    -> Result<Self, Invalid> {
        if let Ok(i) = value.parse::<u64>() { return Ok(Self::Int(i)); }
        if let Ok(i) = value.parse::<i64>() { return Ok(Self::Int(i as u64)); }
        Err(report(loc, "Invalid integer literal"))?
    }
}

// ----------------------------------------------------------------------------

/// An valid identifier.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name(String);

impl std::borrow::Borrow<str> for Name {
    fn borrow(&self) -> &str { self.0.borrow() }
}

impl Name {
    /// Returns `value` as `Self` if possible.
    fn maybe_validate(value: &str) -> Option<Self> {
        let mut cs = value.chars();
        if let Some(c) = cs.next() {
            if !matches!(c, '_' | 'A'..='Z' | 'a'..='z') { return None; }
            while let Some(c) = cs.next() {
                if !matches!(c, '_' | '0'..='9' | 'A'..='Z' | 'a'..='z') { return None; }
            }
            Some(Name(value.into()))
        } else { None }
    }

    /// Returns `value` as `Self` if possible, otherwise reports an error.
    pub fn validate(report: &mut impl FnMut(Location, &str), loc: Location, value: &str)
    -> Result<Self, Invalid> {
        Ok(Name::maybe_validate(value).ok_or_else(|| report(loc, "Invalid identifier"))?)
    }
}

// ----------------------------------------------------------------------------

/// An expression that can appear on the left-hand side of an assignment.
pub enum LExpr {
    Name(Name),
    Literal(Literal),
    Field(Box<LExpr>, Name),
    Call(Name, Vec<Expr>),
    Tuple(Vec<LExpr>),
    Cast(Box<LExpr>, Box<Expr>),
}

impl LExpr {
    pub fn validate_expr(report: &mut impl FnMut(Location, &str), loc: Location, tree: &welly::Expr)
    -> Result<Box<Self>, Invalid> {
        Ok(Box::new(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.into())),
            welly::Expr::Name(s) => {
                if let Some(c) = s.chars().next() {
                    if matches!(c, '0'..='9') {
                        Self::Literal(Literal::validate_integer(report, loc, s)?)
                    } else {
                        Self::Name(Name::validate(report, loc, s)?)
                    }
                } else { /* Impossible? */ Err(report(loc, "Empty name"))? }
            },
            welly::Expr::Round(_round) => {
                todo!();
            },
            welly::Expr::Op(left, op, right) => match op {
                Op::Cast => {
                    let left = compulsory(left, || report(loc, "Missing expression"))?;
                    let right = compulsory(right, || report(loc, "Missing expression"))?;
                    let left = LExpr::validate_expr(report, loc, &*left);
                    let right = Type::validate_expr(report, loc, &*right);
                    Self::Cast(left?, right?)
                },
                Op::Missing => Err(report(loc, "Missing operator"))?,
                _ => Err(report(loc, "This operator does not make an assignable expression"))?,
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(loc, "Missing expression before `.field`"))?;
                let object = LExpr::validate_expr(report, loc, &*object);
                let field = Name::validate(report, loc, field);
                Self::Field(object?, field?)
            },
            _ => Err(report(loc, "Expression is not assignable"))?,
        }))
    }
}

impl AST for Box<LExpr> {
    fn validate(report: &mut impl FnMut(Location, &str), loc: Location, tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            LExpr::validate_expr(report, loc, tree)
        } else {
            Err(report(loc, "Not an expression"))?
        }
    }
}

// ----------------------------------------------------------------------------

/// An expression.
pub enum Expr {
    Name(Name),
    Literal(Literal),
    Field(Box<Expr>, Name),
    Call(Box<Expr>, Vec<Expr>),
    Tuple(Vec<Expr>),
    Cast(Box<LExpr>, Box<Expr>),
    Op(Box<Expr>, Op, Box<Expr>),
    Function(Option<Name>, Vec<LExpr>, Option<Box<Type>>, Block),
}

impl Expr {
    pub fn validate_expr(report: &mut impl FnMut(Location, &str), loc: Location, tree: &welly::Expr)
    -> Result<Box<Self>, Invalid> {
        Ok(Box::new(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.into())),
            welly::Expr::Name(s) => {
                if let Some(c) = s.chars().next() {
                    if matches!(c, '0'..='9') {
                        Self::Literal(Literal::validate_integer(report, loc, s)?)
                    } else {
                        Self::Name(Name::validate(report, loc, s)?)
                    }
                } else { /* Impossible? */ Err(report(loc, "Empty name"))? }
            },
            welly::Expr::Round(_round) => {
                todo!();
            },
            welly::Expr::Function(name, _params, return_type, body) => {
                let _name = optional(name, |n| Name::validate(report, loc, n));
                // TODO: params.
                let _return_type = optional(return_type, |rt| Expr::validate_expr(report, loc, rt));
                let _body = optional(body, |b| Block::validate_brace(report, loc, b));
                todo!();
            },
            welly::Expr::Op(left, op, right) => match op {
                Op::Cast => {
                    let left = compulsory(left, || report(loc, "Missing expression"))?;
                    let right = compulsory(right, || report(loc, "Missing expression"))?;
                    let left = Expr::validate_expr(report, loc, left);
                    let right = Type::validate_expr(report, loc, right);
                    Self::Op(left?, *op, right?)
                },
                Op::Missing => Err(report(loc, "Missing operator"))?,
                _ => { todo!() },
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(loc, "Missing expression before `.field`"))?;
                let object = Expr::validate_expr(report, loc, &*object);
                let field = Name::validate(report, loc, field);
                Self::Field(object?, field?)
            },
            welly::Expr::Call(_fn, _args) => {
                todo!();
            },
        }))
    }
}

impl AST for Box<Expr> {
    fn validate(report: &mut impl FnMut(Location, &str), loc: Location, tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            Expr::validate_expr(report, loc, tree)
        } else {
            Err(report(loc, "Not an expression"))?
        }
    }
}

/// An [`Expr`] used as a type.
type Type = Expr;

// ----------------------------------------------------------------------------

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

impl Stmt {
    fn validate_stmt(_report: &mut impl FnMut(Location, &str), _loc: Location, _tree: &welly::Stmt)
    -> Result<Box<Self>, Invalid> {
        todo!();
    }
}

impl AST for Box<Stmt> {
    fn validate(report: &mut impl FnMut(Location, &str), loc: Location, tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Stmt>() {
            Stmt::validate_stmt(report, loc, tree)
        } else {
            Err(report(loc, "Not a statement"))?
        }
    }
}

// ----------------------------------------------------------------------------

/// A block of [`Stmt`]s.
pub struct Block(Vec<Stmt>);

impl Block {
    pub fn validate_brace(_report: impl FnMut(Location, &str), _loc: Location, _tree: &welly::Brace)
    -> Result<Self, Invalid> {
        todo!();
    }
}