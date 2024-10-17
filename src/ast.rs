use welly_parser::{welly, Tree, Location, Loc};
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
    Int(Loc<u64>),
    Char(Loc<char>),
    Str(Loc<String>),
}

impl Literal {
    /// Returns `value` as `Self` if possible, otherwise reports an error.
    fn validate_integer(report: &mut impl FnMut(Location, &str), value: &Loc<String>)
    -> Result<Self, Invalid> {
        let loc = Loc::location(value);
        if let Ok(i) = value.parse::<u64>() { return Ok(Self::Int(Loc::new(i, loc))); }
        if let Ok(i) = value.parse::<i64>() { return Ok(Self::Int(Loc::new(i as u64, loc))); }
        Err(report(loc, "Invalid integer literal"))?
    }
}

// ----------------------------------------------------------------------------

/// An valid identifier.
#[derive(Debug, Clone)]
pub struct Name(Loc<String>);

impl std::borrow::Borrow<str> for Name {
    fn borrow(&self) -> &str { self.0.borrow() }
}

impl Name {
    /// Returns `value` as `Self` if possible.
    fn maybe_validate(value: &Loc<String>) -> Option<Self> {
        let mut cs = value.chars();
        if let Some(c) = cs.next() {
            if !matches!(c, '_' | 'A'..='Z' | 'a'..='z') { return None; }
            while let Some(c) = cs.next() {
                if !matches!(c, '_' | '0'..='9' | 'A'..='Z' | 'a'..='z') { return None; }
            }
            Some(Name(value.clone()))
        } else { None }
    }

    /// Returns `value` as `Self` if possible, otherwise reports an error.
    pub fn validate(report: &mut impl FnMut(Location, &str), value: &Loc<String>)
    -> Result<Self, Invalid> {
        Ok(Name::maybe_validate(value).ok_or_else(
            || report(Loc::location(value), "Invalid identifier")
        )?)
    }
}

// ----------------------------------------------------------------------------

/// An expression that can appear on the left-hand side of an assignment.
pub enum LExpr {
    Name(Name),
    Literal(Literal),
    Field(Box<LExpr>, Name),
    Call(Name, Loc<Vec<Expr>>),
    Tuple(Loc<Vec<LExpr>>),
    Cast(Box<LExpr>, Location, Box<Expr>),
}

impl LExpr {
    pub fn validate_expr(report: &mut impl FnMut(Location, &str), tree: &welly::Expr)
    -> Result<Box<Self>, Invalid> {
        Ok(Box::new(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                if let Some(c) = s.chars().next() {
                    if matches!(c, '0'..='9') {
                        Self::Literal(Literal::validate_integer(report, s)?)
                    } else {
                        Self::Name(Name::validate(report, s)?)
                    }
                } else { /* Impossible? */ Err(report(Loc::location(s), "Empty name"))? }
            },
            welly::Expr::Round(_round) => {
                todo!();
            },
            welly::Expr::Op(left, op, right) => {
                let loc = Loc::location(op);
                match &**op {
                    Op::Cast => {
                        let left = compulsory(left, || report(loc, "Missing left operand"))?;
                        let right = compulsory(right, || report(loc, "Missing right operand"))?;
                        let left = LExpr::validate_expr(report, &*left);
                        let right = Type::validate_expr(report, &*right);
                        Self::Cast(left?, loc, right?)
                    },
                    Op::Missing => Err(report(loc, "Missing operator"))?,
                    _ => Err(report(loc, "This operator does not make an assignable expression"))?,
                }
            },
            welly::Expr::Field(object, field) => {
                let loc = Loc::location(field);
                let object = compulsory(object, || report(loc, "Missing expression before `.field`"))?;
                let object = LExpr::validate_expr(report, &*object);
                let field = Name::validate(report, field);
                Self::Field(object?, field?)
            },
            _ => Err(report(Location::EVERYWHERE, "Expression is not assignable"))?,
        }))
    }
}

impl AST for Box<LExpr> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            LExpr::validate_expr(report, tree)
        } else {
            Err(report(Location::EVERYWHERE, "Not an expression"))?
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
    Op(Box<Expr>, Loc<Op>, Box<Expr>),
    Function(Option<Name>, Vec<LExpr>, Option<Box<Type>>, Block),
}

impl Expr {
    pub fn validate_expr(report: &mut impl FnMut(Location, &str), tree: &welly::Expr)
    -> Result<Box<Self>, Invalid> {
        Ok(Box::new(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                if let Some(c) = s.chars().next() {
                    if matches!(c, '0'..='9') {
                        Self::Literal(Literal::validate_integer(report, s)?)
                    } else {
                        Self::Name(Name::validate(report, s)?)
                    }
                } else { /* Impossible? */ Err(report(Loc::location(s), "Empty name"))? }
            },
            welly::Expr::Round(_round) => {
                todo!();
            },
            welly::Expr::Function(name, _params, return_type, body) => {
                let _name = optional(name, |n| Name::validate(report, n));
                // TODO: params.
                let _return_type = optional(return_type, |rt| Expr::validate_expr(report, rt));
                let _body = optional(body, |b| Block::validate_brace(report, b));
                todo!();
            },
            welly::Expr::Op(left, op, right) => {
                let loc = Loc::location(op);
                match &**op {
                    Op::Cast => {
                        let left = compulsory(left, || report(loc, "Missing expression"))?;
                        let right = compulsory(right, || report(loc, "Missing expression"))?;
                        let left = Expr::validate_expr(report, left);
                        let right = Type::validate_expr(report, right);
                        Self::Op(left?, *op, right?)
                    },
                    Op::Missing => Err(report(loc, "Missing operator"))?,
                    _ => { todo!() },
                }
            },
            welly::Expr::Field(object, field) => {
                let loc = Loc::location(field);
                let object = compulsory(object, || report(loc, "Missing expression before `.field`"))?;
                let object = Expr::validate_expr(report, &*object);
                let field = Name::validate(report, field);
                Self::Field(object?, field?)
            },
            welly::Expr::Call(_fn, _args) => {
                todo!();
            },
        }))
    }
}

impl AST for Box<Expr> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            Expr::validate_expr(report, tree)
        } else {
            Err(report(Location::EVERYWHERE, "Not an expression"))?
        }
    }
}

/// An [`Expr`] used as a type.
type Type = Expr;

// ----------------------------------------------------------------------------

/// A `case` clause.
pub struct Case(Location, LExpr, Block);

/// An `else` clause.
pub struct Else(Location, Block);

/// A statement.
pub enum Stmt {
    Expr(Expr),
    Assign(LExpr, Loc<AssignOp>, Expr),
    If(Location, Expr, Block, Option<Else>),
    While(Location, Expr, Block, Option<Else>),
    For(Location, LExpr, Expr, Block, Option<Else>),
    Switch(Location, Expr, Vec<Case>, Option<Else>),
}

impl Stmt {
    fn validate_stmt(_report: &mut impl FnMut(Location, &str), _tree: &welly::Stmt)
    -> Result<Box<Self>, Invalid> {
        todo!();
    }
}

impl AST for Box<Stmt> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Stmt>() {
            Stmt::validate_stmt(report, tree)
        } else {
            Err(report(Location::EVERYWHERE, "Not a statement"))?
        }
    }
}

// ----------------------------------------------------------------------------

/// A block of [`Stmt`]s.
pub struct Block(Vec<Stmt>);

impl Block {
    pub fn validate_brace(_report: impl FnMut(Location, &str), _tree: &welly::Brace)
    -> Result<Self, Invalid> {
        todo!();
    }
}
