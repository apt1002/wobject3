use welly_parser::{welly, Tree, Location, Loc, Token};
use welly::{Op, AssignOp};
use super::{Invalid, AST};

/// Reports an error if `tree` is `None`.
fn compulsory<T>(
    tree: &Option<T>,
    missing: impl FnOnce(),
) -> Result<&T, Invalid> {
    Ok(tree.as_ref().ok_or_else(missing)?)
}

/// Returns the sole element of `array`, if its length is `1`.
fn only<T>(array: Box<[T]>) -> Result<T, Box<[T]>> {
    let array: Box<[T; 1]> = array.try_into()?;
    let [element] = *array;
    Ok(element)
}

// ----------------------------------------------------------------------------

/// Represents a comma-separated tuple of `A`s in round brackets.
///
/// The `bool` indicates if there is a trailing comma.
pub struct Tuple<A>(pub Box<[A]>, pub bool);

impl<A> Tuple<A> {
    /// If there's no trailing comma and exactly one element, return it.
    /// Otherwise apply `tuple`.
    fn bracket_or_tuple(self, tuple: impl FnOnce(Box<[A]>) -> A) -> A {
        let Tuple(asts, trailing_comma) = self;
        let asts = if !trailing_comma { only(asts) } else { Err(asts) };
        asts.unwrap_or_else(|asts| tuple(asts))
    }
}

impl<A: AST> AST for Tuple<A> where
    <A as AST>::Generous: Tree,
{
    type Generous = welly::Round;

    fn validate(report: &mut impl FnMut(Location, &str), round: &Self::Generous)
    -> Result<Self, Invalid> {
        struct State<'s, R, A> {
            asts: Vec<A>,
            report: &'s mut R,
            is_valid: bool,
            trailing_comma: bool,
        }
        
        impl<R: FnMut(Location, &str), A> State<'_, R, A> {
            /// Report an error.
            fn report(&mut self, loc: Location, msg: &str) {
                if self.is_valid { (self.report)(loc, msg); }
                self.is_valid = false;
            }

            /// Record an `A`.
            fn push(&mut self, loc: Location, ast: A) {
                if !self.trailing_comma { self.report(loc, "Missing comma"); }
                self.asts.push(ast);
                self.trailing_comma = false;
            }

            /// Record a comma.
            fn comma(&mut self, loc: Location) {
                if self.trailing_comma { self.report(loc, "Missing expression"); }
                self.trailing_comma = true;
            }
        }
        
        let mut state = State {asts: Vec::new(), report, is_valid: true, trailing_comma: false};
        let mut contents = round.0.iter();
        while let Some(&Token(Loc(ref result, loc))) = contents.next() {
            match result {
                Ok(tree) => {
                    if let Some(tree) = tree.downcast_ref::<A::Generous>() {
                        if let Ok(ast) = A::validate(state.report, tree) {
                            state.push(loc, ast);
                        } else {
                            state.is_valid = false;
                        }
                    } else if **tree == ',' {
                        state.comma(loc);
                    } else if state.trailing_comma {
                        state.report(loc, "Expected an expression");
                    } else if state.is_valid {
                        state.report(loc, "Expected a comma");
                    }
                },
                Err(msg) => {
                    state.report(loc, msg);
                },
            }
        }
        if state.is_valid {
            Ok(Self(state.asts.into(), state.trailing_comma))
        } else { Err(Invalid) }
    }
}

// ----------------------------------------------------------------------------

/// A Literal expression, representing a constant value.
pub enum Literal {
    Int(Loc<u64>),
    Char(Loc<char>),
    Str(Loc<String>),
}

impl AST for Loc<u64> {
    type Generous = Loc<String>;

    fn validate(report: &mut impl FnMut(Location, &str), value: &Self::Generous)
    -> Result<Self, Invalid> {
        if let Ok(i) = value.0.parse::<u64>() { return Ok(Loc(i, value.1)); }
        if let Ok(i) = value.0.parse::<i64>() { return Ok(Loc(i as u64, value.1)); }
        Err(report(value.1, "Invalid integer literal"))?
    }
}

// ----------------------------------------------------------------------------

/// An valid identifier.
///
/// Identifiers are written with capital and lower-case letters, digits and
/// underscores, and do not start with a digit.
#[derive(Debug, Clone)]
pub struct Name(Loc<String>);

impl std::borrow::Borrow<str> for Name {
    fn borrow(&self) -> &str { self.0.0.borrow() }
}

impl Name {
    /// Returns `value` as `Self` if possible.
    fn maybe_validate(value: &Loc<String>) -> Option<Self> {
        let mut cs = value.0.chars();
        if let Some(c) = cs.next() {
            if !matches!(c, '_' | 'A'..='Z' | 'a'..='z') { return None; }
            while let Some(c) = cs.next() {
                if !matches!(c, '_' | '0'..='9' | 'A'..='Z' | 'a'..='z') { return None; }
            }
            Some(Self(value.clone()))
        } else { None }
    }
}

impl AST for Name {
    type Generous = Loc<String>;

    fn validate(report: &mut impl FnMut(Location, &str), value: &Self::Generous)
    -> Result<Self, Invalid> {
        Ok(Name::maybe_validate(value).ok_or_else(|| report(value.1, "Invalid identifier"))?)
    }
}

// ----------------------------------------------------------------------------

/// An valid tag.
///
/// Tags are written with capital letters, digits and underscores, and do not
/// start with a digit.
#[derive(Debug, Clone)]
pub struct Tag(Loc<String>);

impl std::borrow::Borrow<str> for Tag {
    fn borrow(&self) -> &str { self.0.0.borrow() }
}

impl Tag {
    /// Returns `value` as `Self` if possible.
    fn maybe_validate(value: &Loc<String>) -> Option<Self> {
        let mut cs = value.0.chars();
        if let Some(c) = cs.next() {
            if !matches!(c, '_' | 'A'..='Z') { return None; }
            while let Some(c) = cs.next() {
                if !matches!(c, '_' | '0'..='9' | 'A'..='Z') { return None; }
            }
            Some(Self(value.clone()))
        } else { None }
    }

    /// Returns `value` as `Self` if possible.
    fn maybe_validate_expr(tree: &welly::Expr) -> Option<Self> {
        if let welly::Expr::Name(s) = tree { Self::maybe_validate(s) } else { None }
    }
}

// ----------------------------------------------------------------------------

/// An expression that can appear on the left-hand side of an assignment.
pub enum LExpr {
    Name(Name),
    Literal(Literal),
    Tuple(Loc<Box<[LExpr]>>),
    Field(Box<LExpr>, Name),
    Tag(Tag, Loc<Box<[LExpr]>>),
    Cast(Box<LExpr>, Location, Box<Type>),
}

impl AST for LExpr {
    type Generous = welly::Expr;

    fn validate(report: &mut impl FnMut(Location, &str), tree: &Self::Generous)
    -> Result<Self, Invalid> {
        Ok(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                let c = s.0.chars().next().expect("Should be non-empty");
                if matches!(c, '0'..='9') {
                    Self::Literal(Literal::Int(Loc::<u64>::validate(report, s)?))
                } else {
                    Self::Name(Name::validate(report, s)?)
                }
            },
            welly::Expr::Round(round) => {
                let loc = Location::EVERYWHERE; // TODO.
                Tuple::validate(report, round)?.bracket_or_tuple(
                    |asts| Self::Tuple(Loc(asts, loc))
                )
            },
            welly::Expr::Function(_name, params, _return_type, _body) => {
                Err(report(params.1, "Expression is not assignable"))?
            },
            welly::Expr::Op(left, op, right) => {
                match op {
                    Loc(Op::Cast, loc) => {
                        let left = compulsory(left, || report(*loc, "Missing left operand"))?;
                        let right = compulsory(right, || report(*loc, "Missing right operand"))?;
                        let left = Box::<Self>::validate(report, &*left);
                        let right = Box::<Type>::validate(report, &*right);
                        Self::Cast(left?, *loc, right?)
                    },
                    Loc(Op::Missing, loc) => Err(report(*loc, "Missing operator"))?,
                    _ => Err(report(op.1, "This operator does not make an assignable expression"))?,
                }
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(field.1, "Missing expression before `.field`"))?;
                let object = Box::<Self>::validate(report, &*object);
                let field = Name::validate(report, field);
                Self::Field(object?, field?)
            },
            welly::Expr::Call(tag, Loc(args, loc)) => {
                let tag = tag.as_ref().expect("Should have parsed as a tuple");
                let args = Tuple::validate(report, args);
                if let Some(tag) = Tag::maybe_validate_expr(tag) {
                    Self::Tag(tag, Loc(args?.0, *loc))
                } else { Err(report(*loc, "Expression is not assignable"))? }
            },
        })
    }
}

// ----------------------------------------------------------------------------

/// An expression.
pub enum Expr {
    Name(Name),
    Literal(Literal),
    Tuple(Loc<Box<[Expr]>>),
    Op(Box<Expr>, Loc<Op>, Box<Expr>),
    Function(Option<Name>, Loc<Box<[LExpr]>>, Option<Box<Type>>, Block),
    FunctionType(Option<Name>, Loc<Box<[LExpr]>>, Option<Box<Type>>),
    Field(Box<Expr>, Name),
    Tag(Tag, Loc<Box<[Expr]>>),
    Call(Box<Expr>, Loc<Box<[Expr]>>),
    Cast(Box<Expr>, Location, Box<Expr>),
}

impl AST for Expr {
    type Generous = welly::Expr;

    fn validate(report: &mut impl FnMut(Location, &str), tree: &Self::Generous)
    -> Result<Self, Invalid> {
        Ok(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                let c = s.0.chars().next().expect("Should be non-empty");
                if matches!(c, '0'..='9') {
                    Self::Literal(Literal::Int(Loc::<u64>::validate(report, s)?))
                } else {
                    Self::Name(Name::validate(report, s)?)
                }
            },
            welly::Expr::Round(round) => {
                let loc = Location::EVERYWHERE; // TODO.
                Tuple::validate(report, round)?.bracket_or_tuple(
                    |asts| Self::Tuple(Loc(asts, loc))
                )
            },
            welly::Expr::Function(name, Loc(params, loc), return_type, body) => {
                let name = Option::<Name>::validate(report, name);
                let params = Tuple::validate(report, params);
                let return_type = Option::<Box<Type>>::validate(report, return_type);
                if let Some(body) = body {
                    let body = Block::validate(report, body);
                    Self::Function(name?, Loc(params?.0, *loc), return_type?, body?)
                } else {
                    Self::FunctionType(name?, Loc(params?.0, *loc), return_type?)
                }
            },
            welly::Expr::Op(left, op, right) => {
                match op {
                    Loc(Op::Cast, loc) => {
                        let left = compulsory(left, || report(*loc, "Missing expression"))?;
                        let right = compulsory(right, || report(*loc, "Missing expression"))?;
                        let left = Box::<Self>::validate(report, left);
                        let right = Box::<Type>::validate(report, right);
                        Self::Cast(left?, *loc, right?)
                    },
                    Loc(Op::Missing, loc) => Err(report(*loc, "Missing operator"))?,
                    _ => { todo!() },
                }
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(field.1, "Missing expression before `.field`"))?;
                let object = Box::<Self>::validate(report, object);
                let field = Name::validate(report, field);
                Self::Field(object?, field?)
            },
            welly::Expr::Call(fn_, Loc(args, loc)) => {
                let fn_ = fn_.as_ref().expect("Should have parsed as a tuple");
                let args = Tuple::validate(report, args);
                if let Some(tag) = Tag::maybe_validate_expr(fn_) {
                    Self::Tag(tag, Loc(args?.0, *loc))
                } else {
                    let fn_ = Box::<Expr>::validate(report, fn_);
                    Self::Call(fn_?, Loc(args?.0, *loc))
                }
            },
        })
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

impl AST for Stmt {
    type Generous = welly::Stmt;

    fn validate(_report: &mut impl FnMut(Location, &str), _tree: &Self::Generous)
    -> Result<Self, Invalid> {
        todo!();
    }
}

// ----------------------------------------------------------------------------

/// A block of [`Stmt`]s.
pub struct Block(Box<[Stmt]>);

impl AST for Block {
    type Generous = welly::Brace;

    fn validate(report: &mut impl FnMut(Location, &str), tree: &welly::Brace)
    -> Result<Self, Invalid> {
        let mut ret = Vec::new();
        let mut is_valid = true;
        for Token(Loc(result, loc)) in &tree.0 {
            match result {
                Ok(tree) => {
                    if let Some(tree) = tree.downcast_ref::<welly::Stmt>() {
                        ret.push(Stmt::validate(report, tree)?);
                    } else {
                        report(*loc, "Expected a statement");
                        is_valid = false;
                    }
                },
                Err(msg) => { report(*loc, msg); is_valid = false; }
            }
        }
        if is_valid { Ok(Block(ret.into())) } else { Err(Invalid) }
    }
}
