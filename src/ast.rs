use welly_parser::{welly, Tree, Location, Loc, Token};
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

/// Returns the sole element of `array`, if its length is `1`.
fn only<T>(array: Box<[T]>) -> Result<T, Box<[T]>> {
    let array: Box<[T; 1]> = array.try_into()?;
    let [element] = *array;
    Ok(element)
}

/// Validates and collects a comma-separated tuple of `A`s.
///
/// Also returns whether there is a trailing comma.
fn tuple<R: FnMut(Location, &str), A>(
    report: &mut R,
    validate: &impl Fn(&mut R, &welly::Expr) -> Result<A, Invalid>,
    round: &welly::Round,
) -> (Result<Box<[A]>, Invalid>, bool) {
    struct State<'s, R, A> {
        asts: Vec<A>,
        report: &'s mut R,
        is_valid: bool,
        was_comma: bool,
    }
    
    impl<'s, R: FnMut(Location, &str), A> State<'s, R, A> {
        /// Report an error.
        fn report(&mut self, loc: Location, msg: &str) {
            (self.report)(loc, msg);
            self.is_valid = false;
        }

        /// Skip past the next comma, if any.
        fn skip(&mut self, contents: &mut impl Iterator<Item=&'s Token>) {
            while let Some(token) = contents.next() {
                if *token == ',' { break; }
            }
            self.was_comma = true;
        }

        /// Record an `A`.
        fn push(&mut self, loc: Location, ast: A) {
            if !self.was_comma { self.report(loc, "Missing comma"); }
            self.asts.push(ast);
            self.was_comma = false;
        }

        /// Record a comma.
        fn comma(&mut self, loc: Location) {
            if self.was_comma { self.report(loc, "Missing expression"); }
            self.was_comma = false;
        }
    }
    
    let mut state = State {asts: Vec::new(), report, is_valid: true, was_comma: false};
    let mut contents = round.0.iter();
    while let Some(&Token(Loc(ref result, loc))) = contents.next() {
        match result {
            Ok(tree) => {
                if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
                    if let Ok(ast) = validate(state.report, tree) {
                        state.push(loc, ast);
                    } else {
                        state.skip(&mut contents);
                    }
                } else if **tree == ',' {
                    state.comma(loc);
                } else {
                    let msg = if state.was_comma { "Expected an expression" } else { "Expected a comma" };
                    state.report(loc, msg);
                    state.skip(&mut contents);
                }
            },
            Err(msg) => {
                state.report(loc, msg);
                state.skip(&mut contents);
            },
        }
    }
    let ret = if state.is_valid { Ok(state.asts.into()) } else { Err(Invalid) };
    (ret, state.was_comma)
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
        if let Ok(i) = value.0.parse::<u64>() { return Ok(Self::Int(Loc(i, value.1))); }
        if let Ok(i) = value.0.parse::<i64>() { return Ok(Self::Int(Loc(i as u64, value.1))); }
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

    /// Returns `value` as `Self` if possible, otherwise reports an error.
    pub fn validate(report: &mut impl FnMut(Location, &str), value: &Loc<String>)
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

impl LExpr {
    /// If `!trailing_comma` and `asts` has exactly one element, returns it.
    /// Otherwise returns a `Self::Tuple`.
    fn bracket_or_tuple(asts: Loc<Box<[Self]>>, trailing_comma: bool) -> Self {
        let Loc(asts, loc) = asts;
        let asts = if !trailing_comma { only(asts) } else { Err(asts) };
        asts.unwrap_or_else(|asts| Self::Tuple(Loc(asts, loc)))
    }

    pub fn validate_expr(report: &mut impl FnMut(Location, &str), tree: &welly::Expr)
    -> Result<Self, Invalid> {
        Ok(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                let c = s.0.chars().next().expect("Should be non-empty");
                if matches!(c, '0'..='9') {
                    Self::Literal(Literal::validate_integer(report, s)?)
                } else {
                    Self::Name(Name::validate(report, s)?)
                }
            },
            welly::Expr::Round(round) => {
                let loc = Location::EVERYWHERE; // TODO.
                let (asts, trailing_comma) = tuple(report, &Self::validate_expr, round);
                Self::bracket_or_tuple(Loc(asts?, loc), trailing_comma)
            },
            welly::Expr::Function(_name, params, _return_type, _body) => {
                Err(report(params.1, "Expression is not assignable"))?
            },
            welly::Expr::Op(left, op, right) => {
                match op {
                    Loc(Op::Cast, loc) => {
                        let left = compulsory(left, || report(*loc, "Missing left operand"))?;
                        let right = compulsory(right, || report(*loc, "Missing right operand"))?;
                        let left = Self::validate_expr(report, &*left);
                        let right = Type::validate_expr(report, &*right);
                        Self::Cast(Box::new(left?), *loc, Box::new(right?))
                    },
                    Loc(Op::Missing, loc) => Err(report(*loc, "Missing operator"))?,
                    _ => Err(report(op.1, "This operator does not make an assignable expression"))?,
                }
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(field.1, "Missing expression before `.field`"))?;
                let object = Self::validate_expr(report, &*object);
                let field = Name::validate(report, field);
                Self::Field(Box::new(object?), field?)
            },
            welly::Expr::Call(tag, Loc(args, loc)) => {
                let tag = tag.as_ref().expect("Should have parsed as a tuple");
                let (args, _) = tuple(report, &Self::validate_expr, args);
                if let Some(tag) = Tag::maybe_validate_expr(tag) {
                    Self::Tag(tag, Loc(args?, *loc))
                } else { Err(report(*loc, "Expression is not assignable"))? }
            },
        })
    }
}

impl AST for Box<LExpr> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            Ok(Box::new(LExpr::validate_expr(report, tree)?))
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
    Tuple(Loc<Box<[Expr]>>),
    Op(Box<Expr>, Loc<Op>, Box<Expr>),
    Function(Option<Name>, Loc<Box<[LExpr]>>, Option<Box<Type>>, Block),
    FunctionType(Option<Name>, Loc<Box<[LExpr]>>, Option<Box<Type>>),
    Field(Box<Expr>, Name),
    Tag(Tag, Loc<Box<[Expr]>>),
    Call(Box<Expr>, Loc<Box<[Expr]>>),
    Cast(Box<Expr>, Location, Box<Expr>),
}

impl Expr {
    /// If `!trailing_comma` and `asts` has exactly one element, returns it.
    /// Otherwise returns a `Self::Tuple`.
    fn bracket_or_tuple(asts: Loc<Box<[Self]>>, trailing_comma: bool) -> Self {
        let Loc(asts, loc) = asts;
        let asts = if !trailing_comma { only(asts) } else { Err(asts) };
        asts.unwrap_or_else(|asts| Self::Tuple(Loc(asts, loc)))
    }

    pub fn validate_expr(report: &mut impl FnMut(Location, &str), tree: &welly::Expr)
    -> Result<Self, Invalid> {
        Ok(match tree {
            welly::Expr::Char(c) => Self::Literal(Literal::Char(*c)),
            welly::Expr::String(s) => Self::Literal(Literal::Str(s.clone())),
            welly::Expr::Name(s) => {
                let c = s.0.chars().next().expect("Should be non-empty");
                if matches!(c, '0'..='9') {
                    Self::Literal(Literal::validate_integer(report, s)?)
                } else {
                    Self::Name(Name::validate(report, s)?)
                }
            },
            welly::Expr::Round(round) => {
                let loc = Location::EVERYWHERE; // TODO.
                let (asts, trailing_comma) = tuple(report, &Self::validate_expr, round);
                Self::bracket_or_tuple(Loc(asts?, loc), trailing_comma)
            },
            welly::Expr::Function(name, Loc(params, loc), return_type, body) => {
                let name = optional(name, |n| Name::validate(report, n));
                let (params, _) = tuple(report, &LExpr::validate_expr, params);
                let return_type = optional(return_type, |rt| Expr::validate_expr(report, rt));
                if let Some(body) = body {
                    let body = Block::validate_brace(report, body);
                    Self::Function(name?, Loc(params?, *loc), return_type?.map(Box::new), body?)
                } else {
                    Self::FunctionType(name?, Loc(params?, *loc), return_type?.map(Box::new))
                }
            },
            welly::Expr::Op(left, op, right) => {
                match op {
                    Loc(Op::Cast, loc) => {
                        let left = compulsory(left, || report(*loc, "Missing expression"))?;
                        let right = compulsory(right, || report(*loc, "Missing expression"))?;
                        let left = Expr::validate_expr(report, left);
                        let right = Type::validate_expr(report, right);
                        Self::Cast(Box::new(left?), *loc, Box::new(right?))
                    },
                    Loc(Op::Missing, loc) => Err(report(*loc, "Missing operator"))?,
                    _ => { todo!() },
                }
            },
            welly::Expr::Field(object, field) => {
                let object = compulsory(object, || report(field.1, "Missing expression before `.field`"))?;
                let object = Expr::validate_expr(report, &*object);
                let field = Name::validate(report, field);
                Self::Field(Box::new(object?), field?)
            },
            welly::Expr::Call(fn_, Loc(args, loc)) => {
                let fn_ = fn_.as_ref().expect("Should have parsed as a tuple");
                let (args, _) = tuple(report, &Self::validate_expr, args);
                if let Some(tag) = Tag::maybe_validate_expr(fn_) {
                    Self::Tag(tag, Loc(args?, *loc))
                } else {
                    let fn_ = Self::validate_expr(report, fn_);
                    Self::Call(Box::new(fn_?), Loc(args?, *loc))
                }
            },
        })
    }
}

impl AST for Box<Expr> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Expr>() {
            Ok(Box::new(Expr::validate_expr(report, tree)?))
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
    -> Result<Self, Invalid> {
        todo!();
    }
}

impl AST for Box<Stmt> {
    fn validate(report: &mut impl FnMut(Location, &str), tree: &dyn Tree)
    -> Result<Self, Invalid> {
        if let Some(tree) = tree.downcast_ref::<welly::Stmt>() {
            Ok(Box::new(Stmt::validate_stmt(report, tree)?))
        } else {
            Err(report(Location::EVERYWHERE, "Not a statement"))?
        }
    }
}

// ----------------------------------------------------------------------------

/// A block of [`Stmt`]s.
pub struct Block(Box<[Stmt]>);

impl Block {
    pub fn validate_brace(report: &mut impl FnMut(Location, &str), tree: &welly::Brace)
    -> Result<Self, Invalid> {
        let mut ret = Vec::new();
        let mut is_valid = true;
        for Token(Loc(result, loc)) in &tree.0 {
            match result {
                Ok(tree) => {
                    if let Some(tree) = tree.downcast_ref::<welly::Stmt>() {
                        ret.push(Stmt::validate_stmt(report, tree)?);
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
