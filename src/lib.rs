use welly_parser::{Location, Tree};

/// The error type of [`AST::validate()`].
///
/// This can conveniently be returned using the idoim `Err(report(...))?`.
pub struct Invalid;

impl From<()> for Invalid {
    fn from(_: ()) -> Self { Self }
}

/// Represents a valid abstract syntax tree of some valid Welly source code.
pub trait AST: Sized {
    /// Attempt to construct a `Self` given its parse [`Tree`].
    ///
    /// If you return this error, you must first `report()` at least one error.
    fn validate(report: &mut impl FnMut(Location, &str), loc: Location, tree: &dyn Tree)
    -> Result<Self, Invalid>;
}

/// All implementations of [`AST`].
pub mod ast;

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
}
