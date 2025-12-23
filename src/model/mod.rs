use std::rc::{Rc};

/// Represents a Welly constructor.
///
/// In source code, a `Tag`s is written as a name consisting only of capital
/// letters, digits and underscores and not beginning with a digit.
// TODO: Represent as a 64-bit integer.
pub type Tag = Rc<str>;

/// Represents a Welly name.
///
/// In source code, a `Name`s is written as letters, digits and underscores,
/// not beginning with a digit, and not a [`Tag`].
// TODO: Represent as a 64-bit integer.
pub type Name = Rc<str>;

/// Represents a map from `Name` to `T`.
pub type Map<T> = std::collections::HashMap<Name, T>;

// ----------------------------------------------------------------------------

mod repr;
pub use repr::{Word, Value, Dynamic};
