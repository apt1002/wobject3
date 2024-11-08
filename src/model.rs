use std::rc::{Rc};

// ----------------------------------------------------------------------------

/// Represents a Welly constructor.
///
/// In source code, a `Tag`s is written as a name consisting only of capital
/// letters, digits and underscores and not beginning with a digit.
// TODO: Represent as a 64-bit integer.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag(Rc<str>);

// ----------------------------------------------------------------------------

/// A reference-counted heap block containing zero or more [`Value`]s.
#[derive(Debug, Clone)]
pub struct Tuple(Rc<[Value]>);

impl Tuple {
    pub fn new(args: impl Into<Rc<[Value]>>) -> Self { Self(args.into()) }

    /// If `self` is not the only reference to the data, clone the data so that
    /// it is. Then, mutably borrow the data.
    pub fn make_mut(&mut self) -> &mut [Value] {
        // Can't use `Rc::make_mut()` because `[T]` doesn't implement `Clone`.
        if Rc::get_mut(&mut self.0).is_none() {
            self.0 = self.0.iter().cloned().collect();
        }
        Rc::get_mut(&mut self.0).unwrap()
    }
}

impl std::ops::Deref for Tuple {
    type Target = [Value];
    fn deref(&self) -> &Self::Target { &*self.0 }
}

// ----------------------------------------------------------------------------

/// A virtual method table. Every object has one of these.
///
/// The purpose of this trait is to hide the implementation of code from the
/// data model, such that `Value` does not depend on it. As a bonus, it allows
/// using more than one kind of code at the same time.
pub trait Call: std::fmt::Debug {
    /// Call method `tag` of `args.last()`, passing `args`.
    ///
    /// - tag - which method of the object to call.
    /// - args - the arguments in right-to-left order, followed by the object.
    fn call(&self, tag: &Tag, args: Vec<Value>) -> Vec<Value>;
}

// ----------------------------------------------------------------------------

/// A first-class Welly value.
///
/// Values can be held in variables and passed to functions.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(u64),
    Structure(Tag, Tuple),
    Object(Rc<dyn Call>, Box<Value>),
}
