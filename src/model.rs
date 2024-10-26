use std::rc::{Rc};

use super::code::{Tag, Table, Code};

// ----------------------------------------------------------------------------

/// A reference-counted heap block containing zero or more [`Value`]s.
#[derive(Debug, Clone)]
pub struct Tuple(Rc<[Value]>);

impl Tuple {
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

/// The compile-time constant part of an object.
#[derive(Debug)]
pub struct Combinator(Box<[Value]>, Table);

impl Combinator {
    pub fn pool(&self) -> &[Value] { &self.0 }
    pub fn get(&self, tag: &Tag) -> &Code { self.1.get(tag).expect("No such method") }
}

/// A first-class Welly value.
///
/// Values can be held in variables and passed to functions.
#[derive(Debug, Clone)]
pub enum Value {
    Integer(u64),
    Structure(Tag, Tuple),
    Object(Rc<Combinator>, Box<Value>),
}
