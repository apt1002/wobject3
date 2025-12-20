use std::{fmt};

use std::rc::{Rc};
use std::num::{Wrapping};

// ----------------------------------------------------------------------------

/// The low-level representation of a [`Value`].
///
/// This trait is object-safe.
pub trait Represent {
    /// Assert that `self` is a [`Tag`].
    fn tag(&self) -> &Tag { panic!("Does not represent a Tag"); }

    /// Assert that `self` is a [`str`].
    fn str(&self) -> &str { panic!("Does not represent a str"); }

    /// Assert that `self` is a [`Word`].
    fn word(&self) -> Word { panic!("Does not represent a Word"); }

    /// Assert that `self` is a tuple of size `N`.
    fn slice(&self) -> &[Value] { panic!("Does not represent a tuple"); }

    /// Assert that `self` is a mutable tuple of size `N`.
    fn slice_mut(&mut self) -> &mut [Value] { panic!("Does not represent a mutable tuple"); }

    /// Assert that `self` is an array of `R`.
    fn array<R: Represent>(&self) -> &[R] { panic!("Does not represent an array"); }

    /// Assert that `self` is a mutable array of `R`.
    fn array_mut<R: Represent>(&mut self) -> &mut [R] { panic!("Does not represent a mutable array"); }

    /// Assert that `self` is a [`Dynamic`].
    fn dynamic(&self) -> &Dynamic { panic!("Does not represent a Dynamic"); }

    /// Assert that `self` is a mutable [`Dynamic`].
    fn dynamic_mut(&mut self) -> &mut Dynamic { panic!("Does not represent a mutable Dynamic"); }
}

impl Represent {
    /// Assert that `self` is a tuple of size `N`.
    fn tuple<const N: usize>(&self) -> &[Value; N] {
        let Ok(ret) = self.slice().try_into() else {
            panic!("Tuple has {} elements, but caller asked for {}", L, N);
        };
        ret
    }

    /// Assert that `self` is a mutable tuple of size `N`.
    fn tuple_mut<const N: usize>(&mut self) -> &[Value; N] {
        let Ok(ret) = self.slice_mut().try_into() else {
            panic!("Tuple has {} elements, but caller asked for {}", L, N);
        };
        ret
    }
}

impl Represent for Box<str> {
    fn str(&self) -> &str { &**self }
}

impl<const L: usize> Represent for [Value; L] {
    fn tuple<const N: usize>(&self) -> &[Value; N] {
        let Ok(ret) = (self as &[Value]).try_into() else {
            panic!("Tuple has {} elements, but caller asked for {}", L, N);
        };
        ret
    }

    fn tuple_mut<const N: usize>(&mut self) -> &mut [Value; N] {
        let Ok(ret) = (self as &mut [Value]).try_into() else {
            panic!("Tuple has {} elements, but caller asked for {}", L, N);
        };
        ret
    }

}

// ----------------------------------------------------------------------------

/// A Welly value is a reference-counted pointer to an [`Object`].
pub struct Value(Rc<dyn Represent>);

impl<T: Represent> From<T> for Value {
    fn from(value: T) -> Self { Self(Rc::new(value) as dyn Represent) }
}

// ----------------------------------------------------------------------------

/// Represents a Welly constructor.
///
/// In source code, a `Tag`s is written as a name consisting only of capital
/// letters, digits and underscores and not beginning with a digit.
// TODO: Represent as a 64-bit integer.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag(Rc<str>);

impl Tag {
    pub fn new(tag: &str) -> Self { Tag(tag.into()) }
}

impl Represent for Tag {
}

// ----------------------------------------------------------------------------

/// A 64-bit integer type with wrapping arithmetic.
#[allow(non_camel_case_types)]
pub type w64 = Wrapping<u64>;

/// A 64-bit word that is not a pointer.
#[derive(Copy, Clone)]
pub union Word {
    pub u: u64,
    pub s: i64,
    pub w: w64,
    pub f: f64,
}

impl Word {
    pub fn u(self) -> u64 { unsafe { self.u } }
    pub fn s(self) -> i64 { unsafe { self.s } }
    pub fn w(self) -> w64 { unsafe { self.w } }
    pub fn f(self) -> f64 { unsafe { self.f } }
    pub fn b(self) -> bool { unsafe { self.s != 0 } }
}

impl fmt::Debug for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:x}", unsafe { self.u })
    }
}

impl From<u64> for Word { fn from(v: u64) -> Self { Self { u: v } } }
impl From<i64> for Word { fn from(v: i64) -> Self { Self { s: v } } }
impl From<w64> for Word { fn from(v: w64) -> Self { Self { w: v } } }
impl From<f64> for Word { fn from(v: f64) -> Self { Self { f: v } } }
impl From<bool> for Word { fn from(v: bool) -> Self { Self { s: -(v as i64) } } }

// ----------------------------------------------------------------------------

/// Represents a dynamically typed value.
pub struct Dynamic {
    type_: Option<Value>,
    value: Box<dyn Represent>,
}
