use std::{fmt};

use std::rc::{Rc};
use std::num::{Wrapping};

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

