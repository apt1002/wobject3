use std::{fmt};
use std::rc::{Rc};
use std::num::{Wrapping};

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

/// A 64-bit integer type with wrapping arithmetic.
#[allow(non_camel_case_types)]
pub type w64 = Wrapping<u64>;

// ----------------------------------------------------------------------------

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

/// The low-level representation of a [`Value`].
///
/// Occupies three machine words.
#[derive(Clone)]
pub enum Value {
    /// An uninitialised `Value`.
    None,

    /// Something represented as a `Word`, e.g. an `Int` or `Float`.
    Word(Word),

    /// Something reprented as a `str`, e.g. a `Tag` or `Str`.
    Str(Rc<str>),

    /// Something represented as multiple [`Value`]s, e.g. a tuple or array.
    Slice(Rc<[Value]>),

    /// Something represented as a map, e.g. a `Module`.
    Map(Rc<Map<Value>>),

    /// A [`Dynamic`].
    Dynamic(Rc<Dynamic>),
}

impl Value {
    /// Assert that `self` is a `Word`.
    pub fn word(&self) -> Word {
        let Self::Word(ret) = self else { panic!("{:?} is not a word", self); };
        *ret
    }

    /// Assert that `self` is a `str`.
    pub fn str(&self) -> &Rc<str> {
        let Self::Str(ret) = self else { panic!("{:?} is not a str", self); };
        ret
    }

    /// Assert that `self` is a `[Value]`.
    pub fn slice(&self) -> &Rc<[Value]> {
        let Self::Slice(ret) = self else { panic!("{:?} is not a slice", self); };
        ret
    }

    /// Assert that `self` is a `Map`.
    pub fn map(&self) -> &Rc<Map<Value>> {
        let Self::Map(ret) = self else { panic!("{:?} is not a map", self); };
        ret
    }

    /// Assert that `self` is a `Dynamic`.
    pub fn dynamic(&self) -> &Dynamic {
        let Self::Dynamic(ret) = self else { panic!("{:?} is not a dynamic", self); };
        ret
    }

    /// Assert that `self` is a tuple of size `N`.
    pub fn unpack<const N: usize>(&self) -> &[Value; N] {
        let slice = self.slice();
        let Ok(ret) = (&**slice).try_into() else {
            panic!("{:?} does not have length {}", slice, N);
        };
        ret
    }
}

impl std::default::Default for Value {
    fn default() -> Self { Value::None }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::None => f.debug_tuple("None").finish(),
            Self::Word(word) => format!("{:x}", word.u()).fmt(f),
            Self::Str(string) => string.fmt(f),
            Self::Slice(values) => values.fmt(f),
            Self::Map(map) => map.fmt(f),
            Self::Dynamic(dynamic) => dynamic.fmt(f),
        }
    }
}

impl<T: Into<Word>> From<T> for Value {
    fn from(value: T) -> Self { Self::Word(value.into()) }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self { Self::Str(value.into()) }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(fields: [Value; N]) -> Self { Self::Slice(Rc::new(fields)) }
}

// ----------------------------------------------------------------------------

/// Represents a dynamically typed value.
///
/// The dynamic type is another `Dynamic`; it is therefore a linked list.
#[derive(Debug, Clone)]
pub struct Dynamic {
    pub type_: Option<Rc<Dynamic>>,
    pub value: Value,
}
