use std::{fmt};
use std::rc::{Rc};
use std::num::{Wrapping};

/// An array of bytes that is probably a `str` but not guaranteed valid UTF-8.
#[derive(Clone, Hash, PartialEq, Eq)]
pub struct Bytes(pub Rc<[u8]>);

impl std::fmt::Debug for Bytes {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match std::str::from_utf8(&*self.0) {
            Ok(string) => string.fmt(f),
            Err(bytes) => bytes.fmt(f),
        }
    }
}

impl From<&[u8]> for Bytes {
    fn from(value: &[u8]) -> Self { Self(value.into()) }
}

impl From<&str> for Bytes {
    fn from(value: &str) -> Self { Self::from(value.as_bytes()) }
}

/// Represents a Welly constructor.
///
/// In source code, a `Tag`s is written as a name consisting only of capital
/// letters, digits and underscores and not beginning with a digit.
// TODO: Represent as a 64-bit integer.
pub type Tag = Bytes;

/// Represents a Welly name.
///
/// In source code, a `Name`s is written as letters, digits and underscores,
/// not beginning with a digit, and not a [`Tag`].
// TODO: Represent as a 64-bit integer.
pub type Name = Bytes;

/// Represents a map from `Name` to `T`.
pub type Map<T> = std::collections::HashMap<Name, T>;

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

/// The low-level representation of a [`Value`].
///
/// This is the representation used in compiled code. Generally, it is not
/// possible to correctly interpret the `Repr` of a `Value` unless you also
/// know its `Type`.
///
/// Occupies three machine words.
#[derive(Clone)]
pub enum Repr {
    /// Something represented as a `Word`, e.g. an `Int` or `Float`.
    Word(Word),

    /// Something reprented as a `str`, e.g. a `Tag` or `Str`.
    Bytes(Bytes),

    /// Something represented as multiple [`Repr`]s, e.g. a tuple or array.
    Values(Rc<[Repr]>),

    /// Something represented as a map, e.g. a `Module`.
    Map(Rc<Map<Repr>>),

    /// A dynamically typed [`Value`].
    ///
    /// `Dynamic(None)` is also used to represent uninitialised data.
    Dynamic(Option<Rc<Value>>),
}

impl Repr {
    /// Assert that `self` is a `Word`.
    pub fn word(&self) -> Word {
        let Self::Word(ret) = self else { panic!("{:?} is not a Word", self); };
        *ret
    }

    /// Assert that `self` is a `[u8]`.
    pub fn bytes(&self) -> &Bytes {
        let Self::Bytes(ret) = self else { panic!("{:?} is not a Bytes", self); };
        ret
    }

    /// Assert that `self` is a mutable `[u8]`.
    pub fn bytes_mut(&mut self) -> &mut [u8] {
        let Self::Bytes(Bytes(ret)) = self else { panic!("{:?} is not a Bytes", self); };
        Rc::get_mut(ret).expect("Bytes are not mutable")
    }

    /// Assert that `self` is a `[Repr]`.
    pub fn values(&self) -> &Rc<[Repr]> {
        let Self::Values(ret) = self else { panic!("{:?} is not a Values", self); };
        ret
    }

    /// Assert that `self` is a mutable `[Repr]`.
    pub fn values_mut(&mut self) -> &mut [Repr] {
        let Self::Values(ret) = self else { panic!("{:?} is not a Values", self); };
        Rc::get_mut(ret).expect("Values are not mutable")
    }

    /// Assert that `self` is a `Map`.
    pub fn map(&self) -> &Rc<Map<Repr>> {
        let Self::Map(ret) = self else { panic!("{:?} is not a Map", self); };
        ret
    }

    /// Assert that `self` is a mutable `Map`.
    pub fn map_mut(&mut self) -> &mut Map<Repr> {
        let Self::Map(ret) = self else { panic!("{:?} is not a Map", self); };
        Rc::get_mut(ret).expect("Map is not mutable")
    }

    /// Assert that `self` is a `Value`.
    pub fn dynamic(&self) -> &Option<Rc<Value>> {
        let Self::Dynamic(ret) = self else { panic!("{:?} is not a Dynamic", self); };
        ret
    }

    /// A place-holder for uninitialised `Repr`s.
    /// This is unlikely to be accidentally interepreted as a useful `Repr`.
    pub const UNINITIALISED: Self = Self::Dynamic(None);

    /// Make `self` mutable by unsharing the data it points to.
    pub fn make_mut(&mut self) {
        match self {
            Self::Word(_) => {},
            Self::Bytes(Bytes(bytes)) => { Rc::make_mut(bytes); },
            Self::Values(values) => { Rc::make_mut(values); },
            Self::Map(map) => { Rc::make_mut(map); },
            Self::Dynamic(value) => { value.as_mut().map(Rc::make_mut); }
        }
    }

    /// Assert that `self` is a tuple of size `N`.
    pub fn unpack<const N: usize>(&self) -> &[Repr; N] {
        let slice = self.values();
        let Ok(ret) = (&**slice).try_into() else {
            panic!("{:?} does not have length {}", slice, N);
        };
        ret
    }
}

impl std::default::Default for Repr {
    fn default() -> Self { Repr::UNINITIALISED }
}

impl fmt::Debug for Repr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Word(word) => format!("{:x}", word.u()).fmt(f),
            Self::Bytes(bytes) => bytes.fmt(f),
            Self::Values(values) => values.fmt(f),
            Self::Map(map) => map.fmt(f),
            Self::Dynamic(None) => { f.debug_tuple("UNINITIALISED").finish() },
            Self::Dynamic(Some(value)) => value.fmt(f),
        }
    }
}

impl<T: Into<Word>> From<T> for Repr {
    fn from(value: T) -> Self { Self::Word(value.into()) }
}

impl From<Bytes> for Repr {
    fn from(value: Bytes) -> Self { Self::Bytes(value) }
}

impl From<&[u8]> for Repr {
    fn from(value: &[u8]) -> Self { Self::Bytes(value.into()) }
}

impl From<&str> for Repr {
    fn from(value: &str) -> Self { Self::Bytes(value.into()) }
}

impl<const N: usize> From<[Repr; N]> for Repr {
    fn from(fields: [Repr; N]) -> Self { Self::Values(Rc::new(fields)) }
}

impl From<&[Repr]> for Repr {
    fn from(fields: &[Repr]) -> Self { Self::Values(fields.into()) }
}

// ----------------------------------------------------------------------------

/// Represents the type of a [`Value`].
pub type Type = Option<Rc<Value>>;

/// Represents a dynamically typed value, consisting of its [`Type`] and its
/// [`Repr`].
///
/// The `Type` is another `Value`; it is therefore a linked list.
#[derive(Clone)]
pub struct Value {
    pub type_: Type,
    pub repr: Repr,
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut t = f.debug_tuple("Value");
        let mut value = self;
        loop {
            t.field(&value.repr);
            let Some(ref type_) = value.type_ else { break; };
            value = &**type_;
        }
        t.finish()
    }
}
