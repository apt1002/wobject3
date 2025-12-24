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
/// Occupies three machine words.
#[derive(Clone)]
pub enum Value {
    /// Something represented as a `Word`, e.g. an `Int` or `Float`.
    Word(Word),

    /// Something reprented as a `str`, e.g. a `Tag` or `Str`.
    Bytes(Bytes),

    /// Something represented as multiple [`Value`]s, e.g. a tuple or array.
    Values(Rc<[Value]>),

    /// Something represented as a map, e.g. a `Module`.
    Map(Rc<Map<Value>>),

    /// A [`Dynamic`].
    Dynamic(Option<Rc<Dynamic>>),
}

impl Value {
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

    /// Assert that `self` is a `[Value]`.
    pub fn values(&self) -> &Rc<[Value]> {
        let Self::Values(ret) = self else { panic!("{:?} is not a Values", self); };
        ret
    }

    /// Assert that `self` is a mutable `[Value]`.
    pub fn values_mut(&mut self) -> &mut [Value] {
        let Self::Values(ret) = self else { panic!("{:?} is not a Values", self); };
        Rc::get_mut(ret).expect("Values are not mutable")
    }

    /// Assert that `self` is a `Map`.
    pub fn map(&self) -> &Rc<Map<Value>> {
        let Self::Map(ret) = self else { panic!("{:?} is not a Map", self); };
        ret
    }

    /// Assert that `self` is a mutable `Map`.
    pub fn map_mut(&mut self) -> &mut Map<Value> {
        let Self::Map(ret) = self else { panic!("{:?} is not a Map", self); };
        Rc::get_mut(ret).expect("Map is not mutable")
    }

    /// Assert that `self` is a `Dynamic`.
    pub fn dynamic(&self) -> &Option<Rc<Dynamic>> {
        let Self::Dynamic(ret) = self else { panic!("{:?} is not a Dynamic", self); };
        ret
    }

    /// A place-holder for uninitialised `Value`s.
    /// This is unlikely to be accidentally interepreted as a useful `Value`.
    pub const UNINITIALISED: Self = Self::Dynamic(None);

    /// Make `self` mutable by unsharing the data it points to.
    pub fn make_mut(&mut self) {
        match self {
            Self::Word(_) => {},
            Self::Bytes(Bytes(bytes)) => { Rc::make_mut(bytes); },
            Self::Values(values) => { Rc::make_mut(values); },
            Self::Map(map) => { Rc::make_mut(map); },
            Self::Dynamic(dynamic) => { dynamic.as_mut().map(Rc::make_mut); }
        }
    }

    /// Assert that `self` is a tuple of size `N`.
    pub fn unpack<const N: usize>(&self) -> &[Value; N] {
        let slice = self.values();
        let Ok(ret) = (&**slice).try_into() else {
            panic!("{:?} does not have length {}", slice, N);
        };
        ret
    }
}

impl std::default::Default for Value {
    fn default() -> Self { Value::UNINITIALISED }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Word(word) => format!("{:x}", word.u()).fmt(f),
            Self::Bytes(bytes) => bytes.fmt(f),
            Self::Values(values) => values.fmt(f),
            Self::Map(map) => map.fmt(f),
            Self::Dynamic(None) => { f.debug_tuple("UNINITIALISED").finish() },
            Self::Dynamic(Some(dynamic)) => dynamic.fmt(f),
        }
    }
}

impl<T: Into<Word>> From<T> for Value {
    fn from(value: T) -> Self { Self::Word(value.into()) }
}

impl From<Bytes> for Value {
    fn from(value: Bytes) -> Self { Self::Bytes(value) }
}

impl From<&[u8]> for Value {
    fn from(value: &[u8]) -> Self { Self::Bytes(value.into()) }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self { Self::Bytes(value.into()) }
}

impl<const N: usize> From<[Value; N]> for Value {
    fn from(fields: [Value; N]) -> Self { Self::Values(Rc::new(fields)) }
}

impl From<&[Value]> for Value {
    fn from(fields: &[Value]) -> Self { Self::Values(fields.into()) }
}

// ----------------------------------------------------------------------------

/// Represents a dynamically typed value.
///
/// The dynamic type is another `Dynamic`; it is therefore a linked list.
#[derive(Clone)]
pub struct Dynamic {
    pub type_: Option<Rc<Dynamic>>,
    pub value: Value,
}

impl fmt::Debug for Dynamic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut t = f.debug_tuple("Dynamic");
        let mut dynamic = self;
        loop {
            t.field(&dynamic.value);
            let Some(ref type_) = dynamic.type_ else { break; };
            dynamic = &**type_;
        }
        t.finish()
    }
}
