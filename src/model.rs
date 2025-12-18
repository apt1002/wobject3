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

// ----------------------------------------------------------------------------

/// The run-time operations that an interpreter might attempt to perform on a
/// `Value`.
///
/// Any of these operations can Welly-panic if attempted on the wrong kind of
/// `Value`. The default implementations all panic.
pub trait Object: std::fmt::Debug {
    /// Check that `self` has a method named `tag` and call it passing `args`.
    ///
    /// - tag - which method to call.
    /// - args - the arguments in right-to-left order, excluding `self`.
    /// Returns:
    /// - the updated `self`, or a dummy value, e.g. the integer `0`.
    /// - the return values in right-to-left order, excluding `self`.
    fn call(self: Rc<Self>, _tag: &Tag, _args: Vec<Value>) -> (Value, Vec<Value>) {
        panic!("Value has no methods")
    }

    /// Check that `self` is a `Word`.
    fn word(&self) -> Word {
        panic!("Value is not a Word");
    }

    /// Check that `self` has a `Tag`.
    fn tag(&self) -> &Tag {
        panic!("Value has no Tag");
    }

    /// Check that `self` is a tuple and read its fields.
    fn tuple_ref(&self) -> &[Value] {
        panic!("Value is not a tuple");
    }

    /// Check that `self` is a tuple and mutate its fields.
    fn tuple_mut(&mut self) -> &mut [Value] {
        let _ = self.tuple_ref(); // Panic with "not a tuple" if appropriate.
        panic!("Value is not mutable");
    }

    /// Check that `Self` implements `Clone`. Returns `Rc::new(self.clone())`.
    ///
    /// `Rc::get_mut()` applied to the result must always succeed.
    /// This is used to implement [`Value::make_mut()`].
    fn dyn_clone(&self) -> Rc<dyn Object> {
        panic!("Value is not cloneable");
    }
}

// ----------------------------------------------------------------------------

/// A first-class Welly value.
///
/// `Value`s can be held in variables and passed to functions.
// One level of indirection serves three purposes:
// - Reference counting.
// - Dynamic dispatch (unknown implementation of methods).
// - Dynamic tuple size (unknown number of elements).
#[derive(Debug, Clone)]
pub struct Value(pub Rc<dyn Object>);

impl Value {
    /// An abbreviation for `Value(Rc::new(v))`.
    pub fn new<T: 'static + Object>(v: T) -> Self { Self(Rc::new(v)) }

    /// Ensure `self` is unshared, using [`Object::dyn_clone()`] if necessary.
    /// Then, borrow it mutably.
    pub fn make_mut(&mut self) -> &mut dyn Object {
        if Rc::get_mut(&mut self.0).is_none() { self.0 = self.0.dyn_clone(); }
        Rc::get_mut(&mut self.0).expect("Bug in `dyn_clone()`")
    }
}

// ----------------------------------------------------------------------------

/// Construct [`Tuple`]s.
pub trait Constructor: 'static + fmt::Debug {
    /// Implement `Object::call()` for `Tuple<Self, N>`.
    ///
    /// Not available in `dyn Constructor`.
    fn call<const N: usize>(
        &self,
        _object: Rc<Tuple<Self, N>>,
        _tag: &Tag,
        _args: Vec<Value>,
    ) -> (Value, Vec<Value>)
    where Self: Sized {
        panic!("Tuple has no methods")
    }

    /// Implement `Object::tag()` for `Tuple<Self, N>`.
    fn tag(&self) -> &Tag {
        panic!("Value has no Tag");
    }
}

// ----------------------------------------------------------------------------

/// A [`Value`] containing zero or more simpler `Value`s.
#[derive(Debug)]
pub struct Tuple<C: Constructor, const N: usize>(Rc<C>, [Value; N]);

impl<C: Constructor, const N: usize> Tuple<C, N> {
    pub fn new(constructor: Rc<C>, values: impl IntoIterator<Item=Value>) -> Self {
        let values = arrayvec::ArrayVec::from_iter(values);
        Self(constructor, values.into_inner().expect("Wrong size"))
    }
}

impl<C: Constructor, const N: usize> Clone for Tuple<C, N> {
    // Cannot automatically derive because not `C: Clone`.
    fn clone(&self) -> Self { Tuple(self.0.clone(), self.1.clone()) }
}

impl<C: Constructor, const N: usize> Object for Tuple<C, N> {
    fn call(self: Rc<Self>, tag: &Tag, args: Vec<Value>) -> (Value, Vec<Value>) {
        self.0.clone().call(self, tag, args)
    }

    fn tag(&self) -> &Tag { &self.0.tag() }
    fn tuple_ref(&self) -> &[Value] { &self.1[..] }
    fn tuple_mut(&mut self) -> &mut [Value] { &mut self.1 }
    fn dyn_clone(&self) -> Rc<dyn Object> { Rc::new(self.clone()) }
}

// ----------------------------------------------------------------------------

pub trait NewTuple: Constructor {
    /// Construct a `Tuple` whose size is not known at compile time.
    ///
    /// The purpose of this method is to call `Tuple::<Self, N>::new()` where
    /// `N` is `values.len()`. The difficulty is that `N` must be a
    /// compile-time constant, whereas `values.len()` is not. To work around
    /// this, the method matches on `values.len()` and selects one of a finite
    /// number of calls to `Tuple::new()`. If `N` is too large it won't work.
    fn new_tuple(self: Rc<Self>, values: std::vec::Drain<'_, Value>) -> Value;
}

impl<T: Constructor> NewTuple for T {
    fn new_tuple(self: Rc<Self>, values: std::vec::Drain<'_, Value>) -> Value {
        match values.len() {
            0 => Value::new(Tuple::<Self, 0>::new(self, values)),
            1 => Value::new(Tuple::<Self, 1>::new(self, values)),
            2 => Value::new(Tuple::<Self, 2>::new(self, values)),
            3 => Value::new(Tuple::<Self, 3>::new(self, values)),
            4 => Value::new(Tuple::<Self, 4>::new(self, values)),
            5 => Value::new(Tuple::<Self, 5>::new(self, values)),
            6 => Value::new(Tuple::<Self, 6>::new(self, values)),
            7 => Value::new(Tuple::<Self, 7>::new(self, values)),
            8 => Value::new(Tuple::<Self, 8>::new(self, values)),
            9 => Value::new(Tuple::<Self, 9>::new(self, values)),
            10 => Value::new(Tuple::<Self, 10>::new(self, values)),
            11 => Value::new(Tuple::<Self, 11>::new(self, values)),
            12 => Value::new(Tuple::<Self, 12>::new(self, values)),
            13 => Value::new(Tuple::<Self, 13>::new(self, values)),
            14 => Value::new(Tuple::<Self, 14>::new(self, values)),
            15 => Value::new(Tuple::<Self, 15>::new(self, values)),
            _ => panic!("Too many fields"),
        }
    }
}
