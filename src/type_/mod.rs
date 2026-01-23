use std::{fmt};
use std::convert::{TryFrom};
use std::rc::{Rc};

use super::{model};
use model::{Bytes, Repr, Type, Value};

/// An error indicating that a Welly type is not of the expected form.
#[derive(Debug, Copy, Clone)]
pub struct Nope;

impl fmt::Display for Nope {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "Incorrect type") }
}

impl std::error::Error for Nope {}

// ----------------------------------------------------------------------------

/// The type of primitive types (other than `PRIMITIVE` itself).
///
/// This is the unique [`Type`] that has no type.
pub const PRIMITIVE: Type = None;

/// Represents the type of a primitive type.
///
/// The Welly representation of a `Primitive` has type `PRIMITIVE`
/// and value of type `Bytes`.
#[derive(Debug, Clone)]
pub struct Primitive(pub Bytes);

impl Primitive {
    /// Construct a `Primitive`.
    pub fn new(name: &'static str) -> Self { Self(name.into()) }

    /// Construct the Welly representation of `self`.
    pub fn to_welly(self) -> Value {
        Value {type_: PRIMITIVE, repr: Repr::Bytes(self.0)}
    }

        /// Tests whether `self` equals `expected`.
    fn matches(&self, expected: &'static str) -> bool {
        &*self.0.0 == expected.as_bytes()
    }
}

impl<'a> TryFrom<&'a Value> for Primitive {
    type Error = Nope;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        let None = value.type_ else { Err(Nope)? };
        Ok(Self(value.repr.bytes().clone()))
    }
}

impl<'a> TryFrom<&'a Type> for Primitive {
    type Error = Nope;

    fn try_from(value: &'a Option<Rc<Value>>) -> Result<Self, Self::Error> {
        let Some(value) = value else { Err(Nope)? };
        (&**value).try_into()
    }
}

// ----------------------------------------------------------------------------

/// Represents the type of a tuple.
#[derive(Debug, Clone)]
pub struct Tuple(pub Rc<[Repr]>);

impl Tuple {
    /// Construct a `Tuple`.
    pub fn new(fields: impl IntoIterator<Item=Type>) -> Self {
        Self(fields.into_iter().map(Repr::Dynamic).collect())
    }

    /// Construct the Welly representation of `self`.
    pub fn to_welly(self) -> Value {
        Value {
            type_: Some(Rc::new(Primitive::new("TUPLE").to_welly())),
            repr: Repr::Values(self.0),
        }
    }

    /// Returns the number of fields the tuple has.
    pub fn len(&self) -> usize { self.0.len() }

    /// Returns the type of field `index`.
    pub fn field(&self, index: usize) -> &Type {
        self.0[index].dynamic()
    }
}

impl<'a> TryFrom<&'a Value> for Tuple {
    type Error = Nope;

    fn try_from(value: &'a Value) -> Result<Self, Self::Error> {
        if !Primitive::try_from(&value.type_)?.matches("TUPLE") { Err(Nope)? }
        Ok(Self(value.repr.values().clone()))
    }
}

impl<'a> TryFrom<&'a Type> for Tuple {
    type Error = Nope;

    fn try_from(value: &'a Option<Rc<Value>>) -> Result<Self, Self::Error> {
        let Some(value) = value else { Err(Nope)? };
        (&**value).try_into()
    }
}
