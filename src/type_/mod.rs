use std::{fmt};
use std::convert::{TryFrom};
use std::rc::{Rc};

use super::{model};
use model::{Bytes, Value, Type, Dynamic};

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
    /// Tests whether `self` equals `expected`.
    fn matches(&self, expected: &'static str) -> bool {
        &*self.0.0 == expected.as_bytes()
    }
}

impl<'a> TryFrom<&'a Dynamic> for Primitive {
    type Error = Nope;

    fn try_from(value: &'a Dynamic) -> Result<Self, Self::Error> {
        let None = value.type_ else { Err(Nope)? };
        Ok(Self(value.value.bytes().clone()))
    }
}

impl<'a> TryFrom<&'a Option<Rc<Dynamic>>> for Primitive {
    type Error = Nope;

    fn try_from(value: &'a Option<Rc<Dynamic>>) -> Result<Self, Self::Error> {
        let Some(value) = value else { Err(Nope)? };
        (&**value).try_into()
    }
}

// ----------------------------------------------------------------------------

/// Represents the type of a tuple.
#[derive(Debug, Clone)]
pub struct Tuple(pub Rc<[Value]>);

impl Tuple {
    /// Returns the number of fields the tuple has.
    pub fn len(&self) -> usize { self.0.len() }

    /// Returns the type of field `index`.
    pub fn field(&self, index: usize) -> &Type {
        self.0[index].dynamic()
    }
}

impl<'a> TryFrom<&'a Dynamic> for Tuple {
    type Error = Nope;

    fn try_from(value: &'a Dynamic) -> Result<Self, Self::Error> {
        if !Primitive::try_from(&value.type_)?.matches("TUPLE") { Err(Nope)? }
        Ok(Self(value.value.values().clone()))
    }
}

impl<'a> TryFrom<&'a Option<Rc<Dynamic>>> for Tuple {
    type Error = Nope;

    fn try_from(value: &'a Option<Rc<Dynamic>>) -> Result<Self, Self::Error> {
        let Some(value) = value else { Err(Nope)? };
        (&**value).try_into()
    }
}
