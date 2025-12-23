use std::{fmt};

use super::{model};
use model::{Map, Word};

type Unary = &'static dyn Fn(Word) -> Word;
type Binary = &'static dyn Fn(Word, Word) -> Word;

pub struct BuiltIn {
    pub name: &'static str,
    pub unary: Map<Unary>,
    pub binary: Map<Binary>,
}

impl fmt::Debug for BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { f.write_str(self.name) }
}

// ----------------------------------------------------------------------------

mod integer;
pub use integer::{compile_integer};
