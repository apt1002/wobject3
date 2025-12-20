use std::collections::{HashMap};
use std::{fmt};
use std::rc::{Rc};

use super::{model};
use model::{Tag, Word};

type Unary = &'static dyn Fn(Word) -> Word;
type Binary = &'static dyn Fn(Word, Word) -> Word;

pub struct BuiltIn {
    name: &'static str,
    unary: HashMap<Tag, Unary>,
    binary: HashMap<Tag, Binary>,
}

impl fmt::Debug for BuiltIn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { f.write_str(self.name) }
}

// ----------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Primitive {
    methods: Rc<BuiltIn>,
    data: Word,
}

mod integer;
pub use integer::{compile_integer};
