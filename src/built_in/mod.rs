use std::collections::{HashMap};
use std::{fmt};

use super::{model};
use model::{Tag, Word, Call, Value};

/// Returns the [`Word`] in `v`, or panics if it's not a [`Value::Word`].
fn to_word(v: Value) -> Word {
    if let Value::Word(w) = v { w } else { panic!("Not a Word") }
}

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

impl Call for BuiltIn {
    fn call(&self, tag: &Tag, args: Vec<Value>) -> Vec<Value> {
        let mut args = args.into_iter();
        match (args.next(), args.next(), args.next()) {
            (None, _, _) => panic!("Not enough arguments"),
            (Some(x), None, _) => {
                let unary = self.unary.get(tag).expect("No such unary method");
                vec![Value::Word(unary(to_word(x)))]
            },
            (Some(x), Some(y), None) => {
                let binary = self.binary.get(tag).expect("No such binary method");
                vec![Value::Word(binary(to_word(x), to_word(y)))]
            },
            _ => panic!("Too many arguments"),
        }
    }
}

// ----------------------------------------------------------------------------

mod integer;
pub use integer::{compile_integer};
