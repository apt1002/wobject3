use std::collections::{HashMap};
use std::{fmt};
use std::rc::{Rc};

use super::{model};
use model::{Tag, Word, Object, Value};

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

impl Object for Primitive {
    fn call(mut self: Rc<Self>, tag: &Tag, args: Vec<Value>) -> (Value, Vec<Value>) {
        let mut args = args.into_iter();
        let ret = match (args.next(), args.next()) {
            (None, _) => {
                let unary = self.methods.unary.get(tag).expect("No such unary method");
                unary(self.data)
            },
            (Some(y), None) => {
                let binary = self.methods.binary.get(tag).expect("No such binary method");
                binary(self.data, y.0.word())
            },
            _ => panic!("Too many arguments"),
        };
        Rc::make_mut(&mut self).data = ret;
        (Value(self), Vec::new())
    }

    fn word(&self) -> Word { self.data }
    fn dyn_clone(&self) -> Rc<dyn Object> { Rc::new(self.clone()) }
}

// ----------------------------------------------------------------------------

mod integer;
pub use integer::{compile_integer};
