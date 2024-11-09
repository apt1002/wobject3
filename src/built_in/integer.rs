use super::{model, Unary, Binary, BuiltIn};
use model::{Tag};

const UNARY: [(&'static str, Unary); 3] = [
    ("NEG", &|x| (-x.w()).into()),
    ("BIT_NOT", &|x| (!x.w()).into()),
    ("BOOL_NOT", &|x| (!x.b()).into()),
];

const BINARY: [(&'static str, Binary); 11] = [
    ("ADD", &|x, y| (x.w() + y.w()).into()),
    ("SUB", &|x, y| (x.w() - y.w()).into()),
    ("MUL", &|x, y| (x.w() * y.w()).into()),
    ("BIT_AND", &|x, y| (x.u() & y.u()).into()),
    ("BIT_OR", &|x, y| (x.u() | y.u()).into()),
    ("BIT_XOR", &|x, y| (x.u() ^ y.u()).into()),
    ("EQ", &|x, y| (x.u() == y.u()).into()),
    ("LE", &|x, y| (x.s() <= y.s()).into()),
    ("ULE", &|x, y| (x.u() <= y.u()).into()),
    ("BOOL_AND", &|x, y| (x.b() & y.b()).into()),
    ("BOOL_OR", &|x, y| (x.b() | y.b()).into()),
];

/// Constructs the method table for type `Integer`.
pub fn compile_integer() -> BuiltIn {
    BuiltIn {
        name: "Integer",
        unary: UNARY.iter().map(|(name, f)| (Tag::new(name), *f)).collect(),
        binary: BINARY.iter().map(|(name, f)| (Tag::new(name), *f)).collect(),
    }
}
