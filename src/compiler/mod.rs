use std::{fmt};
use std::ops::{Index};
use std::rc::{Rc};

use super::{model, code};
use model::{Bytes, Map, Type, Value};
use code::{Opcode};

/// Some sort of interpreter.
///
/// This abstracts the difference between the compiler and the interpreter.
/// The interpreter implements a concrete interpretation of code that follows
/// just one control-flow path. The compiler implements an abstract
/// interpretation that follows all control-flow paths.
pub trait Interpret {
    /// Represents a value or a variable.
    type V: fmt::Debug + Clone;

    /// Returns a `Self::V` representing the constant `value`.
    fn from_constant(&mut self, value: Rc<Value>) -> Self::V;

    /// Tests whether `value` is a known constant, and returns it if so.
    fn to_constant(&self, value: Self::V) -> Option<Rc<Value>>;

    /// Computes the type of `V`. The result will be a constant or `None`.
    fn type_of(&self, value: Self::V) -> Type;

    /// Constructs a tuple.
    fn tuple(&mut self, values: &[Self::V]) -> Self::V;

    /// Extract the fields of a tuple.
    fn fields(&mut self, value: Self::V) -> Box<[Self::V]>;

    /// Perform a unary arithmetic operation.
    fn unary(&mut self, op: Opcode, value: Self::V) -> Self::V;

    /// Perform a binary arithmetic operation.
    fn binary(&mut self, op: Opcode, left: Self::V, right: Self::V) -> Self::V;

    /// Perform a mutation operation.
    fn mutate(&mut self, op: Opcode, structure: Self::V, index: Self::V, value: Self::V) -> Self::V;
}

// ----------------------------------------------------------------------------

/// A concrete implementation of [`Interpret`].
pub struct Interpreter;

impl Interpret for Interpreter {
    type V = Rc<Value>;

    fn from_constant(&mut self, value: Rc<Value>) -> Self::V { value }

    fn to_constant(&self, value: Self::V) -> Option<Rc<Value>> { Some(value) }

    fn type_of(&self, value: Self::V) -> Type { value.type_.clone() }

    fn tuple(&mut self, values: &[Self::V]) -> Self::V {
        todo!();
    }

    fn fields(&mut self, values: Self::V) -> Box<[Self::V]> {
        todo!();
    }

    fn unary(&mut self, op: Opcode, value: Self::V) -> Self::V {
        todo!();
    }

    fn binary(&mut self, op: Opcode, left: Self::V, right: Self::V) -> Self::V {
        todo!();
    }

    fn mutate(&mut self, op: Opcode, structure: Self::V, index: Self::V, value: Self::V) -> Self::V {
        todo!();
    }
}

// ----------------------------------------------------------------------------

/// The index of a data-flow node of a [`Compiler`].
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Node(usize);

/// Represents a data-flow instruction.
///
/// Instructions that compute more than one result are modelled as an
/// instruction that computes a tuple. Use [`Data::Field`] to extract the
/// individual results.
pub enum Data {
    /// An immediate constant.
    Constant(Rc<Value>),

    /// A value passed on entry to a basic block.
    Input,

    /// Construct a tuple from its fields.
    Tuple(Box<[Node]>),

    /// Extract a field of a tuple.
    Field(Node, usize),

    /// A unary arithmetic operation.
    Unary(Opcode, Node),

    /// A binary arithmetic operation.
    ///
    /// This is also used for calling functions. The compiled function is the
    /// first operand and the argument is the second operand.
    Binary(Opcode, Node, Node),

    /// An operation that mutates a struture.
    ///
    /// Operands:
    /// - The structure.
    /// - The index (or similar) at which to mutate it.
    /// - The value (or similar) to store there.
    ///
    /// Other instructions operating on the structure are guaranteed not to be
    /// executed after this one.
    Mutate(Node, Node, Node),
}

// ----------------------------------------------------------------------------

/// The index of a control-flow node of a [`Compiler`].
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Label(usize);

/// Represents a control-flow instruction.
pub enum Control {
    /// Panic with a message.
    Panic(Bytes),

    /// Exit the function returning the value computed by a `Node`.
    Return(Node),

    /// Jump to a `Label`, passing the value computed by a `Node`.
    Jump(Label, Node),

    /// If `Node` is true, jump to the first `Label` otherwise to the second.
    If(Node, Label, Label),

    /// Select a `Label` depending on the `Tag` computed by `Node`.
    Match(Node, Map<Label>),
}

// ----------------------------------------------------------------------------

/// An abstract implementation of [`Interpret`].
pub struct Compiler {
    /// Data-flow instructions. Indexed by `Node`.
    pub nodes: Vec<Data>,

    /// Control-flow instructions. Indexed by `Label`.
    pub labels: Vec<Control>,
}

impl Compiler {
    /// Compile `data` and return a `Node` representing its result.
    pub fn push(&mut self, data: Data) -> Node {
        let ret = Node(self.nodes.len());
        self.nodes.push(data);
        ret
    }
}

impl Index<Node> for Compiler {
    type Output = Data;

    fn index(&self, index: Node) -> &Self::Output { &self.nodes[index.0] }
}

impl Index<Label> for Compiler {
    type Output = Control;

    fn index(&self, index: Label) -> &Self::Output { &self.labels[index.0] }
}

impl Interpret for Compiler {
    type V = Node;

    /// Returns a `Self::V` representing the constant `value`.
    fn from_constant(&mut self, value: Rc<Value>) -> Self::V {
        self.push(Data::Constant(value))
    }

    /// Tests whether `value` is a known constant, and returns it if so.
    fn to_constant(&self, value: Self::V) -> Option<Rc<Value>> {
        if let Data::Constant(ref ret) = self[value] { Some(ret.clone()) } else { None }
    }

    /// Computes the type of `V`. The result will be a constant or `None`.
    fn type_of(&self, value: Self::V) -> Type {
        todo!();
    }

    fn tuple(&mut self, values: &[Self::V]) -> Self::V {
        todo!();
    }

    fn fields(&mut self, values: Self::V) -> Box<[Self::V]> {
        todo!();
    }

    fn unary(&mut self, op: Opcode, value: Self::V) -> Self::V {
        todo!();
    }

    fn binary(&mut self, op: Opcode, left: Self::V, right: Self::V) -> Self::V {
        todo!();
    }

    fn mutate(&mut self, op: Opcode, structure: Self::V, index: Self::V, value: Self::V) -> Self::V {
        todo!();
    }
}
