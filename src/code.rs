use std::rc::{Rc};
use std::collections::{HashMap};

/// Represents a stack position, with `Pos(0)` being the bottom of the stack.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos(u16);

impl Pos {
    pub fn as_usize(&self) -> usize { self.0 as usize }
}

// ----------------------------------------------------------------------------

/// Represents a Welly constructor.
///
/// In source code, a `Tag`s is written as a name consisting only of capital
/// letters, digits and underscores and not beginning with a digit.
// TODO: Represent as a 64-bit integer.
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Tag(Rc<str>);

/// A jump table.
#[derive(Debug)]
pub struct Table(HashMap<Tag, Code>);

impl Table {
    pub fn get(&self, tag: &Tag) -> Option<&Code> { self.0.get(tag) }
}

// ----------------------------------------------------------------------------

/// The last instruction of a basic block.
#[derive(Debug, Copy, Clone)]
#[must_use]
pub enum Terminal {
    /// Return `R0`, ..., `Rtop` to the caller.
    Return,

    /// Break out of an [`Instruction::Loop`].
    Break,

    /// Repeat an [`Instruction::Loop`].
    Continue,

    /// Resume the enclosing block.
    FallThrough,
}

/// An instruction that can fall through.
///
/// Registers:
/// - `P0`, ... are values in the constant pool.
/// - `Q` is the accumulator.
/// - `Q0`, ... are the fields of `Q`, or undefined if `Q` is not a tuple.
/// - `R0`, ... are values on the stack.
///   - `R0` is the bottom of the stack.
///   - `Rtop` is the top of the stack.
#[derive(Debug)]
pub enum Instruction {
    /// Discard `Q` (decrease its reference count), then pop it from the stack.
    Pop,

    /// Push `Q` and replace it with a copy of `Pn` (increasing its reference
    /// count).
    PDup(Pos),

    /// Push `Q` and replace it with a copy of `Qn` (increasing its reference
    /// count).
    QDup(Pos),

    /// Push `Q` and replace it with a copy of `Rn` (increasing its reference
    /// count).
    RDup(Pos),

    /// Swap `Rtop` with `Qn`.
    Q(Pos),

    /// Swap `Q` with `Rn`.
    R(Pos),

    /// Call `Q`'s method `tag`, passing `Rn`, ..., `Rtop`.
    ///
    /// `Q` must be a `Value::Object`, and `tag` must match one of its methods.
    /// On entry to the callee, `Q` is the `self` value of the object.
    /// On exit, the `self` value of the object is replaced by `Q`.
    /// `Rn, ..., Rtop` are replaced by the callee's stack.
    Call(Tag, Pos),

    /// Infinite loop.
    Loop(Code),

    /// Switch on the top item.
    ///
    /// If `Q` is `Value::Structure(tag, v1, ..., v_n)` and the [`Table`]
    /// contains the key `tag`, we execute that case as follows:
    /// - Push `v1` to `v_n`.
    /// - Pop `Q`.
    /// - Run the [`Code`] corresponding to `key`.
    /// Otherwise, we execute the `else` `Code`
    Switch(Table, Option<Code>),
}

/// A basic block of code.
#[derive(Debug)]
pub struct Code(Box<[Instruction]>, Terminal);

impl Code {
    pub fn instructions(&self) -> impl Iterator<Item=&Instruction> { self.0.iter() }
    pub fn terminal(&self) -> Terminal { self.1 }
}

