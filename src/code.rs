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
/// - `Q` is the accumulator. `0` on entry to a block and discarded on exit.
/// - `R0`, ... are values on the stack.
///   - `R0` is the bottom of the stack.
///   - `Rtop` is the top of the stack.
/// - `S` is the `self` register.
/// - `S0`, ... are the fields of `S`, or undefined if `S` is not a tuple.
#[derive(Debug)]
pub enum Instruction {
    /// Discard `Q` (decrease its reference count), then pop it from the stack.
    Pop,

    /// Push a copy of `Q` onto the stack (increase its reference count).
    Push,

    /// Discard `Q` and replace it with a copy of `Pn`.
    PDup(Pos),

    /// Discard the accumulator and replace it with a copy of `Rn`.
    RDup(Pos),

    /// Discard the accumulator and replace it with a copy of `Sn`.
    SDup(Pos),

    /// Swap `Q` with `Rn`.
    R(Pos),

    /// Swap `Q` with `Sn`.
    S(Pos),

    /// Swap `Q` with `S`.
    QS,

    /// Call `Q`'s method `tag`, passing `Rn`, ..., `Rtop`.
    Call(Tag, Pos),

    /// Infinite loop.
    Loop(Code),

    /// Switch on the top item.
    Switch(Table, Option<Code>),
}

/// A basic block of code.
#[derive(Debug)]
pub struct Code(Box<[Instruction]>, Terminal);

impl Code {
    pub fn instructions(&self) -> impl Iterator<Item=&Instruction> { self.0.iter() }
    pub fn terminal(&self) -> Terminal { self.1 }
}

