use std::collections::{HashMap};
use std::rc::{Rc};

use super::model::{Tag, Value, Constructor, Tuple, NewTuple};

/// Represents a stack position, with `Pos(0)` being the bottom of the stack.
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Pos(u16);

impl Pos {
    pub fn as_usize(&self) -> usize { self.0 as usize }
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
/// - `Q` is the accumulator.
/// - `Q0`, ... are the fields of `Q`, or undefined if `Q` is not a tuple.
/// - `R0`, ... are [`Value`]s on the stack.
///   - `R0` is the bottom of the stack.
///   - `Rtop` is the top of the stack.
#[derive(Debug)]
pub enum Instruction {
    /// Discard `Q` (decrease its reference count), then pop it from the stack.
    Pop,

    /// Push `Q` and replace it with a copy of the [`Value`] (increasing its
    /// reference count).
    Lit(Value),

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

    /// Construct a closure with the specified methods and `Q` as `self`.
    New(Rc<dyn NewTuple>, Pos),

    /// Call `Q`'s method `tag`, passing `Rn`, ..., `Rtop`.
    ///
    /// `tag` must match one of the methods of the [`Value`] `Q`.
    /// On exit, `Rn, ..., Rtop` are replaced by the callee's stack.
    /// `Q` is passed to the callee on entry, and returned on exit.
    /// Methods that wish to consume `Q` should return a dummy value in `Q`,
    /// e.g. the integer `0`.
    Call(Tag, Pos),

    /// Infinite loop.
    Loop(Code),

    /// Switch on the top item.
    ///
    /// If `Q.methods` is a [`Tag`] `tag` and the [`Table`] contains the key
    /// `tag`, run the corresponding [`Code`].
    /// Otherwise, we execute the `else` `Code`
    Switch(Table, Option<Code>),
}

// ----------------------------------------------------------------------------

/// A basic block of code.
#[derive(Debug)]
pub struct Code(Box<[Instruction]>, Terminal);

impl Code {
    pub fn instructions(&self) -> impl Iterator<Item=&Instruction> { self.0.iter() }
    pub fn terminal(&self) -> Terminal { self.1 }

    /// Execute a basic block.
    pub fn run(&self, mut q: Value, r: &mut Vec<Value>) -> (Value, Terminal) {
        for instruction in self.instructions() { match instruction {
            Instruction::Pop => {
                q = r.pop().expect("Underflow");
            },
            Instruction::Lit(v) => {
                r.push(q);
                q = v.clone();
            },
            Instruction::QDup(n) => {
                let new_q = q.0.tuple_ref()[n.as_usize()].clone();
                r.push(q);
                q = new_q;
            },
            Instruction::RDup(n) => {
                r.push(q);
                q = r[n.as_usize()].clone();
            },
            Instruction::Q(n) => {
                let q_mut = q.make_mut().tuple_mut(); // Clone-on-write.
                let r_top = r.last_mut().expect("Underflow");
                std::mem::swap(r_top, &mut q_mut[n.as_usize()]);
            },
            Instruction::R(n) => {
                std::mem::swap(&mut q, &mut r[n.as_usize()]);
            },
            Instruction::New(constructor, n) => {
                r.push(q);
                q = constructor.clone().new_tuple(r.drain(n.as_usize()..))
            },
            Instruction::Call(tag, n) => {
                let args: Vec<Value> = r.drain(n.as_usize()..).collect();
                let rets: Vec<Value>;
                (q, rets) = q.0.call(tag, args);
                r.extend(rets);
            },
            Instruction::Loop(code) => {
                loop {
                    let terminal: Terminal;
                    (q, terminal) = code.run(q, r);
                    match terminal {
                        Terminal::Return => { return (q, Terminal::Return); }
                        Terminal::Break => { break; }
                        _ => {}
                    }
                }
            },
            Instruction::Switch(table, else_) => {
                let code = table.get(q.0.tag()).or(else_.as_ref()).expect("Switch is not exhaustive");
                let terminal: Terminal;
                (q, terminal) = code.run(q, r);
                match terminal {
                    Terminal::FallThrough => {},
                    terminal => { return (q, terminal); }
                }
            },
        }}
        (q, self.terminal())
    }

}

// ----------------------------------------------------------------------------

/// A jump table.
#[derive(Debug)]
pub struct Table(HashMap<Tag, Code>);

impl Table {
    pub fn get(&self, tag: &Tag) -> Option<&Code> { self.0.get(tag) }
}

impl Constructor for Table {
    fn call<const N: usize>(
        &self,
        object: Rc<Tuple<Self, N>>,
        tag: &Tag,
        mut args: Vec<Value>,
    ) -> (Value, Vec<Value>)
    where Self: Sized {
        let code = self.get(tag).expect("No such method");
        let (object, terminal) = code.run(Value(object), &mut args);
        match terminal {
            Terminal::Break => panic!("Break at top level"),
            Terminal::Continue => panic!("Continue at top level"),
            _ => {},
        }
        (object, args)
    }
}
