use std::collections::{HashMap};
use std::rc::{Rc};

use super::model::{Tag, Tuple, Call, Value};

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
    New(Rc<dyn Call>),

    /// Call `Q`'s method `tag`, passing `Rn`, ..., `Rtop`.
    ///
    /// `Q` must be a `Value::Object`, and `tag` must match one of its methods.
    /// On entry to the callee, `Q` is the `self` value of the object.
    /// On exit, the `self` value of the object is replaced by `Q`.
    /// `Rn, ..., Rtop` are replaced by the callee's stack.
    Call(Tag, Pos),

    /// Infinite loop.
    Loop(Code),

    /// Construct a tagged tuple with fields `Rn, ..., Rtop, Q`.
    Tag(Tag, Pos),

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

// ----------------------------------------------------------------------------

/// A basic block of code.
#[derive(Debug)]
pub struct Code(Box<[Instruction]>, Terminal);

impl Code {
    pub fn instructions(&self) -> impl Iterator<Item=&Instruction> { self.0.iter() }
    pub fn terminal(&self) -> Terminal { self.1 }

    /// Execute a basic block.
    pub fn run(&self, r: &mut Vec<Value>) -> Terminal {
        let mut q = r.pop().expect("Underflow");
        for instruction in self.instructions() { match instruction {
            Instruction::Pop => {
                q = r.pop().expect("Underflow");
            },
            Instruction::Lit(v) => {
                r.push(q);
                q = v.clone();
            },
            Instruction::QDup(n) => {
                let new_q = if let Value::Structure(_, q) = &q {
                    q[n.as_usize()].clone()
                } else {
                    panic!("Q register is not a structure");
                };
                r.push(q);
                q = new_q;
            },
            Instruction::RDup(n) => {
                r.push(q);
                q = r[n.as_usize()].clone();
            },
            Instruction::Q(n) => {
                if let Value::Structure(_, q) = &mut q {
                    let q = q.make_mut(); // Clone-on-write.
                    let r_top = r.last_mut().expect("Underflow");
                    std::mem::swap(r_top, &mut q[n.as_usize()]);
                } else {
                    panic!("Q is not a structure");
                }
            },
            Instruction::R(n) => {
                std::mem::swap(&mut q, &mut r[n.as_usize()]);
            },
            Instruction::New(methods) => {
                q = Value::Object(methods.clone(), Box::new(q));
            },
            Instruction::Call(tag, n) => {
                if let Value::Object(methods, self_) = q {
                    r.push(*self_);
                    let args: Vec<Value> = r.drain(n.as_usize()..).collect();
                    let rets = methods.call(tag, args);
                    r.extend(rets);
                    let self_ = r.pop().expect("Underflow");
                    q = Value::Object(methods, Box::new(self_));
                } else {
                    panic!("Called a non-object");
                }
            },
            Instruction::Loop(code) => {
                r.push(q);
                loop {
                    match code.run(r) {
                        Terminal::Return => { return Terminal::Return; }
                        Terminal::Break => { break; }
                        _ => {}
                    }
                }
                q = r.pop().expect("Underflow");
            },
            Instruction::Tag(tag, n) => {
                r.push(q);
                let args: Vec<Value> = r.drain(n.as_usize()..).collect();
                q = Value::Structure(tag.clone(), Tuple::new(args));
            },
            Instruction::Switch(table, else_) => {
                if let Value::Structure(tag, tuple) = q {
                    let code = if let Some(code) = table.get(&tag) {
                        r.extend(tuple.iter().cloned());
                        code
                    } else if let Some(code) = else_ {
                        r.push(Value::Structure(tag, tuple));
                        code
                    } else {
                        panic!("Switch is not exhaustive");
                    };
                    match code.run(r) {
                        Terminal::FallThrough => {},
                        terminal => { return terminal; }
                    }
                    q = r.pop().expect("Underflow");
                }
            },
        }}
        r.push(q);
        self.terminal()
    }

}

// ----------------------------------------------------------------------------

/// A jump table.
#[derive(Debug)]
pub struct Table(HashMap<Tag, Code>);

impl Table {
    pub fn get(&self, tag: &Tag) -> Option<&Code> { self.0.get(tag) }
}

impl Call for Table {
    fn call(&self, tag: &Tag, mut args: Vec<Value>) -> Vec<Value> {
        let code = self.get(tag).expect("No such method");
        match code.run(&mut args) {
            Terminal::Break => panic!("Break at top level"),
            Terminal::Continue => panic!("Continue at top level"),
            _ => {},
        }
        args
    }
}
