use super::code::{Terminal, Instruction, Code};
use super::model::{Value};

/// One per active function call.
pub struct Frame<'a> {
    /// A pool of compile-time constants.
    p: &'a [Value],

    /// The data stack.
    r: Vec<Value>,

    /// The `self` value.
    s: Box<Value>,
}

impl<'a> Frame<'a> {
    /// Execute a basic block.
    pub fn run(&mut self, code: &Code) -> Terminal {
        let mut q = Value::Integer(0);
        for instruction in code.instructions() { match instruction {
            Instruction::Pop => {
                q = self.r.pop().expect("Underflow");
            },
            Instruction::Push => {
                self.r.push(q.clone());
            },
            Instruction::PDup(n) => {
                q = self.p[n.as_usize()].clone();
            },
            Instruction::RDup(n) => {
                q = self.r[n.as_usize()].clone();
            },
            Instruction::SDup(n) => {
                if let Value::Structure(_, s) = &*self.s {
                    q = s[n.as_usize()].clone();
                } else {
                    panic!("S register is not a structure");
                }
            },
            Instruction::R(n) => {
                std::mem::swap(&mut q, &mut self.r[n.as_usize()]);
            },
            Instruction::S(n) => {
                if let Value::Structure(_, s) = &mut *self.s {
                    let s = s.make_mut(); // Clone-on-write.
                    std::mem::swap(&mut q, &mut s[n.as_usize()]);
                } else {
                    panic!("S register is not a structure");
                }
            },
            Instruction::QS => {
                std::mem::swap(&mut q, &mut *self.s);
            },
            Instruction::Call(tag, n) => {
                let args: Vec<Value> = self.r.drain(n.as_usize()..).collect();
                if let Value::Object(combinator, self_) = q {
                    let mut callee = Frame {p: combinator.pool(), r: args, s: self_};
                    match callee.run(combinator.get(tag)) {
                        Terminal::Break => panic!("Break at top level"),
                        Terminal::Continue => panic!("Continue at top level"),
                        _ => {},
                    }
                    self.r.extend(callee.r);
                    let self_ = callee.s;
                    q = Value::Object(combinator, self_);
                } else {
                    panic!("Called a non-object");
                }
            },
            Instruction::Loop(code) => {
                loop {
                    match self.run(code) {
                        Terminal::Return => { return Terminal::Return; }
                        Terminal::Break => { break; }
                        _ => {}
                    }
                }
            },
            Instruction::Switch(table, else_) => {
                if let Value::Structure(tag, tuple) = q {
                    let code = if let Some(code) = table.get(&tag) {
                        self.r.extend(tuple.iter().cloned());
                        code
                    } else if let Some(code) = else_ {
                        self.r.push(Value::Structure(tag, tuple));
                        code
                    } else {
                        panic!("Switch is not exhaustive");
                    };
                    match self.run(code) {
                        Terminal::FallThrough => {},
                        terminal => { return terminal; }
                    }
                    q = Value::Integer(0);
                }
            },
        }}
        code.terminal()
    }
}
