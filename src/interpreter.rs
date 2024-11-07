use super::code::{Terminal, Instruction, Code};
use super::model::{Value};

/// One per active function call.
pub struct Frame<'a> {
    /// A pool of compile-time constants.
    p: &'a [Value],

    /// The data stack (`R0` to `Rtop`) followed by `Q`.
    r: Vec<Value>,
}

impl<'a> Frame<'a> {
    /// Execute a basic block.
    pub fn run(&mut self, code: &Code) -> Terminal {
        let mut q = self.r.pop().expect("Underflow");
        for instruction in code.instructions() { match instruction {
            Instruction::Pop => {
                q = self.r.pop().expect("Underflow");
            },
            Instruction::PDup(n) => {
                self.r.push(q);
                q = self.p[n.as_usize()].clone();
            },
            Instruction::QDup(n) => {
                let new_q = if let Value::Structure(_, q) = &q {
                    q[n.as_usize()].clone()
                } else {
                    panic!("Q register is not a structure");
                };
                self.r.push(q);
                q = new_q;
            },
            Instruction::RDup(n) => {
                self.r.push(q);
                q = self.r[n.as_usize()].clone();
            },
            Instruction::Q(n) => {
                if let Value::Structure(_, q) = &mut q {
                    let q = q.make_mut(); // Clone-on-write.
                    let r_top = self.r.last_mut().expect("Underflow");
                    std::mem::swap(r_top, &mut q[n.as_usize()]);
                } else {
                    panic!("Q is not a structure");
                }
            },
            Instruction::R(n) => {
                std::mem::swap(&mut q, &mut self.r[n.as_usize()]);
            },
            Instruction::Call(tag, n) => {
                if let Value::Object(combinator, self_) = q {
                    self.r.push(*self_);
                    let args: Vec<Value> = self.r.drain(n.as_usize()..).collect();
                    let mut callee = Frame {p: combinator.pool(), r: args};
                    match callee.run(combinator.get(tag)) {
                        Terminal::Break => panic!("Break at top level"),
                        Terminal::Continue => panic!("Continue at top level"),
                        _ => {},
                    }
                    self.r.extend(callee.r);
                    let self_ = self.r.pop().expect("Underflow");
                    q = Value::Object(combinator, Box::new(self_));
                } else {
                    panic!("Called a non-object");
                }
            },
            Instruction::Loop(code) => {
                self.r.push(q);
                loop {
                    match self.run(code) {
                        Terminal::Return => { return Terminal::Return; }
                        Terminal::Break => { break; }
                        _ => {}
                    }
                }
                q = self.r.pop().expect("Underflow");
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
                    q = self.r.pop().expect("Underflow");
                }
            },
        }}
        self.r.push(q);
        code.terminal()
    }
}
