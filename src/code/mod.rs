use super::{Bytes, Map, Value};

/// A `T` or a panic message represented as `Bytes`.
type Result<T> = std::result::Result<T, Bytes>;

// ----------------------------------------------------------------------------

/// The instruction set.
///
/// Code is represented as a [`[Value]`]. The first `Value` is an opcode
/// [`Value::Word`] consisting of `Self`s concatenated in little-endian order.
/// Subsequent `Value`s are the immediate constants required by them.
/// Then there is another opcode word, and so on.
#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Opcode(u8);

impl Opcode {
    /// Fetch more instructions. Must be opcode `0`.
    pub const FETCH: Self = Self(0);

    /// Push a copy of a local variable onto the data stack.
    pub const GET: Self = Self(1);

    /// Push a copy of a local variable onto the data stack.
    pub const SET: Self = Self(2);

    /// Swap a local variable with the top item of the data stack.
    pub const SWAP: Self = Self(3);

    /// Return the top item of the data stack.
    pub const RETURN: Self = Self(4);

    /// Return the top item of the data stack.
    pub const MATCH: Self = Self(5);

    /// Panic with the top item of the data stack as the message.
    pub const PANIC: Self = Self(6);

    /// Abandon the current code array and execute another instead.
    pub const JUMP: Self = Self(7);

    /// Call the top item of the stack, passing the next item.
    pub const CALL: Self = Self(8);

    /// Push a block onto the stack and execute it repeatedly.
    pub const LOOP: Self = Self(9);

    /// Drop the top item of the data stack.
    pub const DROP: Self = Self(10);

    /// Push a literal [`Value`] onto the data stack.
    pub const LITERAL: Self = Self(11);

    /// Pack items on top of the data stack into a tuple on the data stack.
    pub const PACK: Self = Self(12);

    /// Unpack the tuple on top of the data stack onto the data stack.
    pub const UNPACK: Self = Self(13);
}

// ----------------------------------------------------------------------------

/// Represents the state of a subroutine call while it is running.
#[derive(Default, Debug, Clone)]
struct Frame<'a> {
    /// Opcodes to execute.
    ir: u64,

    /// Code to execute.
    code: std::slice::Iter<'a, Value>,

    /// The data stack.
    r: Vec<Value>,

    /// The local variables.
    v: Map<Value>,
}

impl<'a> Frame<'a> {
    fn next_opcode(&mut self) -> Opcode {
        let ret = Opcode(self.ir as u8);
        self.ir >>= 9;
        ret
    }

    /// Pop a value from the data stack.
    fn pop(&mut self) -> Value { self.r.pop().expect("Pop") }

    /// Fetch an immediate `Value`.
    fn fetch(&mut self) -> &'a Value { self.code.next().expect("Fetch") }

    /// Fetch an immediate `u64`.
    fn fetch_u64(&mut self) -> u64 { self.fetch().word().u() }

    /// Fetch an immediate `usize`.
    fn fetch_usize(&mut self) -> usize { self.fetch_u64() as usize }

    /// Fetch an immediate `Rc<str>`.
    fn fetch_bytes(&mut self) -> &'a Bytes { self.fetch().bytes() }

    /// Fetch an immediate `&[Value]`.
    fn fetch_block(&mut self) -> &'a [Value] { &**self.fetch().values() }

    /// Fetch an immediate `&Map<Value>`.
    fn fetch_map(&mut self) -> &'a Map<Value> { &**self.fetch().map() }

    /// Replace `ir` and `code` with `target`.
    fn jump(&mut self, target: &'a [Value]) { self.ir = 0; self.code = target.iter(); }

    /// Execute to compute a return value.
    pub fn run(&mut self) -> Result<Value> {
        loop {
            match self.next_opcode() {
                Opcode::FETCH => { self.ir = self.fetch_u64(); },
                Opcode::GET => {
                    let name = self.fetch_bytes();
                    self.r.push(self.v[name].clone()); }
                Opcode::SET => {
                    let value = self.pop();
                    let name = self.fetch_bytes();
                    self.v.insert(name.clone(), value);
                },
                Opcode::SWAP => {
                    let name = self.fetch_bytes();
                    std::mem::swap(
                        self.r.last_mut().expect("Swap"),
                        self.v.get_mut(name).expect("Swap"),
                    );
                },
                Opcode::RETURN => { return Ok(self.pop()); },
                Opcode::MATCH => {
                    let cases = self.fetch_map();
                    let value = self.pop();
                    let [tag, payload] = value.unpack();
                    self.jump(cases[tag.bytes()].values());
                    self.r.push(payload.clone());
                },
                Opcode::PANIC => { return Err(self.fetch_bytes().clone()); },
                Opcode::JUMP => {
                    let block = self.fetch_block();
                    self.jump(block);
                },
                Opcode::DROP => { let _ = self.pop(); },
                Opcode::LITERAL => {
                    let value = self.fetch();
                    self.r.push(value.clone());
                },
                Opcode::PACK => {
                    let arity = self.fetch_usize();
                    let tuple: Vec<Value> = self.r.drain((self.r.len() - arity) ..).collect();
                    self.r.push(Value::Values(tuple.into()));
                },
                Opcode::UNPACK => {
                    let arity = self.fetch_usize();
                    let tuple = self.pop();
                    let tuple = tuple.values();
                    assert_eq!(tuple.len(), arity);
                    self.r.extend(tuple.iter().cloned())
                },
                Opcode::CALL => {
                    let function = self.pop();
                    let function = function.values();
                    let argument = self.pop();
                    self.r.push(call(&**function, argument)?);
                },
                _ => panic!("Undefined Opcode"),
            }
        }
    }
}

/// Execute `code`.
pub fn call(function: &[Value], argument: Value) -> Result<Value> {
    let mut frame = Frame::default();
    frame.jump(function);
    frame.r.push(argument);
    frame.run()
}
