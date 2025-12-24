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
    // Basic control-flow and data-flow.

    /// Fetch more instructions. Must be opcode `0`.
    pub const FETCH: Self = Self(0x00);

    /// Panic with a message.
    /// The message is an imemdiate constant `Str`.
    pub const PANIC: Self = Self(0x01);

    /// Pop `function`. Pop `argument`. Push `function(argument)`.
    pub const CALL: Self = Self(0x02);

    /// Return the top item of the stack.
    pub const RETURN: Self = Self(0x03);

    /// Abandon the current code array and execute another instead.
    /// The target is an immediate constant `Slice`.
    pub const JUMP: Self = Self(0x04);

    /// Push a block onto the stack and execute it repeatedly.
    /// The target is an immediate constant `Slice`.
    pub const LOOP: Self = Self(0x05);

    /// Pop `condition: u64`. If it is non-zero, jump to a block.
    /// The target is an immediate constant `Slice`.
    pub const IF: Self = Self(0x06);

    /// Pop a `Slice`. UNpack it as `(tag: Str, value: Value)`. Push `value`.
    /// Jump to a block depending on the `tag`.
    /// The jump table is an immediate constant `Map`.
    pub const MATCH: Self = Self(0x07);

    // Stack data flow.

    /// Drop the top item of the stack.
    pub const DROP: Self = Self(0x08);

    /// Push a literal [`Value`] onto the stack.
    /// The literal is an immediate constant.
    pub const LITERAL: Self = Self(0x09);

    /// Push onto the stack another reference to the top item of the stack.
    pub const SHARE: Self = Self(0x0A);

    // Swap the top two stack items.
    pub const SWAP: Self = Self(0x0B);

    // Local variable data flow.

    /// Drop a local variable.
    /// The name is an immediate constant `Str`.
    pub const LOCAL_DROP: Self = Self(0x0C);

    /// Pop `value`. Set a local variable to `value`.
    /// The name is an immediate constant `Str`.
    pub const LOCAL_SET: Self = Self(0x0D);

    /// Push a copy of a local variable onto the stack.
    /// The name is an immediate constant.
    pub const LOCAL_SHARE: Self = Self(0x0E);

    /// Swap a local variable with the top item of the stack.
    /// The name is an immediate constant `Str`.
    pub const LOCAL_SWAP: Self = Self(0x0F);

    // Construction and destruction.

    /// Pop `arity` `Value`s, make a fresh `Slice` containing them, push it.
    /// `arity` is an immedite constant `u64`.
    pub const PACK: Self = Self(0x10);

    /// Pop a `Slice`, extract `arity` `Value`s from it, push them.
    /// The arity is an immedite constant `u64`.
    pub const UNPACK: Self = Self(0x11);

    /// Pop `value`. Pop `map: Map`. Set `map.name` to `value`.
    /// `name` is an immediate constant `Str`.
    pub const MAP_SET: Self = Self(0x12);

    /// Pop `map: Map`. Push a copy of `map.name`
    /// `name` is an immediate constant `Str`.
    pub const MAP_SHARE: Self = Self(0x13);

    /// Pop `value`. Push a tuple `(tag, value)`.
    /// `tag` is an immediate constant `Str`.
    pub const TAG: Self = Self(0x14);

    /// Clone the top item of the stack so that it becomes mutable.
    pub const CLONE: Self = Self(0x15);

    /// String arithmetic.

    const STRING_START: u8 = 0x20;

    /// Pop `s: Str`. Push `s`. Push `s.length`.
    pub const STRING_LENGTH: Self = Self(Self::STRING_START + 0x00);

    /// Pop `v: u64`. Pop `i: u64`. Pop `s: Str`. Set `s[i]` to `v`. Push `s`.
    pub const STRING_SET_ITEM: Self = Self(Self::STRING_START + 0x01);

    /// Pop `i: u64`. Pop `s: Str`. Push `s[i]`.
    pub const STRING_GET_ITEM: Self = Self(Self::STRING_START + 0x02);

    /// Pop `v: 64`. Pop `i: u64`. Pop `s: Str`. Swap `s[i]` with `v`. Push `s`.
    pub const STRING_SWAP_ITEM: Self = Self(Self::STRING_START + 0x03);

    /// Pop `l: u64`. Push a fresh `Str` of length `l` of zero bytes.
    pub const STRING_NEW: Self = Opcode(Self::STRING_START + 0x04);

    /// Pop `t: Str`. Pop `i: u64`. Pop `s: Str`.
    /// Set `t.length` bytes of `s` starting at `i` to `t`.
    /// Push `s`.
    pub const STRING_SET_RANGE: Self = Opcode(Self::STRING_START + 0x05);

    /// Pop `l: u64`. Pop `i: u64`. Pop `s: Str`.
    /// Push a fresh `Str` of length `l` copying from  `s` starting at `i`.
    pub const STRING_SHARE_RANGE: Self = Opcode(Self::STRING_START + 0x06);

    /// Pop `t: Str`. Pop `i: u64`. Pop `s: Str`.
    /// Swap `l` bytes of `s` starting at `i` with `t`.
    /// Push `s`. Push `t`.
    pub const STRING_SWAP_RANGE: Self = Opcode(Self::STRING_START + 0x07);

    /// Array arithmetic.

    const ARRAY_START: u8 = 0x28;

    /// Pop `s: Slice`. Push `s`. Push `s.length`.
    pub const ARRAY_LENGTH: Self = Self(Self::ARRAY_START + 0x00);

    /// Pop `v: u64`. Pop `i: u64`. Pop `s: Slice`. Set `s[i]` to `v`. Push `s`.
    pub const ARRAY_SET_ITEM: Self = Self(Self::ARRAY_START + 0x01);

    /// Pop `i: u64`. Pop `s: Slice`. Push `s[i]`.
    pub const ARRAY_GET_ITEM: Self = Self(Self::ARRAY_START + 0x02);

    /// Pop `v: 64`. Pop `i: u64`. Pop `s: Slice`. Swap `s[i]` with `v`. Push `s`.
    pub const ARRAY_SWAP_ITEM: Self = Self(Self::ARRAY_START + 0x03);

    /// Pop `l: u64`. Push a fresh `Slice` of length `l` of `UNINITIALISED`.
    pub const ARRAY_NEW: Self = Opcode(Self::ARRAY_START + 0x04);

    /// Pop `t: Slice`. Pop `i: u64`. Pop `s: Slice`.
    /// Set `t.length` `Value`s of `s` starting at `i` to `t`.
    /// Push `s`.
    pub const ARRAY_SET_RANGE: Self = Opcode(Self::ARRAY_START + 0x05);

    /// Pop `l: u64`. Pop `i: u64`. Pop `s: Slice`.
    /// Push a fresh `Slice` of length `l` copying from  `s` starting at `i`.
    pub const ARRAY_SHARE_RANGE: Self = Opcode(Self::ARRAY_START + 0x06);

    /// Pop `t: Slice`. Pop `i: u64`. Pop `s: Slice`.
    /// Swap `l` `Value`s of `s` starting at `i` with `t`.
    /// Push `s`. Push `t`.
    pub const ARRAY_SWAP_RANGE: Self = Opcode(Self::ARRAY_START + 0x07);

    // Integer arithmetic.

    const INT_START: u8 = 0x30;

    pub const INT_ADD: Self = Opcode(Self::INT_START + 0x00);
    pub const INT_MULTIPLY: Self = Opcode(Self::INT_START + 0x01);
    pub const INT_EQUAL: Self = Opcode(Self::INT_START + 0x02);
    pub const INT_NEGATE: Self = Opcode(Self::INT_START + 0x03);

    pub const INT_AND: Self = Opcode(Self::INT_START + 0x04);
    pub const INT_OR: Self = Opcode(Self::INT_START + 0x05);
    pub const INT_XOR: Self = Opcode(Self::INT_START + 0x06);
    pub const INT_INVERT: Self = Opcode(Self::INT_START + 0x07);

    pub const INT_SIGNED_LESS_THAN: Self = Opcode(Self::INT_START + 0x08);
    pub const INT_UNSIGNED_LESS_THAN: Self = Opcode(Self::INT_START + 0x09);
    pub const INT_SIGNED_DIVIDE_REMAINDER: Self = Opcode(Self::INT_START + 0x0A);
    pub const INT_UNSIGNED_DIVIDE_REMAINDER: Self = Opcode(Self::INT_START + 0x0B);

    pub const INT_SIGNED_SHIFT_RIGHT: Self = Opcode(Self::INT_START + 0x0C);
    pub const INT_UNSIGNED_SHIFT_RIGHT: Self = Opcode(Self::INT_START + 0x0D);
    pub const INT_SHIFT_LEFT: Self = Opcode(Self::INT_START + 0x0E);
    pub const INT_POWER: Self = Opcode(Self::INT_START + 0x0F);

    // Floating-point arithmetic.

    const FLOAT_START: u8 = 0x40;

    pub const FLOAT_ADD: Self = Opcode(Self::FLOAT_START + 0x00);
    pub const FLOAT_MULTIPLY: Self = Opcode(Self::FLOAT_START + 0x01);
    pub const FLOAT_EQUAL: Self = Opcode(Self::FLOAT_START + 0x02);
    pub const FLOAT_NEGATE: Self = Opcode(Self::FLOAT_START + 0x03);

    pub const FLOAT_LESS_THAN: Self = Opcode(Self::FLOAT_START + 0x04);
    pub const FLOAT_DIVIDE_REMAINDER: Self = Opcode(Self::FLOAT_START + 0x05);
    pub const FLOAT_SHIFT_LEFT: Self = Opcode(Self::FLOAT_START + 0x06);
    pub const FLOAT_POWER: Self = Opcode(Self::FLOAT_START + 0x07);
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

    /// Pop a value from the stack.
    fn pop(&mut self) -> Value { self.r.pop().expect("Pop") }

    /// Peek at the top value on the stack.
    fn top(&mut self) -> &mut Value { self.r.last_mut().expect("Top") }

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
                Opcode::PANIC => { return Err(self.fetch_bytes().clone()); },
                Opcode::CALL => {
                    let function = self.pop();
                    let function = function.values();
                    let argument = self.pop();
                    self.r.push(call(&**function, argument)?);
                },
                Opcode::RETURN => { return Ok(self.pop()); },
                Opcode::JUMP => {
                    let block = self.fetch_block();
                    self.jump(block);
                },
                Opcode::LOOP => todo!(),
                Opcode::IF => todo!(),
                Opcode::MATCH => {
                    let cases = self.fetch_map();
                    let top = self.pop();
                    let [tag, payload] = top.unpack();
                    self.jump(cases[tag.bytes()].values());
                    self.r.push(payload.clone());
                },
                Opcode::DROP => { let _ = self.pop(); },
                Opcode::LITERAL => {
                    let value = self.fetch();
                    self.r.push(value.clone());
                },
                Opcode::SHARE => {
                    let value = self.top().clone();
                    self.r.push(value);
                },
                Opcode::SWAP => {
                    let mut value = self.pop();
                    std::mem::swap(&mut value, self.top());
                    self.r.push(value);
                },
                Opcode::LOCAL_DROP => {
                    let name = self.fetch_bytes();
                    self.v.remove(name);
                },
                Opcode::LOCAL_SET => {
                    let name = self.fetch_bytes();
                    let top = self.pop();
                    self.v.insert(name.clone(), top);
                },
                Opcode::LOCAL_SHARE => {
                    let name = self.fetch_bytes();
                    self.r.push(self.v[name].clone());
                },
                Opcode::LOCAL_SWAP => {
                    let name = self.fetch_bytes();
                    std::mem::swap(
                        self.r.last_mut().expect("Swap"),
                        self.v.get_mut(name).expect("Swap"),
                    );
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
                Opcode::MAP_SET => {
                    let name = self.fetch_bytes();
                    let top = self.pop();
                    self.top().map_mut().insert(name.clone(), top);
                },
                Opcode::MAP_SHARE => {
                    let name = self.fetch_bytes();
                    let value = self.top().map()[name].clone();
                    self.r.push(value);
                },
                Opcode::TAG => {
                    let tag = Value::Bytes(self.fetch().bytes().clone());
                    let value = self.pop();
                    self.r.push([tag, value].into());
                },
                Opcode::CLONE => { self.top().make_mut(); },
                Opcode::STRING_LENGTH => {
                    let l = self.pop().bytes().0.len() as u64;
                    self.r.push(l.into());
                },
                Opcode::STRING_SET_ITEM => {
                    let v = self.pop().word().u() as u8;
                    let i = self.pop().word().u() as usize;
                    self.top().bytes_mut()[i] = v;
                },
                Opcode::STRING_GET_ITEM => {
                    let i = self.pop().word().u() as usize;
                    let v = self.pop().bytes().0[i];
                    self.r.push((v as u64).into());
                },
                Opcode::STRING_SWAP_ITEM => {
                    let mut v = self.pop().word().u() as u8;
                    let i = self.pop().word().u() as usize;
                    std::mem::swap(&mut v, &mut self.top().bytes_mut()[i]);
                },
                Opcode::STRING_NEW => {
                    let l = self.pop().word().u() as usize;
                    self.r.push(Value::Bytes(Bytes(std::iter::repeat(0u8).take(l).collect())));
                },
                Opcode::STRING_SET_RANGE => {
                    let t = self.pop();
                    let ts = t.bytes();
                    let i = self.pop().word().u() as usize;
                    let s = self.top().bytes_mut();
                    assert!(s.len() >= i);
                    assert!(s.len() - i >= ts.0.len());
                    for j in 0..ts.0.len() { s[i + j] = ts.0[i]; }
                },
                Opcode::STRING_SHARE_RANGE => {
                    let l = self.pop().word().u() as usize;
                    let i = self.pop().word().u() as usize;
                    let s = self.pop();
                    self.r.push(Value::Bytes(s.bytes().0[i..][..l].into()));
                },
                Opcode::STRING_SWAP_RANGE => {
                    let mut t = self.pop();
                    let ts = t.bytes_mut();
                    let i = self.pop().word().u() as usize;
                    let s = self.top().bytes_mut();
                    assert!(s.len() >= i);
                    assert!(s.len() - i >= ts.len());
                    for j in 0..ts.len() {
                        std::mem::swap(&mut s[i + j], &mut ts[j]);
                    }
                    self.r.push(t);
                },
                Opcode::ARRAY_LENGTH => {
                    let l = self.pop().values().len() as u64;
                    self.r.push(l.into());
                },
                Opcode::ARRAY_SET_ITEM => {
                    let v = self.pop();
                    let i = self.pop().word().u() as usize;
                    self.top().values_mut()[i] = v;
                },
                Opcode::ARRAY_GET_ITEM => {
                    let i = self.pop().word().u() as usize;
                    let v = self.top().values()[i].clone();
                    self.r.push(v);
                },
                Opcode::ARRAY_SWAP_ITEM => {
                    let mut v = self.pop();
                    let i = self.pop().word().u() as usize;
                    std::mem::swap(&mut v, &mut self.top().values_mut()[i]);
                },
                Opcode::ARRAY_NEW => {
                    let l = self.pop().word().u() as usize;
                    self.r.push(Value::Values(std::iter::repeat(Value::UNINITIALISED).take(l).collect()));
                },
                Opcode::ARRAY_SET_RANGE => {
                    let t = self.pop();
                    let ts = t.values();
                    let i = self.pop().word().u() as usize;
                    let s = self.top().values_mut();
                    assert!(s.len() >= i);
                    assert!(s.len() - i >= ts.len());
                    for j in 0..ts.len() { s[i + j] = ts[i].clone(); }
                },
                Opcode::ARRAY_SHARE_RANGE => {
                    let l = self.pop().word().u() as usize;
                    let i = self.pop().word().u() as usize;
                    let s = self.pop();
                    self.r.push(Value::Values(s.values()[i..][..l].into()));
                },
                Opcode::ARRAY_SWAP_RANGE => {
                    let mut t = self.pop();
                    let ts = t.values_mut();
                    let i = self.pop().word().u() as usize;
                    let s = self.top().values_mut();
                    assert!(s.len() >= i);
                    assert!(s.len() - i >= ts.len());
                    for j in 0..ts.len() {
                        std::mem::swap(&mut s[i + j], &mut ts[j]);
                    }
                    self.r.push(t);
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
