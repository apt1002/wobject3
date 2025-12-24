pub mod model;
pub use model::{Bytes, Tag, Name, Map, Word, Value, Dynamic};

pub mod built_in;

mod code;
pub use code::{Opcode, call};

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
}
