pub mod model;
pub use model::{Tag, Name, Map, Word, Value, Dynamic};

pub mod built_in;

mod code;
pub use code::{Opcode, call};

// ----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
}
