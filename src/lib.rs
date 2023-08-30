pub mod error;
pub mod reader;

pub mod stringify;
pub mod tag;
pub mod types;
pub mod writer;

pub mod prelude;

// Type additions for serde serialization
#[cfg(feature = "serde")]
pub mod serde;

// Re-exports
pub use prelude::*;
pub use types::serialize_vec;
