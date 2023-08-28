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
pub use error::{DecodeError, DecodeResult};
pub use reader::TdfDeserializer;
pub use stringify::{StringifyError, TdfStringifier};
pub use tag::{RawTag, Tag, Tagged, TdfType};
pub use types::serialize_vec;
pub use types::{
    Blob, GroupSlice, ObjectId, ObjectType, TaggedUnion, TdfDeserialize, TdfDeserializeOwned,
    TdfMap, TdfSerialize, TdfSerializeOwned, TdfTyped, VarIntList, U12,
};
pub use writer::TdfSerializer;
