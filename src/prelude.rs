//! Prelude for commonly used types re-exported

pub use crate::error::{DecodeError, DecodeResult};
pub use crate::stringify::{StringifyError, TdfStringifier};
pub use crate::tag::{RawTag, Tag, Tagged, TdfType};
pub use crate::types::{
    Blob, GroupSlice, ObjectId, ObjectType, TaggedUnion, TdfDeserialize, TdfDeserializeOwned,
    TdfMap, TdfSerialize, TdfSerializeOwned, TdfTyped, VarIntList, U12,
};
pub use crate::{reader::TdfDeserializer, writer::TdfSerializer};

#[cfg(feature = "derive")]
pub use tdf_derive::{TdfDeserialize, TdfSerialize, TdfTyped};
