use crate::error::TdfError;

/// Different types of tdfs that are known and able to be
/// parsed and encoded
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TdfType {
    /// Variable length integer
    VarInt = 0x0,
    /// Variable length string with null terminator
    String = 0x1,
    /// Variable length collection of bytes
    Blob = 0x2,
    /// Group of tdf tags
    Group = 0x3,
    /// List of tdf values
    List = 0x4,
    /// Ordered map of tdf values
    Map = 0x5,
    /// Union between a tag type and a value
    TaggedUnion = 0x6,
    /// List of variable length integers
    VarIntList = 0x7,
    /// Object type key (Component, Type)
    ObjectType = 0x8,
    /// Object ID (Component, Type, Id)
    ObjectId = 0x9,
    /// Floating point value
    Float = 0xA,
    /// Not yet fully decoded, seems to be some value with a string
    U12 = 0xC,
}

impl TryFrom<u8> for TdfType {
    type Error = TdfError;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0x0 => TdfType::VarInt,
            0x1 => TdfType::String,
            0x2 => TdfType::Blob,
            0x3 => TdfType::Group,
            0x4 => TdfType::List,
            0x5 => TdfType::Map,
            0x6 => TdfType::TaggedUnion,
            0x7 => TdfType::VarIntList,
            0x8 => TdfType::ObjectType,
            0x9 => TdfType::ObjectId,
            0xA => TdfType::Float,
            0xC => TdfType::U12,
            // Handle unknown types
            ty => return Err(TdfError::UnknownType { ty }),
        })
    }
}
