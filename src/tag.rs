//! Implementation for [`Tag`]s and [`TdfType`]s

use crate::{
    codec::{reader::TdfReader, Decodable},
    error::DecodeResult,
};

use super::error::DecodeError;
use std::fmt::{Debug, Display, Write};

/// Represents the tag for a tagged value. Contains the
/// tag itself and the type of value stored after
#[derive(Debug, PartialEq, Eq)]
pub struct Tagged {
    /// The decoded tag
    pub tag: Tag,
    /// The Tdf type after this tag
    pub ty: TdfType,
}

impl Decodable for Tagged {
    fn decode(r: &mut TdfReader) -> DecodeResult<Self> {
        let input: [u8; 4] = r.read_bytes()?;
        let ty: TdfType = TdfType::try_from(input[3])?;
        let mut output: [u8; 4] = [0, 0, 0, 0];

        output[0] |= (input[0] & 0x80) >> 1;
        output[0] |= (input[0] & 0x40) >> 2;
        output[0] |= (input[0] & 0x30) >> 2;
        output[0] |= (input[0] & 0x0C) >> 2;

        output[1] |= (input[0] & 0x02) << 5;
        output[1] |= (input[0] & 0x01) << 4;
        output[1] |= (input[1] & 0xF0) >> 4;

        output[2] |= (input[1] & 0x08) << 3;
        output[2] |= (input[1] & 0x04) << 2;
        output[2] |= (input[1] & 0x03) << 2;
        output[2] |= (input[2] & 0xC0) >> 6;

        output[3] |= (input[2] & 0x20) << 1;
        output[3] |= input[2] & 0x1F;

        let tag = Tag(output);

        Ok(Self { tag, ty })
    }
}

/// Decoded tag bytes type
#[derive(Debug, PartialEq, Eq)]
pub struct Tag(pub [u8; 4]);

impl From<&[u8]> for Tag {
    fn from(value: &[u8]) -> Self {
        let mut out = [0u8; 4];

        // Only copy the max of 4 bytes
        let len = value.len().min(4);
        out[0..len].copy_from_slice(value);

        Self(out)
    }
}

impl From<&[u8; 4]> for Tag {
    fn from(value: &[u8; 4]) -> Self {
        Self(*value)
    }
}

/// Tags are stored as the raw input to avoid extra
/// heap allocation so they must be converted to strings
/// for displaying
impl Display for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for byte in self.0 {
            // Skip empty key bytes
            if byte != 0 {
                f.write_char(byte as char)?;
            }
        }
        Ok(())
    }
}

/// Types from the Blaze packet system which are used to describe
/// what data needs to be decoded.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
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

impl Decodable for TdfType {
    fn decode(r: &mut TdfReader) -> DecodeResult<Self> {
        let value = r.read_byte()?;
        TdfType::try_from(value)
    }
}

/// Convert bytes back to tdf types
impl TryFrom<u8> for TdfType {
    type Error = DecodeError;

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
            ty => return Err(DecodeError::UnknownType { ty }),
        })
    }
}

#[cfg(test)]
mod test {
    use super::{Tag, TdfType};

    /// Tests that u8 values can be correctly converted
    /// into tdf types
    #[test]
    fn test_u8_tdf_type() {
        let types: [(u8, TdfType); 12] = [
            (0x0, TdfType::VarInt),
            (0x1, TdfType::String),
            (0x2, TdfType::Blob),
            (0x3, TdfType::Group),
            (0x4, TdfType::List),
            (0x5, TdfType::Map),
            (0x6, TdfType::TaggedUnion),
            (0x7, TdfType::VarIntList),
            (0x8, TdfType::ObjectType),
            (0x9, TdfType::ObjectId),
            (0xA, TdfType::Float),
            (0xC, TdfType::U12),
        ];

        for (value, expected) in types {
            let ty = TdfType::try_from(value).unwrap();
            assert_eq!(ty, expected);
        }

        for (expected, ty) in types {
            let value = ty as u8;
            assert_eq!(value, expected);
        }
    }

    #[test]
    fn test_tag_from_bytes() {
        // Valid tag
        let input = b"ABCD";
        let tag = Tag::from(input);
        assert_eq!(&tag.0, b"ABCD");

        // Short tag should be zero padded
        let input: &[u8] = b"AB";
        let tag = Tag::from(input);
        assert_eq!(&tag.0, b"AB\0\0");

        // Empty tag should be zeroed
        let input: &[u8] = &[];
        let tag = Tag::from(input);
        assert_eq!(&tag.0, b"\0\0\0\0");
    }
}
