//! Implementation for [`Tag`]s and [`TdfType`]s

use crate::{
    error::DecodeResult,
    reader::TdfDeserializer,
    types::{
        float::skip_f32, group::GroupSlice, list::skip_list, map::skip_map,
        tagged_union::skip_tagged_union, var_int::skip_var_int, Blob, ObjectId, ObjectType,
        TdfDeserializeOwned, TdfGeneric, TdfSerializeOwned, VarIntList,
    },
    writer::TdfSerializer,
};

use super::error::DecodeError;
use std::fmt::{Debug, Display, Write};

/// Represents a raw byte tag string for example :
/// ```
/// let tag: &[u8] = b"TEST";
/// ```
///
/// Tags can only contain A-Z 0-9 and cannot be longer than 4 characters
/// numbers may not be encoded correctly
pub type RawTag<'a> = &'a [u8];

/// Represents the tag for a tagged value. Contains the
/// tag itself and the type of value stored after
#[derive(Debug)]
pub struct Tagged {
    /// The decoded tag
    pub tag: Tag,
    /// The Tdf type after this tag
    pub ty: TdfType,
}

impl Tagged {
    /// Skips an entire tagged value by reading the tag then using
    /// the specific skip implementation for that type
    pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
        let tag = Self::deserialize_owned(r)?;
        tag.ty.skip(r, false)
    }

    /// Serializes a tagged value from the raw tag and value type
    pub fn serialize_raw<S: TdfSerializer>(w: &mut S, tag: RawTag, value_type: TdfType) {
        debug_assert!(tag.len() <= 4, "Tag cannot be longer than 4 bytes");
        debug_assert!(!tag.is_empty(), "Tag cannot be empty");

        let mut output: [u8; 4] = [0, 0, 0, value_type as u8];
        let length: usize = tag.len();
        if length > 0 {
            output[0] |= (tag[0] & 0x40) << 1;
            output[0] |= (tag[0] & 0x10) << 2;
            output[0] |= (tag[0] & 0x0F) << 2;
        }
        if length > 1 {
            output[0] |= (tag[1] & 0x40) >> 5;
            output[0] |= (tag[1] & 0x10) >> 4;
            output[1] |= (tag[1] & 0x0F) << 4;
        }
        if length > 2 {
            output[1] |= (tag[2] & 0x40) >> 3;
            output[1] |= (tag[2] & 0x10) >> 2;
            output[1] |= (tag[2] & 0x0C) >> 2;
            output[2] |= (tag[2] & 0x03) << 6;
        }
        if length > 3 {
            output[2] |= (tag[3] & 0x40) >> 1;
            output[2] |= tag[3] & 0x1F;
        }
        w.write_slice(&output);
    }
}

impl TdfDeserializeOwned for Tagged {
    fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
        let input: [u8; 4] = r.read_fixed()?;
        let ty: TdfType = TdfType::try_from(input[3])?;
        let mut output: [u8; 4] = [0, 0, 0, 0];

        fn decode(m: u8, c: u8) -> u8 {
            if m | c == 0x00 {
                0x0
            } else if m & 0x40 == 0 {
                0x30 | c
            } else {
                m | c
            }
        }

        output[0] = decode((input[0] & 0x80) >> 1, (input[0] & 0x7C) >> 2);
        output[1] = decode(
            (input[0] & 2) << 5,
            ((input[0] & 1) << 4) | ((input[1] & 0xF0) >> 4),
        );
        output[2] = decode(
            (input[1] & 8) << 3,
            ((input[1] & 7) << 2) | ((input[2] & 0xC0) >> 6),
        );
        output[3] = decode((input[2] & 0x20) << 1, input[2] & 0x1F);

        let tag = Tag(output);

        Ok(Tagged { tag, ty })
    }
}

/// Decoded tag bytes type
#[derive(PartialEq, Eq, Clone, Copy)]
pub struct Tag(pub [u8; 4]);

impl From<RawTag<'_>> for Tag {
    fn from(value: RawTag) -> Self {
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

impl Debug for Tag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Tag({:?} {})", self.0, self)
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
    /// Variable length integers
    VarInt = 0x0,
    /// String values
    String = 0x1,
    /// Blob of bytes
    Blob = 0x2,
    /// Group of multiple tags
    Group = 0x3,
    /// List of values
    List = 0x4,
    /// Map of key value pairs
    Map = 0x5,
    /// Tagged unions
    TaggedUnion = 0x6,
    /// Variable length integer list
    VarIntList = 0x7,
    /// Type of an object
    ObjectType = 0x8,
    /// Type of an object with an ID
    ObjectId = 0x9,
    /// Float values
    Float = 0xA,
    /// Generic type
    Generic = 0xC,
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
            0xC => TdfType::Generic,
            ty => return Err(DecodeError::UnknownType { ty }),
        })
    }
}

impl TdfType {
    /// Skips the underlying type for this type
    pub fn skip(&self, r: &mut TdfDeserializer, heat_compat: bool) -> DecodeResult<()> {
        match self {
            TdfType::VarInt => skip_var_int(r),
            TdfType::String | TdfType::Blob => Blob::skip(r),
            TdfType::Group => GroupSlice::skip(r, heat_compat),
            TdfType::List => skip_list(r),
            TdfType::Map => skip_map(r),
            TdfType::TaggedUnion => skip_tagged_union(r, heat_compat),
            TdfType::VarIntList => VarIntList::skip(r),
            TdfType::ObjectType => ObjectType::skip(r),
            TdfType::ObjectId => ObjectId::skip(r),
            TdfType::Float => skip_f32(r),
            TdfType::Generic => TdfGeneric::skip(r),
        }
    }
}

impl TdfDeserializeOwned for TdfType {
    fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
        let value = r.read_byte()?;
        TdfType::try_from(value)
    }
}

impl TdfSerializeOwned for TdfType {
    fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
        let value = self as u8;
        w.write_byte(value);
    }
}

#[cfg(test)]
mod test {
    use crate::{
        types::TdfSerializeOwned, DecodeError, Tag, Tagged, TdfDeserialize, TdfDeserializer,
        TdfType,
    };

    static TYPE_MAPPING: &[(u8, TdfType)] = &[
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
        (0xC, TdfType::Generic),
    ];

    #[test]
    #[should_panic(expected = "Tag cannot be longer than 4 bytes")]
    fn test_tag_too_long() {
        let mut w = Vec::new();
        Tagged::serialize_raw(&mut w, b"TEST1", TdfType::VarInt);
    }

    #[test]
    #[should_panic(expected = "Tag cannot be empty")]
    fn test_tag_empty() {
        let mut w = Vec::new();
        Tagged::serialize_raw(&mut w, b"", TdfType::VarInt);
    }

    #[test]
    fn test_tag_serialize() {
        let tags: &[(&[u8], &[u8])] = &[
            (b"TEST", &[210, 92, 244]),
            (b"1234", &[69, 36, 212]),
            (b"AB12", &[134, 36, 82]),
            (b"AB", &[134, 32, 0]),
        ];

        let mut w = Vec::new();

        for (tag, expected) in tags {
            Tagged::serialize_raw(&mut w, tag, TdfType::VarInt);

            assert_eq!(&w[..3], *expected);

            w.clear();
        }
    }

    #[test]
    fn test_tag_from_bytes() {
        let tags: &[(&[u8], &[u8; 4])] = &[
            (b"TEST", b"TEST"),
            (b"1234", b"1234"),
            (b"AB", b"AB\0\0"),
            (b"", b"\0\0\0\0"),
        ];

        for (bytes, expected) in tags {
            let tag = Tag::from(*bytes);
            assert_eq!(&tag.0, *expected)
        }
    }

    #[test]
    fn test_tag_display() {
        let tags: &[(&[u8], &str)] = &[
            (b"TEST", "TEST"),
            (b"1234", "1234"),
            (b"AB", "AB"),
            (b"", ""),
        ];

        for (bytes, expected) in tags {
            let tag = Tag::from(*bytes);
            let str = tag.to_string();

            assert_eq!(str.as_str(), *expected)
        }
    }

    #[test]
    fn test_tdf_type_encoding() {
        let mut w = Vec::new();
        for (value, expected) in TYPE_MAPPING {
            expected.serialize_owned(&mut w);
            assert_eq!(w[0], *value);

            let mut r = TdfDeserializer::new(&w);
            let ty = TdfType::deserialize(&mut r).unwrap();
            assert_eq!(ty, *expected);

            w.clear();
        }
    }

    #[test]
    fn test_tdf_type_into_byte() {
        for (value, expected) in TYPE_MAPPING {
            let raw = *expected as u8;
            assert_eq!(*value, raw);
        }
    }

    #[test]
    fn test_tdf_type_from_byte() {
        for (value, expected) in TYPE_MAPPING {
            let ty = TdfType::try_from(*value).unwrap();
            assert_eq!(ty, *expected);
        }
    }

    #[test]
    fn test_tdf_type_from_byte_unknown() {
        let err = TdfType::try_from(0x99).unwrap_err();
        assert!(matches!(err, DecodeError::UnknownType { ty: 0x99 }));
    }
}
