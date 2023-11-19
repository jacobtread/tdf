//! Types implementation for custom types used while encoding values
//! with Blaze packets

pub use blob::Blob;
pub use group::GroupSlice;
pub use map::TdfMap;
pub use object_id::ObjectId;
pub use object_type::ObjectType;
pub use tagged_union::TaggedUnion;
pub use u12::U12;
pub use var_int_list::VarIntList;

use crate::{error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer};

/// Serializes the provided [TdfSerialize] type
/// as a Vec of bytes
pub fn serialize_vec<V>(value: &V) -> Vec<u8>
where
    V: TdfSerialize,
{
    let mut output = Vec::new();
    value.serialize(&mut output);
    output
}

#[cfg(feature = "bytes")]
pub mod bytes {
    //! Provides helper functions for the "bytes" feature flag

    use bytes::Bytes;

    use crate::{serialize_vec, TdfSerialize};

    /// Serializes the provided [TdfSerialize] type
    /// as a [Bytes]
    #[inline]
    pub fn serialize_bytes<V>(value: &V) -> Bytes
    where
        V: TdfSerialize,
    {
        Bytes::from(serialize_vec(value))
    }
}

/// [TdfDeserialize] trait implemented by structures that can
/// be deserialized from bytes of tdf values
///
/// This trait can be implemented by any custom structure in order
/// to provide a deserialization implementation for the structure.
///
/// ## Typed Deserialization
///
/// If you would like to use the structure as a value that can be decoded
/// from a tagged type you must implement [TdfTyped] to map it to the
/// desired [TdfType]. Your deserialization must match the chosen type
/// otherwise the reader may be unable to deserialize any further values.
///
/// If you are reading a group type and don't indent to completely
/// read all the values you will need to skip the remaining data with
/// [GroupSlice::deserialize_content_skip]
///
/// ## Implementing Deserialization
///
/// See [Deserialization](crate::reader) for all the reading functions that
/// can be used
///
/// ### Example Without Type
///
/// Below is an example of a deserializer you may use for a packet structure
///
/// ```
/// use tdf::prelude::*;
///
/// struct MyCustomStruct<'de> {
///     test: u32,
///     value: &'de str
/// }
///
/// impl<'de> TdfDeserialize<'de> for MyCustomStruct<'de> {
///     fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
///         let test: u32 = r.tag(b"TEST")?;
///         let value: &str = r.tag(b"VALU")?;
///         Ok(Self { test, value })
///     }
/// }
///
/// ```
///
/// ### Group Example
///
/// Below is an example of deserializing a structure as a [TdfType::Group] type. When
/// deserializing a group ensure that if you arent completely reading all the fields
/// that you call [GroupSlice::deserialize_content_skip] to skip the remaining data
///
///
/// ```
/// use tdf::prelude::*;
///
/// struct MyCustomStruct<'de> {
///     test: u32,
///     value: &'de str
/// }
///
/// impl<'de> TdfDeserialize<'de> for MyCustomStruct<'de> {
///     fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
///         let test: u32 = r.tag(b"TEST")?;
///         let value: &str = r.tag(b"VALU")?;
///         GroupSlice::deserialize_content_skip(r)?;
///         Ok(Self { test, value })
///     }
/// }
///
/// impl TdfTyped for MyCustomStruct<'_> {
///     const TYPE: TdfType = TdfType::Group;
/// }
///
/// ```
pub trait TdfDeserialize<'de>: Sized {
    /// Deserialize this value from the provided deserializer
    fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self>;
}

/// [TdfDeserializeOwned] is an alternative to [TdfDeserialize] where the
/// deserialization lifetime 'de is not required in order to deserialize the
/// value.
///
/// See [TdfDeserialize] for deserialization examples
pub trait TdfDeserializeOwned: Sized {
    /// Deserialize this value from the provided deserializer
    fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self>;
}

/// All types that implement [TdfDeserializeOwned] also implement [TdfDeserialize]
impl<T> TdfDeserialize<'_> for T
where
    T: TdfDeserializeOwned,
{
    #[inline]
    fn deserialize(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
        Self::deserialize_owned(r)
    }
}

/// [TdfSerialize] trait implemented by structures that can be
/// serialized into bytes of tdf values
///
/// This trait can be implemented by any custom structure in order
/// to provide a serialization implementation for the structure.
///
/// ## Typed Serialization
///
/// If you would like to use the structure as a tag value you will need
/// to implement [TdfTyped] to map it to a [TdfType]. Your serialization
/// must match the structure for the [TdfType] that you choose.
///
/// If you are serializing a group ([TdfType::Group]) of values you must
/// ensure that you call [tag_group_end](TdfSerializer::tag_group_end) at
/// the end of your implementation to end the group
///
/// ## Implementing TdfSerialize
///
/// See [Serialization](crate::writer) for all the tagging and writing functions
/// that can be used
///
/// ### Example Without Type
///
/// Below is an example of a serializer you may use for a packet structure
///
/// ```
/// use tdf::prelude::*;
///
/// struct MyCustomStruct {
///     value: u32,
/// }
///
/// impl TdfSerialize for MyCustomStruct {
///     fn serialize<S: TdfSerializer>(&self, w: &mut S) {
///         w.tag_u32(b"TEST", self.value);
///         w.tag_str(b"VALU", "This is an example string");
///     }
/// }
///
/// ```
///
/// ### Group Example
///
/// Below is an example of serializing a structure as a [TdfType::Group] type. When
/// serializing a group ensure that you call [tag_group_end](TdfSerializer::tag_group_end)
/// after your writing logic
///
///
/// ```
/// use tdf::prelude::*;
///
/// struct MyCustomStruct {
///     value: u32,
/// }
///
/// impl TdfSerialize for MyCustomStruct {
///     fn serialize<S: TdfSerializer>(&self, w: &mut S) {
///         w.tag_u32(b"TEST", self.value);
///         w.tag_str(b"VALU", "This is an example string");
///         w.tag_group_end();
///     }
/// }
///
/// impl TdfTyped for MyCustomStruct {
///     const TYPE: TdfType = TdfType::Group;
/// }
///
/// ```
pub trait TdfSerialize: Sized {
    /// Serialize this value into the provided serializer
    fn serialize<S: TdfSerializer>(&self, w: &mut S);
}

/// INTERNAL API
///
/// You should not implement this, this is only intended for serializing
/// primitives internally through writer APIs without double dereferencing
///
/// [TdfSerializeOwned] trait implemented by structures that
/// can be serialized in their owned value form.
///
/// This is implemented by primitive values to prevent unnessciary
/// copying when serializing values that have already been copied
pub trait TdfSerializeOwned: Sized {
    /// Serialize this value into the provided serializer
    ///
    /// Value is serialized using an owned copy of the value
    /// actual implementation is similar to normal serialization
    /// see [TdfSerialize] for examples
    fn serialize_owned<S: TdfSerializer>(self, w: &mut S);
}

/// Associated trait for types that can be encoded/decoded
/// as a specific [TdfType] rather than just a generic
/// encoding and decoding
pub trait TdfTyped {
    /// The [TdfType] this value is represented as
    const TYPE: TdfType;
}

pub mod var_int {
    //! Variable-length integer related implementations and related
    //! help functions

    use super::{TdfDeserializeOwned, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
        TdfSerialize,
    };

    /// Skips amy un-read bytes from a variable-length integer
    pub fn skip_var_int(r: &mut TdfDeserializer) -> DecodeResult<()> {
        let mut byte = r.read_byte()?;
        while byte >= 0x80 {
            byte = r.read_byte()?;
        }
        Ok(())
    }

    /// Macro for implementing variable-length integer serialization
    /// for a specific integer type
    ///
    /// # Arguments
    /// * value - The value to use for serializing
    /// * w - The writer to use to write the output
    macro_rules! impl_serialize_int {
        ($value:ident, $w:ident) => {{
            if $value < 0x40 {
                $w.write_byte($value as u8);
                return;
            }

            $w.write_byte((($value & 0x3f) | 0x80) as u8);

            let mut value = $value >> 6;
            while value >= 0x80 {
                $w.write_byte(((value & 0x7f) | 0x80) as u8);
                value >>= 7;
            }

            $w.write_byte(value as u8)
        }};
    }

    /// Macro for implementing variable-length integer deserialization
    /// for a specific integer type
    ///
    /// # Arguments
    /// * ty - The type that is being deserialized
    /// * r - The reader to read bytes from
    macro_rules! impl_deserialize_int {
        ($ty:ty, $r:ident) => {{
            let mut byte: u8 = $r.read_byte()?;
            let mut result: $ty = (byte & 0x3F) as $ty;
            let mut shift = 6;
            while byte >= 0x80 {
                byte = $r.read_byte()?;
                result |= ((byte & 0x7f) as $ty).wrapping_shl(shift);
                shift += 7;
            }

            Ok(result)
        }};
    }

    impl TdfDeserializeOwned for bool {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u8::deserialize_owned(r)?;
            Ok(value == 1)
        }
    }

    impl TdfSerializeOwned for bool {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            w.write_byte(self as u8)
        }
    }

    impl TdfSerialize for bool {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for bool {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u8

    impl TdfDeserializeOwned for u8 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u8, r)
        }
    }

    impl TdfSerializeOwned for u8 {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfSerialize for u8 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for u8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i8

    impl TdfDeserializeOwned for i8 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u8::deserialize_owned(r)?;
            Ok(value as i8)
        }
    }

    impl TdfSerializeOwned for i8 {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            (self as u8).serialize_owned(w)
        }
    }

    impl TdfSerialize for i8 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for i8 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u16

    impl TdfDeserializeOwned for u16 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u16, r)
        }
    }

    impl TdfSerializeOwned for u16 {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfSerialize for u16 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for u16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for i16 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u16::deserialize_owned(r)?;
            Ok(value as i16)
        }
    }

    // VarInt i16

    impl TdfSerializeOwned for i16 {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            (self as u16).serialize_owned(w);
        }
    }

    impl TdfSerialize for i16 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for i16 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for u32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u32, r)
        }
    }

    // VarInt u32

    impl TdfSerializeOwned for u32 {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            impl_serialize_int!(self, w)
        }
    }

    impl TdfSerialize for u32 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for u32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i32

    impl TdfDeserializeOwned for i32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u32::deserialize_owned(r)?;
            Ok(value as i32)
        }
    }

    impl TdfSerializeOwned for i32 {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            (self as u32).serialize_owned(w);
        }
    }

    impl TdfSerialize for i32 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for i32 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt u64

    impl TdfDeserializeOwned for u64 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(u64, r)
        }
    }

    impl TdfSerializeOwned for u64 {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            impl_serialize_int!(self, w);
        }
    }

    impl TdfSerialize for u64 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for u64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt i64

    impl TdfDeserializeOwned for i64 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = u64::deserialize_owned(r)?;
            Ok(value as i64)
        }
    }

    impl TdfSerializeOwned for i64 {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            (self as u64).serialize_owned(w);
        }
    }

    impl TdfSerialize for i64 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for i64 {
        const TYPE: TdfType = TdfType::VarInt;
    }

    // VarInt usize

    impl TdfDeserializeOwned for usize {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            impl_deserialize_int!(usize, r)
        }
    }

    impl TdfSerializeOwned for usize {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            impl_serialize_int!(self, w);
        }
    }

    impl TdfSerialize for usize {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for usize {
        const TYPE: TdfType = TdfType::VarInt;
    }

    impl TdfDeserializeOwned for isize {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let value = usize::deserialize_owned(r)?;
            Ok(value as isize)
        }
    }

    // VarInt isize

    impl TdfSerializeOwned for isize {
        #[inline]
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            (self as usize).serialize_owned(w);
        }
    }

    impl TdfSerialize for isize {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for isize {
        const TYPE: TdfType = TdfType::VarInt;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            reader::TdfDeserializer,
            types::{TdfDeserializeOwned, TdfSerialize},
        };

        #[test]
        fn test_unsigned_encoding() {
            let mut w = Vec::new();

            let values: &[(u64, &[u8])] = &[
                (0, &[0]),
                (5, &[5]),
                (128, &[128, 2]),
                (256, &[128, 4]),
                (512, &[128, 8]),
                (321, &[129, 5]),
                (2048, &[128, 32]),
                (4096, &[128, 64]),
                (4632, &[152, 72]),
                (8192, &[128, 128, 1]),
                (16384, &[128, 128, 2]),
                (32768, &[128, 128, 4]),
                (65536, &[128, 128, 8]),
                (4294967296, &[128, 128, 128, 128, 32]),
                (
                    9223372036854776000,
                    &[128, 131, 128, 128, 128, 128, 128, 128, 128, 2],
                ),
            ];

            for (value, expected) in values {
                value.serialize(&mut w);
                assert_eq!(&w, expected);

                let mut r = TdfDeserializer::new(&w);
                let read_value = u64::deserialize_owned(&mut r).unwrap();
                assert_eq!(read_value, *value);

                w.clear();
            }
        }

        #[test]
        fn test_signed_encoding() {
            let mut w = Vec::new();

            let values: &[(i64, &[u8])] = &[
                (-0, &[0]),
                (-5, &[187, 255, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-128, &[128, 254, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-256, &[128, 252, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-512, &[128, 248, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-321, &[191, 250, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-2048, &[128, 224, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-4096, &[128, 192, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-4632, &[168, 183, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-8192, &[128, 128, 255, 255, 255, 255, 255, 255, 255, 3]),
                (-16384, &[128, 128, 254, 255, 255, 255, 255, 255, 255, 3]),
                (-32768, &[128, 128, 252, 255, 255, 255, 255, 255, 255, 3]),
                (-65536, &[128, 128, 248, 255, 255, 255, 255, 255, 255, 3]),
                (
                    -4294967296,
                    &[128, 128, 128, 128, 224, 255, 255, 255, 255, 3],
                ),
                (
                    -9223372036854775000,
                    &[168, 140, 128, 128, 128, 128, 128, 128, 128, 2],
                ),
            ];

            for (value, expected) in values {
                value.serialize(&mut w);
                assert_eq!(&w, expected);

                let mut r = TdfDeserializer::new(&w);
                let read_value = i64::deserialize_owned(&mut r).unwrap();
                assert_eq!(read_value, *value);

                w.clear();
            }
        }
    }
}

pub mod string {
    //! String and string type related implementations and helper
    //! functions

    use super::Blob;
    use super::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };
    use std::borrow::Cow;

    // str slice types

    /// Special functions for writing an empty string value
    pub fn write_empty_str<S: TdfSerializer>(w: &mut S) {
        w.write_slice(&[1, 0]);
    }

    impl<'de> TdfDeserialize<'de> for &'de str {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let bytes: &'de [u8] = Blob::deserialize_raw(r)?;
            let text: &'de str =
                std::str::from_utf8(bytes).map_err(DecodeError::InvalidUtf8Value)?;

            if text.is_empty() {
                return Err(DecodeError::Other("String value has zero length"));
            }

            Ok(&text[..text.len() - 1])
        }
    }

    impl TdfSerialize for &str {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            let bytes = self.as_bytes();
            let needs_terminator = !matches!(bytes.last(), Some(0));
            let mut length = bytes.len();
            if needs_terminator {
                length += 1;
            }

            length.serialize_owned(w);
            w.write_slice(bytes);

            if needs_terminator {
                w.write_byte(0);
            }
        }
    }

    impl TdfTyped for &str {
        const TYPE: TdfType = TdfType::String;
    }

    // Owned String types

    impl TdfDeserializeOwned for String {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let bytes: &[u8] = Blob::deserialize_raw(r)?;
            let text: Cow<str> = String::from_utf8_lossy(bytes);
            let mut text: String = text.to_string();
            // Remove null terminator
            text.pop();
            Ok(text)
        }
    }

    impl TdfSerialize for String {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            self.as_str().serialize(w);
        }
    }

    impl TdfTyped for String {
        const TYPE: TdfType = TdfType::String;
    }

    impl<'de> TdfDeserialize<'de> for Cow<'de, str> {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let value: &'de str = <&str>::deserialize(r)?;
            Ok(Cow::Borrowed(value))
        }
    }

    impl TdfSerialize for Cow<'_, str> {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            match self {
                Cow::Borrowed(value) => value.serialize(w),
                Cow::Owned(value) => value.serialize(w),
            }
        }
    }

    impl TdfTyped for Cow<'_, str> {
        const TYPE: TdfType = TdfType::String;
    }

    #[cfg(test)]
    mod test {
        use crate::{serialize_vec, Blob, TdfDeserialize, TdfDeserializer, TdfSerialize};

        /// Tests that strings are encoded correctly and
        /// zero termined. Only tests [&str] as the other
        /// string types defer to it
        #[test]
        fn test_str_serialize() {
            let values = &[
                ("My example string", "My example string\0"),
                ("Test", "Test\0"),
                ("", "\0"),
                ("A", "A\0"),
            ];

            let mut w = Vec::new();

            for (value, expected) in values {
                value.serialize(&mut w);

                let expected_len = value.len() + 1; /* Length includes terminator */
                let length_bytes = serialize_vec(&expected_len);

                // Serialized content should start with the string length
                assert!(w.starts_with(&length_bytes));
                // Should end with a null terminator
                assert!(w.ends_with(&[0]));

                let mut r = TdfDeserializer::new(&w);
                let str_bytes = Blob::deserialize_raw(&mut r).unwrap();

                assert_eq!(expected.as_bytes(), str_bytes);

                // Reset buffer
                w.clear();
            }
        }

        /// Tests that strings are deserialized correctly ensuring
        /// the null terminator is removed
        #[test]
        fn test_str_deserialize() {
            let values = &["My example string", "Test", "", "A"];

            let mut w = Vec::new();

            for value in values {
                value.serialize(&mut w);

                let mut r = TdfDeserializer::new(&w);
                let str = <&str>::deserialize(&mut r).unwrap();
                assert_eq!(str, *value);

                r.cursor = 0;

                let str = String::deserialize(&mut r).unwrap();
                assert_eq!(str.as_str(), *value);

                // Reset buffer
                w.clear();
            }
        }
    }
}

pub mod blob {
    //! Blob and blob type related implementations and helper functions

    use super::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
    };

    /// [Blob] is a structure representing a variable length chunk
    /// of bytes with the length deliminated by a variable-length integer
    /// followed by the chunk of bytes
    ///
    /// The [String] structure uses the same underlying logic but wraps
    /// extra utf8 parsing on top
    #[derive(Default, Debug, Clone, PartialEq, Eq)]
    pub struct Blob<'de>(pub &'de [u8]);

    impl AsRef<[u8]> for Blob<'_> {
        fn as_ref(&self) -> &[u8] {
            self.0
        }
    }

    impl Blob<'_> {
        /// Serializes a blob value from its raw slice component
        pub fn serialize_raw<S: TdfSerializer>(w: &mut S, slice: &[u8]) {
            slice.len().serialize_owned(w);
            w.write_slice(slice);
        }

        /// Deserializes a blob directly as a slice
        pub fn deserialize_raw<'de>(r: &mut TdfDeserializer<'de>) -> DecodeResult<&'de [u8]> {
            let length = usize::deserialize_owned(r)?;
            let bytes = r.read_bytes(length)?;
            Ok(bytes)
        }

        /// Skips a blob type
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            let length: usize = usize::deserialize_owned(r)?;
            r.skip_length(length)
        }
    }

    impl<'de> TdfDeserialize<'de> for Blob<'de> {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let bytes = Self::deserialize_raw(r)?;
            Ok(Blob(bytes))
        }
    }

    impl TdfSerialize for Blob<'_> {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            Self::serialize_raw(w, self.0)
        }
    }

    impl TdfTyped for Blob<'_> {
        const TYPE: TdfType = TdfType::Blob;
    }

    /// [OwnedBlob] is an alternative to [Blob] which created a copy
    /// of the underlying bytes removing the associated lifetime
    pub struct OwnedBlob(pub Vec<u8>);

    impl TdfDeserializeOwned for OwnedBlob {
        fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
            let blob = Blob::deserialize_raw(r)?;
            Ok(Self(blob.to_vec()))
        }
    }

    impl TdfSerialize for OwnedBlob {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            Blob::serialize_raw(w, &self.0);
        }
    }

    impl TdfTyped for OwnedBlob {
        const TYPE: TdfType = TdfType::Blob;
    }
}

pub mod list {
    //! List type related implementations and helper functions

    use super::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// Skips a list while deserializing
    pub fn skip_list(r: &mut TdfDeserializer) -> DecodeResult<()> {
        let ty: TdfType = TdfType::deserialize_owned(r)?;
        let length: usize = usize::deserialize_owned(r)?;
        for _ in 0..length {
            ty.skip(r)?;
        }
        Ok(())
    }

    /// Serializes the header portion of a list, which contains the
    /// type and length of the list
    pub fn serialize_list_header<S: TdfSerializer>(w: &mut S, ty: TdfType, length: usize) {
        #[cfg(feature = "heat-compat")]
        {
            let ty = ty.heat_compat_list_type();
            ty.serialize_owned(w);
        }

        #[cfg(not(feature = "heat-compat"))]
        {
            ty.serialize_owned(w);
        }

        length.serialize_owned(w);
    }

    impl<'de, C> TdfDeserialize<'de> for Vec<C>
    where
        C: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let value_type = TdfType::deserialize_owned(r)?;

            if value_type != C::TYPE {
                return Err(DecodeError::InvalidType {
                    expected: C::TYPE,
                    actual: value_type,
                });
            }

            let length = usize::deserialize_owned(r)?;
            let mut values = Vec::with_capacity(length);

            for _ in 0..length {
                values.push(C::deserialize(r)?);
            }
            Ok(values)
        }
    }

    impl<V> TdfSerialize for Vec<V>
    where
        V: TdfSerialize + TdfTyped,
    {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            self.as_slice().serialize(w)
        }
    }

    impl<V> TdfTyped for Vec<V> {
        const TYPE: TdfType = TdfType::List;
    }

    impl<V> TdfSerialize for &[V]
    where
        V: TdfSerialize + TdfTyped,
    {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            serialize_list_header(w, V::TYPE, self.len());
            self.iter().for_each(|value| value.serialize(w));
        }
    }

    impl<C> TdfTyped for &[C]
    where
        C: TdfSerialize + TdfTyped,
    {
        const TYPE: TdfType = TdfType::List;
    }
}

pub mod map {
    //! Map type related implementations and helper functions

    use super::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped};
    use std::{
        borrow::Borrow,
        fmt::Debug,
        ops::{Bound, Index, IndexMut, RangeBounds},
    };

    use crate::{
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
        tag::TdfType,
        writer::TdfSerializer,
    };

    /// [TdfMap] is the data structure used for serializing maps in the
    /// Tdf format. This map implementation uses Rusts nightly
    /// [SortedMap](https://doc.rust-lang.org/stable/nightly-rustc/rustc_data_structures/sorted_map/struct.SortedMap.html)
    #[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct TdfMap<K, V> {
        data: Vec<(K, V)>,
    }

    impl<K, V> Default for TdfMap<K, V> {
        #[inline]
        fn default() -> TdfMap<K, V> {
            TdfMap { data: Vec::new() }
        }
    }

    impl<K, V> TdfMap<K, V> {
        /// Creates a new [TdfMap]
        #[inline]
        pub const fn new() -> TdfMap<K, V> {
            TdfMap { data: Vec::new() }
        }

        /// Creates a new [TdfMap] with a specific initial capacity
        #[inline]
        pub fn with_capacity(cap: usize) -> TdfMap<K, V> {
            TdfMap {
                data: Vec::with_capacity(cap),
            }
        }

        /// Returns the underlying vec of data
        pub fn into_inner(self) -> Vec<(K, V)> {
            self.data
        }
    }

    impl<K: Ord, V> TdfMap<K, V> {
        /// Construct a `SortedMap` from a presorted set of elements. This is faster
        /// than creating an empty map and then inserting the elements individually.
        ///
        /// It is up to the caller to make sure that the elements are sorted by key
        /// and that there are no duplicates.
        #[inline]
        pub fn from_presorted_elements(elements: Vec<(K, V)>) -> TdfMap<K, V> {
            TdfMap { data: elements }
        }

        /// Inserts a key and value into the map
        #[inline]
        pub fn insert(&mut self, key: K, mut value: V) -> Option<V> {
            match self.lookup_index_for(&key) {
                Ok(index) => {
                    let slot = unsafe { self.data.get_unchecked_mut(index) };
                    std::mem::swap(&mut slot.1, &mut value);
                    Some(value)
                }
                Err(index) => {
                    self.data.insert(index, (key, value));
                    None
                }
            }
        }

        /// Removes a key from the map
        #[inline]
        pub fn remove(&mut self, key: &K) -> Option<V> {
            match self.lookup_index_for(key) {
                Ok(index) => Some(self.data.remove(index).1),
                Err(_) => None,
            }
        }

        /// Gets a reference to a specific value from the map using its key
        #[inline]
        pub fn get<Q>(&self, key: &Q) -> Option<&V>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            match self.lookup_index_for(key) {
                Ok(index) => unsafe { Some(&self.data.get_unchecked(index).1) },
                Err(_) => None,
            }
        }

        /// Gets a mutable reference to a specific value from the map using its key
        #[inline]
        pub fn get_mut<Q>(&mut self, key: &Q) -> Option<&mut V>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            match self.lookup_index_for(key) {
                Ok(index) => unsafe { Some(&mut self.data.get_unchecked_mut(index).1) },
                Err(_) => None,
            }
        }

        /// Gets a mutable reference to the value in the entry, or insert a new one.
        #[inline]
        pub fn get_mut_or_insert_default(&mut self, key: K) -> &mut V
        where
            K: Eq,
            V: Default,
        {
            let index = match self.lookup_index_for(&key) {
                Ok(index) => index,
                Err(index) => {
                    self.data.insert(index, (key, V::default()));
                    index
                }
            };
            unsafe { &mut self.data.get_unchecked_mut(index).1 }
        }

        /// Clears the underlying structure
        #[inline]
        pub fn clear(&mut self) {
            self.data.clear();
        }

        /// Iterate over elements, sorted by key
        #[inline]
        pub fn iter(&self) -> std::slice::Iter<'_, (K, V)> {
            self.data.iter()
        }

        /// Iterate over the keys, sorted
        #[inline]
        pub fn keys(&self) -> impl Iterator<Item = &K> + ExactSizeIterator + DoubleEndedIterator {
            self.data.iter().map(|(k, _)| k)
        }

        /// Iterate over values, sorted by key
        #[inline]
        pub fn values(&self) -> impl Iterator<Item = &V> + ExactSizeIterator + DoubleEndedIterator {
            self.data.iter().map(|(_, v)| v)
        }

        /// Returns the length of the map
        #[inline]
        pub fn len(&self) -> usize {
            self.data.len()
        }

        /// Returns whether the map is empty
        #[inline]
        pub fn is_empty(&self) -> bool {
            self.len() == 0
        }

        /// Returns a range of key value pairs in the map
        #[inline]
        pub fn range<R>(&self, range: R) -> &[(K, V)]
        where
            R: RangeBounds<K>,
        {
            let (start, end) = self.range_slice_indices(range);
            &self.data[start..end]
        }

        /// Removes a range of key value pairs in the map
        #[inline]
        pub fn remove_range<R>(&mut self, range: R)
        where
            R: RangeBounds<K>,
        {
            let (start, end) = self.range_slice_indices(range);
            self.data.splice(start..end, std::iter::empty());
        }

        /// Mutate all keys with the given function `f`. This mutation must not
        /// change the sort-order of keys.
        #[inline]
        pub fn offset_keys<F>(&mut self, f: F)
        where
            F: Fn(&mut K),
        {
            self.data.iter_mut().map(|(k, _)| k).for_each(f);
        }

        /// Inserts a presorted range of elements into the map. If the range can be
        /// inserted as a whole in between to existing elements of the map, this
        /// will be faster than inserting the elements individually.
        ///
        /// It is up to the caller to make sure that the elements are sorted by key
        /// and that there are no duplicates.
        #[inline]
        pub fn insert_presorted(&mut self, elements: Vec<(K, V)>) {
            if elements.is_empty() {
                return;
            }

            let start_index = self.lookup_index_for(&elements[0].0);

            let elements = match start_index {
                Ok(index) => {
                    let mut elements = elements.into_iter();
                    self.data[index] = elements.next().unwrap();
                    elements
                }
                Err(index) => {
                    if index == self.data.len() || elements.last().unwrap().0 < self.data[index].0 {
                        // We can copy the whole range without having to mix with
                        // existing elements.
                        self.data.splice(index..index, elements);
                        return;
                    }

                    let mut elements = elements.into_iter();
                    self.data.insert(index, elements.next().unwrap());
                    elements
                }
            };

            // Insert the rest
            for (k, v) in elements {
                self.insert(k, v);
            }
        }

        /// Looks up the key in `self.data` via `slice::binary_search()`.
        #[inline(always)]
        fn lookup_index_for<Q>(&self, key: &Q) -> Result<usize, usize>
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.data.binary_search_by(|(x, _)| x.borrow().cmp(key))
        }

        #[inline]
        fn range_slice_indices<R>(&self, range: R) -> (usize, usize)
        where
            R: RangeBounds<K>,
        {
            let start = match range.start_bound() {
                Bound::Included(k) => match self.lookup_index_for(k) {
                    Ok(index) | Err(index) => index,
                },
                Bound::Excluded(k) => match self.lookup_index_for(k) {
                    Ok(index) => index + 1,
                    Err(index) => index,
                },
                Bound::Unbounded => 0,
            };

            let end = match range.end_bound() {
                Bound::Included(k) => match self.lookup_index_for(k) {
                    Ok(index) => index + 1,
                    Err(index) => index,
                },
                Bound::Excluded(k) => match self.lookup_index_for(k) {
                    Ok(index) | Err(index) => index,
                },
                Bound::Unbounded => self.data.len(),
            };

            (start, end)
        }

        /// Checks if the map contains a specific key
        #[inline]
        pub fn contains_key<Q>(&self, key: &Q) -> bool
        where
            K: Borrow<Q>,
            Q: Ord + ?Sized,
        {
            self.get(key).is_some()
        }
    }

    impl<K: Ord, V> IntoIterator for TdfMap<K, V> {
        type Item = (K, V);
        type IntoIter = std::vec::IntoIter<(K, V)>;

        fn into_iter(self) -> Self::IntoIter {
            self.data.into_iter()
        }
    }

    impl<'a, K, Q, V> Index<&'a Q> for TdfMap<K, V>
    where
        K: Ord + Borrow<Q>,
        Q: Ord + ?Sized,
    {
        type Output = V;

        fn index(&self, key: &Q) -> &Self::Output {
            self.get(key).expect("no entry found for key")
        }
    }

    impl<'a, K, Q, V> IndexMut<&'a Q> for TdfMap<K, V>
    where
        K: Ord + Borrow<Q>,
        Q: Ord + ?Sized,
    {
        fn index_mut(&mut self, key: &Q) -> &mut Self::Output {
            self.get_mut(key).expect("no entry found for key")
        }
    }

    impl<K: Ord, V> FromIterator<(K, V)> for TdfMap<K, V> {
        fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
            let mut data: Vec<(K, V)> = iter.into_iter().collect();

            data.sort_unstable_by(|(k1, _), (k2, _)| k1.cmp(k2));
            data.dedup_by(|(k1, _), (k2, _)| k1 == k2);

            TdfMap { data }
        }
    }

    impl<K: Debug, V: Debug> Debug for TdfMap<K, V> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            f.debug_map()
                .entries(self.data.iter().map(|(a, b)| (a, b)))
                .finish()
        }
    }

    /// Helper function for deserializing the header portion of the
    /// map returning the map key value types and length
    pub fn deserialize_map_header(
        r: &mut TdfDeserializer,
    ) -> DecodeResult<(TdfType, TdfType, usize)> {
        let key_type: TdfType = TdfType::deserialize_owned(r)?;
        let value_type: TdfType = TdfType::deserialize_owned(r)?;
        let length = usize::deserialize_owned(r)?;
        Ok((key_type, value_type, length))
    }

    /// Helper function for serializing a map header from the
    /// raw key type, value type and length
    pub fn serialize_map_header<S: TdfSerializer>(
        w: &mut S,
        key_type: TdfType,
        value_type: TdfType,
        length: usize,
    ) {
        key_type.serialize_owned(w);
        value_type.serialize_owned(w);
        length.serialize_owned(w);
    }

    /// Skips a map type when deserializing
    pub fn skip_map(r: &mut TdfDeserializer) -> DecodeResult<()> {
        let (key_ty, value_ty, length) = deserialize_map_header(r)?;
        for _ in 0..length {
            key_ty.skip(r)?;
            value_ty.skip(r)?;
        }
        Ok(())
    }

    impl<'de, K, V> TdfDeserialize<'de> for TdfMap<K, V>
    where
        K: TdfDeserialize<'de> + TdfTyped + Ord,
        V: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let (key_ty, value_ty, length) = deserialize_map_header(r)?;

            if key_ty != K::TYPE {
                return Err(DecodeError::InvalidType {
                    expected: K::TYPE,
                    actual: key_ty,
                });
            }
            if value_ty != V::TYPE {
                return Err(DecodeError::InvalidType {
                    expected: V::TYPE,
                    actual: value_ty,
                });
            }

            let mut map: TdfMap<K, V> = TdfMap::with_capacity(length);
            for _ in 0..length {
                let key: K = K::deserialize(r)?;
                let value: V = V::deserialize(r)?;
                map.insert(key, value);
            }
            Ok(map)
        }
    }

    impl<K, V> TdfSerialize for TdfMap<K, V>
    where
        K: TdfSerialize + TdfTyped + Ord,
        V: TdfSerialize + TdfTyped,
    {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            serialize_map_header(w, K::TYPE, V::TYPE, self.len());

            self.data.iter().for_each(|(key, value)| {
                key.serialize(w);
                value.serialize(w);
            });
        }
    }

    impl<K, V> TdfTyped for TdfMap<K, V> {
        const TYPE: TdfType = TdfType::Map;
    }

    #[cfg(test)]
    mod test {
        use crate::TdfMap;

        #[test]
        fn test_insert_and_iter() {
            let mut map: TdfMap<i32, i32> = TdfMap::new();
            let mut expected = Vec::new();

            for x in 0..100 {
                assert_eq!(map.iter().cloned().collect::<Vec<_>>(), expected);

                let x = 1000 - x * 2;
                map.insert(x, x);
                expected.insert(0, (x, x));
            }
        }

        #[test]
        fn test_get_and_index() {
            let mut map: TdfMap<i32, i32> = TdfMap::new();
            let mut expected = Vec::new();

            for x in 0..100 {
                let x = 1000 - x;
                if x & 1 == 0 {
                    map.insert(x, x);
                }
                expected.push(x);
            }

            for mut x in expected {
                if x & 1 == 0 {
                    assert_eq!(map.get(&x), Some(&x));
                    assert_eq!(map.get_mut(&x), Some(&mut x));
                    assert_eq!(map[&x], x);
                    assert_eq!(&mut map[&x], &mut x);
                } else {
                    assert_eq!(map.get(&x), None);
                    assert_eq!(map.get_mut(&x), None);
                }
            }
        }

        #[test]
        fn test_range() {
            let mut map = TdfMap::new();
            map.insert(1, 1);
            map.insert(3, 3);
            map.insert(6, 6);
            map.insert(9, 9);

            let keys = |s: &[(_, _)]| s.iter().map(|e| e.0).collect::<Vec<u32>>();

            for start in 0..11 {
                for end in 0..11 {
                    if end < start {
                        continue;
                    }

                    let mut expected = vec![1, 3, 6, 9];
                    expected.retain(|&x| x >= start && x < end);

                    assert_eq!(
                        keys(map.range(start..end)),
                        expected,
                        "range = {}..{}",
                        start,
                        end
                    );
                }
            }
        }

        #[test]
        fn test_offset_keys() {
            let mut map = TdfMap::new();
            map.insert(1, 1);
            map.insert(3, 3);
            map.insert(6, 6);

            map.offset_keys(|k| *k += 1);

            let mut expected = TdfMap::new();
            expected.insert(2, 1);
            expected.insert(4, 3);
            expected.insert(7, 6);

            assert_eq!(map, expected);
        }

        fn keys(s: TdfMap<u32, u32>) -> Vec<u32> {
            s.into_iter().map(|(k, _)| k).collect::<Vec<u32>>()
        }

        fn elements(s: TdfMap<u32, u32>) -> Vec<(u32, u32)> {
            s.into_iter().collect::<Vec<(u32, u32)>>()
        }

        #[test]
        fn test_remove_range() {
            let mut map = TdfMap::new();
            map.insert(1, 1);
            map.insert(3, 3);
            map.insert(6, 6);
            map.insert(9, 9);

            for start in 0..11 {
                for end in 0..11 {
                    if end < start {
                        continue;
                    }

                    let mut expected = vec![1, 3, 6, 9];
                    expected.retain(|&x| x < start || x >= end);

                    let mut map = map.clone();
                    map.remove_range(start..end);

                    assert_eq!(keys(map), expected, "range = {}..{}", start, end);
                }
            }
        }

        #[test]
        fn test_remove() {
            let mut map = TdfMap::new();
            let mut expected = Vec::new();

            for x in 0..10 {
                map.insert(x, x);
                expected.push((x, x));
            }

            for x in 0..10 {
                let mut map = map.clone();
                let mut expected = expected.clone();

                assert_eq!(map.remove(&x), Some(x));
                expected.remove(x as usize);

                assert_eq!(map.iter().cloned().collect::<Vec<_>>(), expected);
            }
        }

        #[test]
        fn test_insert_presorted_non_overlapping() {
            let mut map = TdfMap::new();
            map.insert(2, 0);
            map.insert(8, 0);

            map.insert_presorted(vec![(3, 0), (7, 0)]);

            let expected = vec![2, 3, 7, 8];
            assert_eq!(keys(map), expected);
        }

        #[test]
        fn test_insert_presorted_first_elem_equal() {
            let mut map = TdfMap::new();
            map.insert(2, 2);
            map.insert(8, 8);

            map.insert_presorted(vec![(2, 0), (7, 7)]);

            let expected = vec![(2, 0), (7, 7), (8, 8)];
            assert_eq!(elements(map), expected);
        }

        #[test]
        fn test_insert_presorted_last_elem_equal() {
            let mut map = TdfMap::new();
            map.insert(2, 2);
            map.insert(8, 8);

            map.insert_presorted(vec![(3, 3), (8, 0)]);

            let expected = vec![(2, 2), (3, 3), (8, 0)];
            assert_eq!(elements(map), expected);
        }

        #[test]
        fn test_insert_presorted_shuffle() {
            let mut map = TdfMap::new();
            map.insert(2, 2);
            map.insert(7, 7);

            map.insert_presorted(vec![(1, 1), (3, 3), (8, 8)]);

            let expected = vec![(1, 1), (2, 2), (3, 3), (7, 7), (8, 8)];
            assert_eq!(elements(map), expected);
        }

        #[test]
        fn test_insert_presorted_at_end() {
            let mut map = TdfMap::new();
            map.insert(1, 1);
            map.insert(2, 2);

            map.insert_presorted(vec![(3, 3), (8, 8)]);

            let expected = vec![(1, 1), (2, 2), (3, 3), (8, 8)];
            assert_eq!(elements(map), expected);
        }
    }
}

pub mod tagged_union {
    //! Tagged union type related implementations and helper functions

    use super::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfTyped};
    use crate::{
        error::{DecodeError, DecodeResult},
        reader::TdfDeserializer,
        tag::{Tag, Tagged, TdfType},
        writer::TdfSerializer,
    };

    /// Representation of a tagged union
    #[derive(Debug, PartialEq, Eq)]
    pub enum TaggedUnion<V> {
        /// Set variant of a union value
        Set {
            /// The descriminant for the enum type
            key: u8,
            /// The tag for the associated value
            tag: Tag,
            /// The associated value
            value: V,
        },
        /// Unset variant of a union value
        Unset,
    }

    /// Key used by tagged unions that have no set value
    pub const TAGGED_UNSET_KEY: u8 = 0x7F;

    impl<V> TaggedUnion<V> {
        /// Checks if the union is of set type
        pub fn is_set(&self) -> bool {
            matches!(self, Self::Set { .. })
        }

        /// Checks if the union is of unset type
        pub fn is_unset(&self) -> bool {
            matches!(self, Self::Unset)
        }

        /// Unwraps the underlying value stored in this tagged
        /// union. Will panic if the tagged union is unset
        pub fn unwrap(self) -> V {
            match self {
                Self::Unset => panic!("Attempted to unwrap unset union"),
                Self::Set { value, .. } => value,
            }
        }
    }

    impl<V> From<TaggedUnion<V>> for Option<V> {
        fn from(value: TaggedUnion<V>) -> Self {
            match value {
                TaggedUnion::Set { value, .. } => Some(value),
                TaggedUnion::Unset => None,
            }
        }
    }

    impl<V> TdfTyped for TaggedUnion<V> {
        const TYPE: TdfType = TdfType::TaggedUnion;
    }

    /// Skips the next tagged unoion while deserializing
    pub fn skip_tagged_union(r: &mut TdfDeserializer) -> DecodeResult<()> {
        let ty = r.read_byte()?;
        if ty != TAGGED_UNSET_KEY {
            Tagged::skip(r)?;
        }
        Ok(())
    }

    impl<'de, V> TdfDeserialize<'de> for TaggedUnion<V>
    where
        V: TdfDeserialize<'de> + TdfTyped,
    {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            let key = r.read_byte()?;
            if key == TAGGED_UNSET_KEY {
                return Ok(TaggedUnion::Unset);
            }
            let tag = Tagged::deserialize_owned(r)?;
            let expected_type = V::TYPE;
            let actual_type = tag.ty;
            if actual_type != expected_type {
                return Err(DecodeError::InvalidType {
                    expected: expected_type,
                    actual: actual_type,
                });
            }
            let value = V::deserialize(r)?;

            Ok(TaggedUnion::Set {
                key,
                tag: tag.tag,
                value,
            })
        }
    }

    impl<V> TdfSerialize for TaggedUnion<V>
    where
        V: TdfSerialize + TdfTyped,
    {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            match self {
                TaggedUnion::Set { key, tag, value } => {
                    w.write_byte(*key);
                    Tagged::serialize_raw(w, &tag.0, V::TYPE);
                    value.serialize(w);
                }
                TaggedUnion::Unset => w.write_byte(TAGGED_UNSET_KEY),
            }
        }
    }
}

pub mod var_int_list {
    //! Variable-length integer list type related implementations and helper functions

    use super::{TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
    };

    use super::var_int::skip_var_int;

    /// Wrapper type for a list of variable-length integers.
    /// Represented using a Vec of u64 values
    #[derive(Debug, PartialEq, Eq, Default, Clone)]
    pub struct VarIntList(pub Vec<u64>);

    impl VarIntList {
        /// Creates a new VarIntList
        pub const fn new() -> Self {
            Self(Vec::new())
        }

        /// Consumes self returning the underlying
        /// Vec storing the variable-length integer values
        pub fn into_inner(self) -> Vec<u64> {
            self.0
        }

        /// Skips the next var int list when deserializing
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            let length: usize = usize::deserialize_owned(r)?;
            for _ in 0..length {
                skip_var_int(r)?;
            }
            Ok(())
        }
    }

    impl From<Vec<u64>> for VarIntList {
        fn from(value: Vec<u64>) -> Self {
            Self(value)
        }
    }

    impl AsRef<[u64]> for VarIntList {
        fn as_ref(&self) -> &[u64] {
            self.0.as_ref()
        }
    }

    impl TdfDeserializeOwned for VarIntList {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let length = usize::deserialize_owned(r)?;
            let mut out = Vec::with_capacity(length);
            for _ in 0..length {
                out.push(u64::deserialize_owned(r)?);
            }
            Ok(VarIntList(out))
        }
    }

    impl TdfSerialize for VarIntList {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            self.0.len().serialize_owned(w);
            self.0
                .iter()
                .copied()
                .for_each(|value| value.serialize_owned(w));
        }
    }

    impl TdfTyped for VarIntList {
        const TYPE: TdfType = TdfType::VarIntList;
    }
}

pub mod object_type {
    //! ObjectType type related implementations and helper functions

    use super::{
        var_int::skip_var_int, TdfDeserializeOwned, TdfSerialize, TdfSerializeOwned, TdfTyped,
    };
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
    };

    /// [ObjectType] structure represents a type of object
    /// within the blaze system, this type consists of a
    /// component value and a type value
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct ObjectType {
        /// The component type for this object
        pub component: u16,
        /// The actual object type
        pub ty: u16,
    }

    impl ObjectType {
        /// Creates a new [ObjectType] where the values are all zero
        #[inline]
        pub const fn zero() -> Self {
            Self::new(0, 0)
        }

        /// Create a new [ObjectType] from its component and type
        pub const fn new(component: u16, ty: u16) -> Self {
            Self { component, ty }
        }

        /// Skip sthe next ObjectType when deserializing
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            skip_var_int(r)?;
            skip_var_int(r)
        }
    }

    impl TdfDeserializeOwned for ObjectType {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let component = u16::deserialize_owned(r)?;
            let ty = u16::deserialize_owned(r)?;
            Ok(Self { component, ty })
        }
    }

    impl TdfSerialize for ObjectType {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            self.component.serialize_owned(w);
            self.ty.serialize_owned(w);
        }
    }

    impl TdfTyped for ObjectType {
        const TYPE: TdfType = TdfType::ObjectType;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            reader::TdfDeserializer,
            types::{TdfDeserialize, TdfSerialize},
        };

        use super::ObjectType;

        /// Tests that object types can be correctly serialized
        #[test]
        fn test_encode_object_id() {
            let mut w = Vec::new();
            let object_type = ObjectType {
                component: 30722,
                ty: 1,
            };
            object_type.serialize(&mut w);

            // Check deserialize works correctly
            let mut r = TdfDeserializer::new(&w);
            let value = ObjectType::deserialize(&mut r).unwrap();
            assert_eq!(object_type, value);

            // Check serialized bytes match expected
            let expected = &[
                130, 224, 3, // Component
                1, // Type
            ];

            assert_eq!(&r.buffer, expected)
        }
    }
}

pub mod object_id {
    //! ObjectId type related implementations and helper functions

    use super::{
        object_type::ObjectType, var_int::skip_var_int, TdfDeserializeOwned, TdfSerialize,
        TdfSerializeOwned, TdfTyped,
    };
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
    };

    /// [ObjectId] structure represents an ID within the blaze system that
    /// is also tied to a specific [ObjectType]
    #[derive(Debug, PartialEq, Eq, Clone)]
    pub struct ObjectId {
        /// The type of object the ID represents
        pub ty: ObjectType,
        /// The actual ID of the object
        pub id: u64,
    }

    impl ObjectId {
        /// Creates a new [ObjectId] where the values are all zero
        #[inline]
        pub const fn zero() -> Self {
            Self::new(ObjectType::zero(), 0)
        }

        /// Create a new [ObjectId] from its type and id
        pub const fn new(ty: ObjectType, id: u64) -> Self {
            Self { ty, id }
        }

        /// Create a new [ObjectId] from its component, type, and id
        pub const fn new_raw(component: u16, ty: u16, id: u64) -> Self {
            Self {
                ty: ObjectType { component, ty },
                id,
            }
        }

        /// Skips an ObjectId when deserializing
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            ObjectType::skip(r)?;
            skip_var_int(r)
        }
    }

    impl TdfDeserializeOwned for ObjectId {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let ty = ObjectType::deserialize_owned(r)?;
            let id = u64::deserialize_owned(r)?;
            Ok(Self { ty, id })
        }
    }

    impl TdfSerialize for ObjectId {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            self.ty.serialize(w);
            self.id.serialize_owned(w);
        }
    }

    impl TdfTyped for ObjectId {
        const TYPE: TdfType = TdfType::ObjectId;
    }

    #[cfg(test)]
    mod test {
        use crate::{
            reader::TdfDeserializer,
            types::object_type::ObjectType,
            types::{TdfDeserialize, TdfSerialize},
        };

        use super::ObjectId;

        /// Tests that object IDs can be correctly serialized
        #[test]
        fn test_encode_object_id() {
            let mut w = Vec::new();
            let object_id = ObjectId {
                ty: ObjectType {
                    component: 30722,
                    ty: 1,
                },
                id: 0x0000012,
            };
            object_id.serialize(&mut w);

            // Check deserialize works correctly
            let mut r = TdfDeserializer::new(&w);
            let value = ObjectId::deserialize(&mut r).unwrap();
            assert_eq!(object_id.ty, value.ty);
            assert_eq!(object_id.id, value.id);

            // Check serialized bytes match expected
            let expected = &[
                130, 224, 3,  // Component
                1,  // Type
                18, // ID
            ];

            assert_eq!(&r.buffer, expected)
        }
    }
}

pub mod float {
    //! float type related implementations and helper functions

    use super::{TdfDeserializeOwned, TdfSerializeOwned, TdfTyped};
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
        TdfSerialize,
    };

    /// Skips the 4 bytes required for a 32 bit float value
    #[inline]
    pub fn skip_f32(r: &mut TdfDeserializer) -> DecodeResult<()> {
        r.skip_length(4)
    }

    impl TdfDeserializeOwned for f32 {
        fn deserialize_owned(r: &mut TdfDeserializer) -> DecodeResult<Self> {
            let bytes: [u8; 4] = r.read_fixed()?;
            Ok(f32::from_be_bytes(bytes))
        }
    }

    impl TdfSerializeOwned for f32 {
        fn serialize_owned<S: TdfSerializer>(self, w: &mut S) {
            let bytes: [u8; 4] = self.to_be_bytes();
            w.write_slice(&bytes);
        }
    }

    impl TdfSerialize for f32 {
        #[inline]
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            (*self).serialize_owned(w)
        }
    }

    impl TdfTyped for f32 {
        const TYPE: TdfType = TdfType::Float;
    }

    #[cfg(test)]
    mod test {
        use crate::{reader::TdfDeserializer, types::TdfDeserializeOwned, types::TdfSerialize};

        /// Tests f32 encoding and decoding
        #[test]
        fn test_float_encoding() {
            let data: &[(f32, [u8; 4])] = &[
                (123.0, [66, 246, 0, 0]),
                (254.0, [67, 126, 0, 0]),
                (1.0, [63, 128, 0, 0]),
                (-3.0, [192, 64, 0, 0]),
            ];

            let mut w = Vec::new();
            for (value, expected) in data {
                // Check serialized buffer matches expected bytes
                value.serialize(&mut w);
                assert_eq!(&w, expected);

                // Check that the deserialize works correctly
                let mut r = TdfDeserializer::new(&w);
                let read_value = f32::deserialize_owned(&mut r).unwrap();
                assert_eq!(read_value, *value);

                // Reset writer for next iteration
                w.clear();
            }
        }
    }
}

pub mod u12 {
    //! U12 type related implementations and helper functions

    use super::{Blob, TdfDeserialize, TdfSerialize, TdfTyped};
    use crate::{
        error::DecodeResult, reader::TdfDeserializer, tag::TdfType, writer::TdfSerializer,
        GroupSlice, TdfDeserializeOwned,
    };

    /// [U12] The type/name for this structure is not yet known
    /// but is represented using 8 bytes of data and a string value
    #[derive(Debug)]
    pub struct U12 {
        /// The leading byte value (Encoding not yet known)
        pub data: [u8; 8],
        /// Associated string value
        pub value: String,
    }

    impl U12 {
        /// Skips a U12 value while deserializing
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            r.skip_length(8)?;
            Blob::skip(r)?;
            GroupSlice::deserialize_group_end(r)?;
            Ok(())
        }
    }

    impl TdfDeserializeOwned for U12 {
        fn deserialize_owned(r: &mut TdfDeserializer<'_>) -> DecodeResult<Self> {
            let data: [u8; 8] = r.read_fixed()?;
            let value: String = String::deserialize(r)?;
            GroupSlice::deserialize_group_end(r)?;
            Ok(Self { data, value })
        }
    }

    impl TdfSerialize for U12 {
        fn serialize<S: TdfSerializer>(&self, w: &mut S) {
            w.write_slice(&self.data);
            self.value.serialize(w);
            w.write_byte(0);
        }
    }

    impl TdfTyped for U12 {
        const TYPE: TdfType = TdfType::Generic;
    }
}

pub mod group {
    //! Group type related implementations and helper functions

    use super::TdfDeserialize;
    use crate::{error::DecodeResult, reader::TdfDeserializer, tag::Tagged};

    /// [GroupSlice] is a slice of bytes representing a group tdf that hasn't
    /// been decoded into its inner tags
    ///
    /// The structure also provides functions that are useful for decoding group types:
    ///
    /// * [GroupSlice::deserialize_prefix_two] - Attempt to deserialize the list prefix
    /// * [GroupSlice::deserialize_group_end] - Attempt to deserialize the end of list
    /// * [GroupSlice::deserialize_content] - Attempt to deserialize the content using an action
    /// * [GroupSlice::deserialize_content_skip] - Attempt to deserialize the content skipping all values
    pub struct GroupSlice<'de> {
        /// The encoded byte contents of the group
        pub data: &'de [u8],
    }

    impl GroupSlice<'_> {
        /// This is a product of the heat bug, this value is not actualy apart
        /// of structs but is handled here as it has been encounted in the HNET
        /// list where a union is miss encoded as a struct
        pub fn deserialize_prefix_two(r: &mut TdfDeserializer) -> DecodeResult<()> {
            let is_two = r.read_byte()? == 2;
            if !is_two {
                r.step_back();
            }
            Ok(())
        }

        /// Attempts to read a byte if the byte value is zero the group is
        /// considered ended otherwise the cursor is stepped back and reading
        /// should continue as normal
        pub fn deserialize_group_end(r: &mut TdfDeserializer) -> DecodeResult<bool> {
            let is_end = r.read_byte()? == 0;
            if !is_end {
                r.step_back();
            }
            Ok(is_end)
        }

        /// Deserializes all the items in the slice using the provided decoding
        /// action function
        #[inline]
        pub fn deserialize_content<'de, A>(
            r: &mut TdfDeserializer<'de>,
            mut action: A,
        ) -> DecodeResult<&'de [u8]>
        where
            A: FnMut(&mut TdfDeserializer<'de>) -> DecodeResult<()>,
        {
            let start = r.cursor;
            loop {
                let is_end = Self::deserialize_group_end(r)?;
                if is_end {
                    break;
                }

                // Call the decoding action on the type
                action(r)?;
            }
            let end = r.cursor - 1;
            let data = &r.buffer[start..end];
            Ok(data)
        }

        /// Skips the remaining contents of a group until the condition
        /// within [GroupSlice::deserialize_group_end] is met and the group
        /// is considered finished
        #[inline]
        pub fn deserialize_content_skip<'de>(
            r: &mut TdfDeserializer<'de>,
        ) -> DecodeResult<&'de [u8]> {
            Self::deserialize_content(r, Tagged::skip)
        }

        /// Skips a Group type while deserializing
        pub fn skip(r: &mut TdfDeserializer) -> DecodeResult<()> {
            #[cfg(feature = "heat-compat")]
            {
                Self::deserialize_prefix_two(r)?;
            }
            Self::deserialize_content_skip(r)?;
            Ok(())
        }
    }

    impl<'de> TdfDeserialize<'de> for GroupSlice<'de> {
        fn deserialize(r: &mut TdfDeserializer<'de>) -> DecodeResult<Self> {
            #[cfg(feature = "heat-compat")]
            {
                Self::deserialize_prefix_two(r)?;
            }
            let data = Self::deserialize_content_skip(r)?;
            Ok(Self { data })
        }
    }
}

pub mod extra {
    //! Extra typing for builtin types required by upstream crates

    use std::{borrow::Cow, rc::Rc, sync::Arc};

    use crate::{TdfDeserialize, TdfDeserializeOwned, TdfSerialize, TdfType, TdfTyped};

    /// Unit type can be serialized as nothing
    impl TdfSerialize for () {
        fn serialize<S: crate::TdfSerializer>(&self, _: &mut S) {}
    }

    /// Unit type can be deserialized from nothing
    impl TdfDeserializeOwned for () {
        fn deserialize_owned(_: &mut crate::TdfDeserializer<'_>) -> crate::DecodeResult<Self> {
            Ok(())
        }
    }

    /// [Result] types can serializes from either branch as long
    /// as both branches implement [TdfSerialize]
    impl<T, E> TdfSerialize for Result<T, E>
    where
        T: TdfSerialize,
        E: TdfSerialize,
    {
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            match self {
                Ok(value) => value.serialize(w),
                Err(value) => value.serialize(w),
            }
        }
    }

    /// [Option] types can be serialized as something or as nothing
    /// depdning on the value.
    impl<T> TdfSerialize for Option<T>
    where
        T: TdfSerialize,
    {
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            if let Some(value) = self {
                value.serialize(w);
            }
        }
    }

    impl<T> TdfSerialize for &T
    where
        T: TdfSerialize,
    {
        #[inline]
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            TdfSerialize::serialize(*self, w);
        }
    }

    impl<T> TdfTyped for &T
    where
        T: TdfTyped,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfSerialize for &mut T
    where
        T: TdfSerialize,
    {
        #[inline]
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            TdfSerialize::serialize(*self, w);
        }
    }

    impl<T> TdfTyped for &mut T
    where
        T: TdfTyped,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfSerialize for Box<T>
    where
        T: TdfSerialize,
    {
        #[inline]
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            TdfSerialize::serialize(self.as_ref(), w);
        }
    }

    impl<T> TdfTyped for Box<T>
    where
        T: TdfTyped,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfSerialize for Rc<T>
    where
        T: TdfSerialize,
    {
        #[inline]
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            TdfSerialize::serialize(self.as_ref(), w);
        }
    }

    impl<T> TdfTyped for Rc<T>
    where
        T: TdfTyped,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfSerialize for Arc<T>
    where
        T: TdfSerialize,
    {
        #[inline]
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            TdfSerialize::serialize(self.as_ref(), w);
        }
    }

    impl<T> TdfTyped for Arc<T>
    where
        T: TdfTyped,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfSerialize for Cow<'_, T>
    where
        T: TdfSerialize + ToOwned,
        T::Owned: TdfSerialize,
    {
        fn serialize<S: crate::TdfSerializer>(&self, w: &mut S) {
            match self {
                Cow::Borrowed(value) => (**value).serialize(w),
                Cow::Owned(value) => (*value).serialize(w),
            };
        }
    }

    impl<T> TdfTyped for Cow<'_, T>
    where
        T: TdfTyped + ToOwned,
    {
        const TYPE: TdfType = T::TYPE;
    }

    impl<T> TdfDeserializeOwned for Box<T>
    where
        T: for<'de> TdfDeserialize<'de>,
    {
        #[inline]
        fn deserialize_owned(r: &mut crate::TdfDeserializer<'_>) -> crate::DecodeResult<Self> {
            T::deserialize(r).map(Box::new)
        }
    }

    impl<T> TdfDeserializeOwned for Rc<T>
    where
        T: for<'de> TdfDeserialize<'de>,
    {
        #[inline]
        fn deserialize_owned(r: &mut crate::TdfDeserializer<'_>) -> crate::DecodeResult<Self> {
            T::deserialize(r).map(Rc::new)
        }
    }

    impl<T> TdfDeserializeOwned for Arc<T>
    where
        T: for<'de> TdfDeserialize<'de>,
    {
        #[inline]
        fn deserialize_owned(r: &mut crate::TdfDeserializer<'_>) -> crate::DecodeResult<Self> {
            T::deserialize(r).map(Arc::new)
        }
    }

    impl<'de, T> TdfDeserialize<'de> for Cow<'de, T>
    where
        T: ToOwned,
        T::Owned: TdfDeserialize<'de>,
    {
        fn deserialize(r: &mut crate::TdfDeserializer<'de>) -> crate::DecodeResult<Self> {
            <T::Owned as TdfDeserialize<'de>>::deserialize(r).map(Cow::Owned)
        }
    }
}
