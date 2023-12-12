//! Provides an implementation for re-processing serialized tdf bytes into a human-readable form
//! see [TdfStringifier]

use crate::{
    error::DecodeError,
    reader::TdfDeserializer,
    tag::{Tagged, TdfType},
    types::{
        group::GroupSlice, map::deserialize_map_header, tagged_union::TAGGED_UNSET_KEY, Blob,
        ObjectId, ObjectType, TdfDeserialize, TdfDeserializeOwned, U12,
    },
};
use std::fmt::Display;

/// Wrapper around a [TdfDeserializer] that deserializes the values into
/// a human readable string format writing it onto the associated writer
pub struct TdfStringifier<'de, W> {
    /// The deserializer to read from
    pub r: TdfDeserializer<'de>,
    /// The writer to write the human readable format to
    pub w: W,
}

/// Errors that can occur while stringifying
#[derive(Debug)]
pub enum StringifyError {
    /// Formatting error
    Format(std::fmt::Error),
    /// Decoding error
    Decode(DecodeError),
}

impl Display for StringifyError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StringifyError::Format(err) => err.fmt(f),
            StringifyError::Decode(err) => err.fmt(f),
        }
    }
}

impl From<std::fmt::Error> for StringifyError {
    fn from(value: std::fmt::Error) -> Self {
        Self::Format(value)
    }
}

impl From<DecodeError> for StringifyError {
    fn from(value: DecodeError) -> Self {
        Self::Decode(value)
    }
}

type StringifyResult = Result<(), StringifyError>;

impl<'de, W> TdfStringifier<'de, W>
where
    W: std::fmt::Write,
{
    /// Creates a new [TdfStringifier] from the provided reader and writer
    pub fn new(r: TdfDeserializer<'de>, w: W) -> TdfStringifier<'de, W> {
        Self { r, w }
    }

    /// Creates a new [TdfStringifier] from the provided reader that writes
    /// to a string
    pub fn new_string(r: TdfDeserializer<'de>) -> (String, bool) {
        let mut out = String::new();
        let mut this = TdfStringifier { r, w: &mut out };
        let success = this.stringify();
        (out, success)
    }

    /// Stringifies the contents of the reader writing the output to the writer
    /// returns a bool indicating whether the deserialization failed
    pub fn stringify(&mut self) -> bool {
        if self.w.write_str("{\n").is_err() {
            return false;
        }

        while !self.r.is_empty() {
            if let Err(err) = self.stringify_tag(1) {
                let remaining = &self.r.buffer[self.r.cursor..];
                let _  = write!(
                    &mut self.w,
                    "... (ERR) cause: {}, (cursor: {}) (remaining: {} byte(s)) (remaining dump: {:?})",
                    err,
                    self.r.cursor,
                    remaining.len(),
                    remaining
                );
                return false;
            }
        }

        _ = self.w.write_char('}');

        true
    }

    fn write_indent(&mut self, indent: usize) -> StringifyResult {
        for _ in 0..indent {
            self.w.write_str("  ")?;
        }
        Ok(())
    }

    fn stringify_tag(&mut self, indent: usize) -> StringifyResult {
        let tag = Tagged::deserialize_owned(&mut self.r)?;
        self.write_indent(indent)?;
        write!(&mut self.w, "\"{}\": ", tag.tag)?;
        self.stringify_type(indent, &tag.ty, false)?;
        self.w.write_str(",\n")?;
        Ok(())
    }

    fn stringify_type(
        &mut self,
        indent: usize,
        ty: &TdfType,
        heat_compat: bool,
    ) -> StringifyResult {
        match ty {
            TdfType::VarInt => self.stringify_var_int(),
            TdfType::String => self.stringify_string(),
            TdfType::Blob => self.stringify_blob(),
            TdfType::Group => self.stringify_group(indent, heat_compat),
            TdfType::List => self.stringify_list(indent),
            TdfType::Map => self.stringify_map(indent),
            TdfType::TaggedUnion => self.stringify_tagged_union(indent, heat_compat),
            TdfType::VarIntList => self.stringify_var_int_list(),
            TdfType::ObjectType => self.stringify_object_type(),
            TdfType::ObjectId => self.stringify_object_id(),
            TdfType::Float => self.stringify_f32(),
            TdfType::Generic => self.stringify_u12(),
        }
    }

    fn stringify_var_int(&mut self) -> StringifyResult {
        let value = u64::deserialize_owned(&mut self.r)?;
        write!(&mut self.w, "{}", value)?;
        Ok(())
    }

    fn stringify_string(&mut self) -> StringifyResult {
        let value = String::deserialize(&mut self.r)?;
        self.w.write_char('"')?;
        self.w.write_str(&value)?;
        self.w.write_char('"')?;
        Ok(())
    }

    fn stringify_blob(&mut self) -> StringifyResult {
        let value = Blob::deserialize_raw(&mut self.r)?;

        self.w.write_str("Blob([")?;

        let last_index = value.len().saturating_sub(1);
        for (index, value) in value.iter().enumerate() {
            write!(&mut self.w, "{:#X}", value)?;
            if index != last_index {
                self.w.write_str(", ")?;
            }
        }

        self.w.write_str("])")?;

        Ok(())
    }

    /// Attempts to validate that a group can be properly decoded from the message
    fn try_validate_group(&mut self) -> bool {
        // Store the cursor for restoring
        let cursor = self.r.cursor;

        let mut valid = false;

        // Attempt to deserialize the group
        if GroupSlice::deserialize_content_skip(&mut self.r).is_ok() {
            valid = true;
        }

        // Restore cursor
        self.r.cursor = cursor;

        valid
    }

    fn stringify_group(&mut self, indent: usize, heat_compat: bool) -> StringifyResult {
        // See if the value can actually be deserialized as a group (It might actually be a heat bugged union)
        if heat_compat && !self.try_validate_group() {
            // TODO: Check other possible heat types

            // Try and serialize it as a union
            return self.stringify_tagged_union(indent, true);
        }

        self.w.write_str("{\n")?;

        loop {
            let is_end = GroupSlice::deserialize_group_end(&mut self.r)?;
            if is_end {
                break;
            }

            self.stringify_tag(indent + 1)?;
        }

        self.write_indent(indent)?;
        self.w.write_char('}')?;

        Ok(())
    }

    fn stringify_list(&mut self, indent: usize) -> StringifyResult {
        let value_type: TdfType = TdfType::deserialize_owned(&mut self.r)?;
        let length: usize = usize::deserialize_owned(&mut self.r)?;

        // Map and group types should be in a expanded format
        let is_expanded = matches!(value_type, TdfType::Map | TdfType::Group);

        self.w.write_char('[')?;
        if is_expanded {
            self.w.write_char('\n')?;
        }

        let next_ident = indent + 1;

        let last_index = length.saturating_sub(1);
        for i in 0..length {
            if is_expanded {
                self.write_indent(next_ident)?;
            }

            self.stringify_type(next_ident, &value_type, true)?;

            if i != last_index {
                self.w.write_str(", ")?;
            }

            if is_expanded {
                self.w.write_char('\n')?;
            }
        }

        if is_expanded {
            self.write_indent(indent)?;
        }

        self.w.write_char(']')?;
        Ok(())
    }

    fn stringify_map(&mut self, indent: usize) -> StringifyResult {
        let (key_ty, value_ty, length) = deserialize_map_header(&mut self.r)?;

        let start = self.r.cursor;
        let next_indent = indent + 1;
        let last_index = length.saturating_sub(1);
        self.w.write_str("{\n")?;

        for i in 0..length {
            // Handle error while reading map entry
            if let Err(err) = self.stringify_map_entry(next_indent, &key_ty, &value_ty) {
                write!(
                    &mut self.w,
                    "Err: {}, Inclusive Map Bytes: {:?}",
                    err,
                    &self.r.buffer[start..]
                )?;

                return Err(err);
            }

            if i != last_index {
                self.w.write_char(',')?;
            }

            self.w.write_char('\n')?;
        }

        self.write_indent(indent)?;
        self.w.write_char('}')?;
        Ok(())
    }

    fn stringify_map_entry(
        &mut self,
        indent: usize,
        key_ty: &TdfType,
        value_ty: &TdfType,
    ) -> StringifyResult {
        self.write_indent(indent)?;
        self.stringify_type(indent, key_ty, true)?;
        self.w.write_str(": ")?;
        self.stringify_type(indent, value_ty, true)?;
        Ok(())
    }

    fn stringify_tagged_union(&mut self, indent: usize, heat_compat: bool) -> StringifyResult {
        let key = self.r.read_byte()?;

        if key == TAGGED_UNSET_KEY {
            self.w.write_str("TaggedUnion(Unset)")?;
            return Ok(());
        }

        // Attempt deserialize normally
        if !heat_compat {
            let tag = Tagged::deserialize_owned(&mut self.r)?;
            write!(&mut self.w, "Union(\"{}\", {}, ", &tag.tag, key)?;
            self.stringify_type(indent + 1, &tag.ty, false)?;
            self.w.write_char(')')?;
        } else {
            // Heat compat assumes the union value is always a group
            write!(&mut self.w, "HeatUnion({}, ", key)?;
            self.stringify_group(indent + 1, false)?;
            self.w.write_char(')')?;
        }

        Ok(())
    }

    fn stringify_var_int_list(&mut self) -> StringifyResult {
        let length = usize::deserialize_owned(&mut self.r)?;
        self.w.write_str("VarIntList [")?;

        let last_index = length.saturating_sub(1);

        for i in 0..length {
            self.stringify_var_int()?;
            if i != last_index {
                self.w.write_str(", ")?;
            }
        }

        self.w.write_char(']')?;

        Ok(())
    }

    fn stringify_object_type(&mut self) -> StringifyResult {
        let value = ObjectType::deserialize_owned(&mut self.r)?;
        write!(&mut self.w, "{:?}", value)?;
        Ok(())
    }

    fn stringify_object_id(&mut self) -> StringifyResult {
        let value = ObjectId::deserialize_owned(&mut self.r)?;
        write!(&mut self.w, "{:?}", value)?;
        Ok(())
    }

    fn stringify_f32(&mut self) -> StringifyResult {
        let value = f32::deserialize_owned(&mut self.r)?;
        write!(&mut self.w, "{}", value)?;
        Ok(())
    }

    fn stringify_u12(&mut self) -> StringifyResult {
        let value = U12::deserialize(&mut self.r)?;
        write!(&mut self.w, "{:?}", value)?;
        Ok(())
    }
}
