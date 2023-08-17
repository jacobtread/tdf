//! Logic for converting tdf's into human readable string
//! formats

use crate::{
    error::DecodeResult,
    reader::TdfReader,
    tag::{Tagged, TdfType},
    types::{ObjectId, ObjectType, UNION_UNSET},
};
use std::fmt::write;

use super::Decodable;

pub struct StringifyReader<'a> {
    pub reader: TdfReader<'a>,
    pub out: String,
}

impl StringifyReader<'_> {
    fn write_indent(&mut self, indent: usize) {
        self.out.reserve(indent * 2);
        for _ in 0..indent {
            self.out.push_str("  ");
        }
    }

    pub fn new(reader: TdfReader<'_>) -> StringifyReader<'_> {
        StringifyReader {
            reader,
            out: String::new(),
        }
    }

    pub fn stringify(mut self) -> String {
        while self.reader.cursor < self.reader.buffer.len() {
            if let Err(err) = self.next_tag(1) {
                let remaining = &self.reader.buffer[self.reader.cursor..];
                self.out.push_str(&format!(
                    "... cause: {:?}, remaining {} {:?}",
                    err,
                    remaining.len(),
                    remaining,
                ));
                break;
            }
        }
        self.out
    }

    fn next_tag(&mut self, indent: usize) -> DecodeResult<()> {
        let tagged = Tagged::decode(&mut self.reader)?;

        self.write_indent(indent);

        let name = tagged.tag.to_string();

        self.out.push('"');
        self.out.push_str(&name);
        self.out.push_str("\": ");

        self.next_type(indent, &tagged.ty)?;

        self.out.push_str(",\n");
        Ok(())
    }

    fn next_type(&mut self, indent: usize, ty: &TdfType) -> DecodeResult<()> {
        match ty {
            TdfType::VarInt => self.next_var_int(),
            TdfType::String => self.next_string(),
            TdfType::Blob => self.next_blob(),
            TdfType::Group => self.next_group(indent),
            TdfType::List => self.next_list(indent),
            TdfType::Map => self.next_map(indent),
            TdfType::TaggedUnion => self.next_tagged_union(indent),
            TdfType::VarIntList => self.next_var_int_list(),
            TdfType::ObjectType => self.next_object_type(),
            TdfType::ObjectId => self.next_object_id(),
            TdfType::Float => self.next_float(),
            TdfType::U12 => self.next_u12(),
        }
    }

    fn next_var_int(&mut self) -> DecodeResult<()> {
        let value = self.reader.read_u64()?;
        self.out.push_str(&value.to_string());
        Ok(())
    }

    fn next_string(&mut self) -> DecodeResult<()> {
        let value = self.reader.read_string()?;
        self.out.push('"');
        self.out.push_str(&value);
        self.out.push('"');
        Ok(())
    }

    fn next_blob(&mut self) -> DecodeResult<()> {
        let value = self.reader.read_blob()?;
        let length = value.len();
        self.out.push_str("Blob [");
        for (i, value) in value.iter().enumerate() {
            self.out.push_str(&format!("0x{:X}", value));
            if i < length - 1 {
                self.out.push_str(", ");
            }
        }
        self.out.push(']');
        Ok(())
    }

    fn next_group(&mut self, indent: usize) -> DecodeResult<()> {
        self.out.push_str("{\n");
        let mut is_two: bool = false;
        while self.reader.cursor < self.reader.buffer.len() {
            let byte: u8 = self.reader.buffer[self.reader.cursor];
            if byte == 0 {
                self.reader.cursor += 1;
                break;
            }
            if byte == 2 {
                is_two = true;
                self.reader.cursor += 1;
            }
            self.next_tag(indent + 1)?;
        }
        self.out.push_str(&"  ".repeat(indent));
        self.out.push('}');
        if is_two {
            self.out.push_str(" (2)");
        }
        Ok(())
    }

    fn next_list(&mut self, indent: usize) -> DecodeResult<()> {
        let value_type: TdfType = self.reader.read_type()?;
        let length: usize = self.reader.read_usize()?;
        let expand = matches!(value_type, TdfType::Map | TdfType::Group);
        self.out.push('[');
        if expand {
            self.out.push('\n');
        }

        for i in 0..length {
            if expand {
                self.out.push_str(&"  ".repeat(indent + 1));
            }
            self.next_type(indent + 1, &value_type)?;
            if i < length - 1 {
                self.out.push_str(", ");
            }
            if expand {
                self.out.push('\n');
            }
        }
        if expand {
            self.out.push_str(&"  ".repeat(indent));
        }
        self.out.push(']');
        Ok(())
    }

    fn next_map(&mut self, indent: usize) -> DecodeResult<()> {
        let key_type: TdfType = self.reader.read_type()?;
        let value_type: TdfType = self.reader.read_type()?;
        let length: usize = self.reader.read_usize()?;
        self.out.push_str(&format!(
            "Map<{:?}, {:?}, {}>",
            key_type, value_type, length
        ));
        self.out.push_str("{\n");

        let start = self.reader.cursor;

        let mut proc = || -> DecodeResult<()> {
            for i in 0..length {
                self.out.push_str(&"  ".repeat(indent + 1));
                self.next_type(indent + 1, &key_type)?;
                self.out.push_str(": ");
                self.next_type(indent + 1, &value_type)?;
                if i < length - 1 {
                    self.out.push(',');
                }
                self.out.push('\n')
            }
            Ok(())
        };

        if let Err(err) = proc() {
            self.out.push_str(&format!("Err: {}", err));
            self.out
                .push_str(&format!("Full Bytes: {:?}", &self.reader.buffer[start..]));
        }

        self.out.push_str(&"  ".repeat(indent));
        self.out.push('}');
        Ok(())
    }

    fn next_tagged_union(&mut self, indent: usize) -> DecodeResult<()> {
        let ty = self.reader.read_byte()?;
        if ty == UNION_UNSET {
            self.out.push_str("Union(Unset)")
        } else {
            let tagged = Tagged::decode(&mut self.reader)?;
            self.out
                .push_str(&format!("Union(\"{}\", {}, ", &tagged.tag, ty));
            self.next_type(indent + 1, &tagged.ty)?;
            self.out.push(')')
        }
        Ok(())
    }

    fn next_var_int_list(&mut self) -> DecodeResult<()> {
        let length: usize = self.reader.read_usize()?;
        self.out.push_str("VarList [");
        for i in 0..length {
            self.next_var_int()?;
            if i < length - 1 {
                self.out.push_str(", ");
            }
        }
        self.out.push(']');
        Ok(())
    }

    fn next_object_id(&mut self) -> DecodeResult<()> {
        let id = ObjectId::decode(&mut self.reader)?;
        self.out.push_str(&id.to_string());
        Ok(())
    }

    fn next_object_type(&mut self) -> DecodeResult<()> {
        let ty = ObjectType::decode(&mut self.reader)?;
        self.out.push_str(&ty.to_string());
        Ok(())
    }

    fn next_float(&mut self) -> DecodeResult<()> {
        let value = self.reader.read_f32()?;
        self.out.push_str(&value.to_string());
        Ok(())
    }

    fn next_u12(&mut self) -> DecodeResult<()> {
        let bytes = self.reader.read_slice(8)?;
        self.out.push_str(&format!("{:?} + ", bytes));

        self.next_string()?;
        Ok(())
    }
}
