use std::{error::Error, fmt::Display};

/// Errors that can occur while operating on Tdf structures
#[derive(Debug)]
pub enum TdfError {
    /// Error where a type of Tdf is not known
    UnknownType {
        /// The unknown type provided
        ty: u8,
    },
}

impl TdfError {
    // TODO: With context impl
    // pub fn with_context(self, input:)  -> DecodeError<'
}

/// Error with additional buffer and cursor related
/// context information
#[derive(Debug)]
pub struct DecodeError<'buffer> {
    /// The error that occurred
    pub error: TdfError,
    /// Cursor position before the error occurred
    pub previous_cursor: usize,
    /// Cursor position at time of error
    pub cursor: usize,
    /// Buffer being operated on
    pub buffer: &'buffer [u8],
}

impl Error for TdfError {}

impl Display for TdfError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!("Not implemented");
    }
}
