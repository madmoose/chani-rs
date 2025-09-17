#[derive(Debug)]
pub enum Error {
    InvalidFunction,
    FileNotFound,
    PathNotFound,
    TooManyOpenFiles,
    AccessDenied,
    InvalidHandle,
    ArenaTrashed,
    NotEnoughMemory,
    InvalidBlock,
    BadEnvironment,
    BadFormat,
    InvalidAccess,
    InvalidData,
    InvalidDrive,
    CurrentDirectory,
    NotSameDevice,
    NoMoreFiles,
}

impl Error {
    pub fn to_dos_error_code(&self) -> u16 {
        match self {
            Error::InvalidFunction => 1,
            Error::FileNotFound => 2,
            Error::PathNotFound => 3,
            Error::TooManyOpenFiles => 4,
            Error::AccessDenied => 5,
            Error::InvalidHandle => 6,
            Error::ArenaTrashed => 7,
            Error::NotEnoughMemory => 8,
            Error::InvalidBlock => 9,
            Error::BadEnvironment => 10,
            Error::BadFormat => 11,
            Error::InvalidAccess => 12,
            Error::InvalidData => 13,
            Error::InvalidDrive => 15,
            Error::CurrentDirectory => 16,
            Error::NotSameDevice => 17,
            Error::NoMoreFiles => 18,
        }
    }
}
