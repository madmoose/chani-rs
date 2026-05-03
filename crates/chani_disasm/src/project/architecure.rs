use std::str::FromStr;

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Architecture {
    _8086,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InvalidArchitecture;

impl FromStr for Architecture {
    type Err = InvalidArchitecture;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "8086" => Ok(Architecture::_8086),
            _ => Err(InvalidArchitecture),
        }
    }
}

impl ToString for Architecture {
    fn to_string(&self) -> String {
        match self {
            Architecture::_8086 => "8086",
        }
        .to_string()
    }
}
