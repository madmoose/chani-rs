use std::ops::{Div, Mul};

#[derive(Debug, Clone, Copy)]
pub struct Clock(pub f64);

impl Mul<u32> for Clock {
    type Output = Clock;

    fn mul(self, rhs: u32) -> Self::Output {
        Clock(self.0 * rhs as f64)
    }
}

impl Div<u32> for Clock {
    type Output = Clock;

    fn div(self, rhs: u32) -> Self::Output {
        Clock(self.0 / rhs as f64)
    }
}

pub struct Multiplier(u32, u32);

/// Represents a duration in microseconds
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub struct Microseconds(pub f64);

/// Represents a duration in attoseconds (10^-18 seconds)
///
/// Range: 0 to 340,282,366,920,938,463,463,374,607,431,768,211,455 attoseconds
/// (approximately 0 to 10.79 billion years)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Attoseconds(pub u128);

impl Attoseconds {
    /// Create Attoseconds from microseconds
    pub fn from_microseconds(us: f64) -> Self {
        // 1 microsecond = 10^12 attoseconds
        Attoseconds((us * 1_000_000_000_000.0) as u128)
    }

    /// Convert to microseconds
    pub fn to_microseconds(self) -> f64 {
        self.0 as f64 / 1_000_000_000_000.0
    }
}
