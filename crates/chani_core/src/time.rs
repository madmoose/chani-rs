//! Time utilities for the emulator.
//!
//! Provides types and functions for handling time in attoseconds (i128)
//! and frequencies in MHz (f64), suitable for high-precision emulation timing.

use core::ops::{Add, Sub};

/// Represents a time duration in attoseconds (1e-18 seconds).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct Attoseconds(pub i128);

impl Attoseconds {
    /// Creates a new Attoseconds value.
    pub fn new(attos: i128) -> Self {
        Self(attos)
    }

    // Creates a new Attoseconds value from an integer nanoseconds value.
    pub fn from_nanoseconds(nanos: i128) -> Self {
        // 1 nanosecond = 1e9 attoseconds
        Attoseconds(nanos * 1_000_000_000)
    }

    /// Returns the number of attoseconds.
    pub fn as_i128(&self) -> i128 {
        self.0
    }

    /// Converts attoseconds to seconds as f64.
    pub fn as_seconds(&self) -> f64 {
        self.0 as f64 * 1e-18
    }

    /// Converts attoseconds to nanoseconds as f64.
    pub fn as_nanoseconds(&self) -> f64 {
        self.0 as f64 * 1e-9
    }
}

impl Add for Attoseconds {
    type Output = Attoseconds;
    fn add(self, rhs: Attoseconds) -> Attoseconds {
        Attoseconds(
            self.0
                .checked_add(rhs.0)
                .expect("Attoseconds addition overflowed"),
        )
    }
}

impl Sub for Attoseconds {
    type Output = Attoseconds;
    fn sub(self, rhs: Attoseconds) -> Attoseconds {
        Attoseconds(
            self.0
                .checked_sub(rhs.0)
                .expect("Attoseconds subtraction underflowed"),
        )
    }
}

/// Represents a frequency in megahertz (MHz).
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Default)]
pub struct Megahertz(pub f64);

impl Megahertz {
    /// Creates a new Megahertz value.
    pub fn new(mhz: f64) -> Self {
        Self(mhz)
    }

    /// Returns the frequency in MHz.
    pub fn as_f64(&self) -> f64 {
        self.0
    }

    /// Returns the frequency in Hz.
    pub fn as_hz(&self) -> f64 {
        self.0 * 1_000_000.0
    }

    /// Returns the period of one cycle in attoseconds.
    pub fn period_attoseconds(&self) -> Attoseconds {
        if self.0 > 0.0 {
            let period = 1.0 / (self.0 * 1e6) * 1e18;
            Attoseconds(period as i128)
        } else {
            Attoseconds(i128::MAX)
        }
    }
}

/// Converts a number of cycles at a given frequency to attoseconds.
pub fn cycles_to_attoseconds(cycles: u64, freq: Megahertz) -> Attoseconds {
    if freq.0 > 0.0 {
        // cycles_to_attoseconds: cycles / (freq in Hz) * 1e18
        let attos = (cycles as f64) * 1e18 / (freq.0 * 1e6);
        Attoseconds(attos as i128)
    } else {
        Attoseconds(i128::MAX)
    }
}

/// Converts a duration in attoseconds to the number of cycles at a given frequency.
pub fn attoseconds_to_cycles(attos: Attoseconds, freq: Megahertz) -> u64 {
    if freq.0 > 0.0 {
        ((attos.0 as f64) * (freq.0 * 1e-12)) as u64
    } else {
        0
    }
}
