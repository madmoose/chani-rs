pub trait BitOps: Sized {
    fn msb(self) -> bool;
    fn lsb(self) -> bool;
    fn rol(self, count: u8, cf: bool) -> (Self, bool);
    fn ror(self, count: u8, cf: bool) -> (Self, bool);
    fn rcl(self, count: u8, cf: bool) -> (Self, bool);
    fn rcr(self, count: u8, cf: bool) -> (Self, bool);
    fn shl(self, count: u8, cf: bool) -> (Self, bool);
    fn shr(self, count: u8, cf: bool) -> (Self, bool);
    fn setmo(self, count: u8, cf: bool) -> (Self, bool);
    fn sar(self, count: u8, cf: bool) -> (Self, bool);
}

macro_rules! impl_bitops {
    ($prim:ty) => {
        impl BitOps for $prim {
            fn msb(self) -> bool {
                (self >> (<$prim>::BITS - 1)) & 1 != 0
            }

            fn lsb(self) -> bool {
                self & 1 != 0
            }

            fn rol(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    cf = self.msb();
                    self = (self << 1) | (cf as $prim);
                }
                (self, cf)
            }

            fn ror(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    cf = (self & 1) != 0;
                    self = ((cf as $prim) << (<$prim>::BITS - 1)) | (self >> 1);
                }
                (self, cf)
            }

            fn rcl(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    let msb = self.msb();
                    self = (self << 1) | (cf as $prim);
                    cf = msb;
                }
                (self, cf)
            }

            fn rcr(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    let lsb = self.lsb();
                    self = ((cf as $prim) << (<$prim>::BITS - 1)) | (self >> 1);
                    cf = lsb;
                }
                (self, cf)
            }

            fn shl(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    cf = self.msb();
                    self <<= 1;
                }
                (self, cf)
            }

            fn shr(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    cf = self.lsb();
                    self >>= 1;
                }
                (self, cf)
            }

            fn setmo(self, _count: u8, _cf: bool) -> (Self, bool) {
                (Self::MAX, false)
            }

            fn sar(mut self, count: u8, mut cf: bool) -> (Self, bool) {
                for _ in 0..count {
                    cf = self.lsb();
                    let msb = self.msb();
                    self = ((msb as $prim) << (<$prim>::BITS - 1)) | (self >> 1);
                }
                (self, cf)
            }
        }
    };
}

impl_bitops!(u8);
impl_bitops!(u16);
