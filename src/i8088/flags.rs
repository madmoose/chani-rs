/*
 * ######## ##          ###     ######    ######
 * ##       ##         ## ##   ##    ##  ##    ##
 * ##       ##        ##   ##  ##        ##
 * ######   ##       ##     ## ##   ####  ######
 * ##       ##       ######### ##    ##        ##
 * ##       ##       ##     ## ##    ##  ##    ##
 * ##       ######## ##     ##  ######    ######
 */

/*
 * Sign
 */

#[inline]
pub fn signbit8(v: u16) -> bool {
    (v >> 7) & 1 != 0
}

#[inline]
pub fn signbit16(v: u16) -> bool {
    v >> 15 != 0
}

/*
 * Carry
 */

#[inline]
pub fn cf8_add(_res: u16, dst: u16, src: u16, cf: bool) -> bool {
    let dst = dst as u8;
    let src = src as u8;
    dst.carrying_add(src, cf).1
}

#[inline]
pub fn cf16_add(_res: u16, dst: u16, src: u16, cf: bool) -> bool {
    dst.carrying_add(src, cf).1
}

#[inline]
pub fn _cfw_add(res: u16, dst: u16, src: u16, cf: bool, w: bool) -> bool {
    if !w {
        cf8_add(res, dst, src, cf)
    } else {
        cf16_add(res, dst, src, cf)
    }
}

#[inline]
pub fn cf8_sub(_res: u16, dst: u16, src: u16, cf: bool) -> bool {
    let dst = dst as u8;
    let src = src as u8;
    dst.borrowing_sub(src, cf).1
}

#[inline]
pub fn cf16_sub(_res: u16, dst: u16, src: u16, cf: bool) -> bool {
    dst.borrowing_sub(src, cf).1
}

#[inline]
pub fn _cfw_sub(res: u16, dst: u16, src: u16, cf: bool, w: bool) -> bool {
    if !w {
        cf8_sub(res, dst, src, cf)
    } else {
        cf16_sub(res, dst, src, cf)
    }
}

/*
 * Parity
 */

#[inline]
pub fn pf8(res: u16) -> bool {
    (0x9669 >> ((res ^ (res >> 4)) & 0xf)) & 1 != 0
}

#[inline]
pub fn pf16(res: u16) -> bool {
    pf8(res)
}

/*
 * Adjust
 */

#[inline]
pub fn af8(res: u16, dst: u16, src: u16) -> bool {
    ((res ^ src ^ dst) >> 4) & 1 != 0
}

#[inline]
pub fn af16(res: u16, dst: u16, src: u16) -> bool {
    af8(res, dst, src)
}

/*
 * Zero
 */

#[inline]
pub fn zf8(res: u16) -> bool {
    (res & 0x00ff) == 0
}

#[inline]
pub fn zf16(res: u16) -> bool {
    res == 0
}

#[inline]
pub fn _zfw(res: u16, w: bool) -> bool {
    if !w {
        zf8(res)
    } else {
        zf16(res)
    }
}

/*
 * Sign
 */

#[inline]
pub fn sf8(res: u16) -> bool {
    signbit8(res)
}

#[inline]
pub fn sf16(res: u16) -> bool {
    signbit16(res)
}

#[inline]
pub fn _sfw(res: u16, w: bool) -> bool {
    if !w {
        sf8(res)
    } else {
        sf16(res)
    }
}

/*
 * Overflow
 */

#[inline]
pub fn of8_add(res: u16, dst: u16, src: u16) -> bool {
    signbit8((res ^ src) & (res ^ dst))
}

#[inline]
pub fn of16_add(res: u16, dst: u16, src: u16) -> bool {
    signbit16((res ^ src) & (res ^ dst))
}

#[inline]
pub fn _ofw_add(res: u16, dst: u16, src: u16, w: bool) -> bool {
    if !w {
        of8_add(res, dst, src)
    } else {
        of16_add(res, dst, src)
    }
}

#[inline]
pub fn of8_sub(res: u16, dst: u16, src: u16) -> bool {
    signbit8((src ^ dst) & (res ^ dst))
}

#[inline]
pub fn of16_sub(res: u16, dst: u16, src: u16) -> bool {
    signbit16((src ^ dst) & (res ^ dst))
}

#[inline]
pub fn _ofw_sub(res: u16, dst: u16, src: u16, w: bool) -> bool {
    if !w {
        of8_sub(res, dst, src)
    } else {
        of16_sub(res, dst, src)
    }
}
