extern crate bmp;
extern crate log;

extern crate scitools;

use scitools::stream;
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use bmp::{Image, Pixel};
use log::{info, warn};
use std::env;

#[derive(Debug)]
pub enum PictureError {
    IoError(std::io::Error),
    InvalidPalette(usize),
    UnrecognizedOpcode(u8),
    UnrecognizedOpcodeX(u8),
    Unimplemented(PicOp),
    UnimplementedX(PicOpX),
}

impl From<std::io::Error> for PictureError {
    fn from(error: std::io::Error) -> Self {
       PictureError::IoError(error)
    }
}

const SCREEN_HEIGHT: u32 = 200;
const SCREEN_WIDTH: u32 = 320;

const DRAW_ENABLE_VISUAL: u32 = 1;
const DRAW_ENABLE_PRIORITY: u32 = 2;
const DRAW_ENABLE_CONTROL: u32 = 4;

const PATTERN_MASK_SIZE: u8 = 0x07;
const PATTERN_FLAG_RECTANGLE: u8 = 0x10;
const PATTERN_FLAG_USE_PATTERN: u8 = 0x20;

const EGA_PALETTE_COUNT: usize = 4;
const EGA_PALETTE_SIZE: usize = 40;

// http://www.shikadi.net/moddingwiki/EGA_Palette
fn ega_color_to_rgb(color: u8) -> Pixel {
    match color {
        0x00 => Pixel{ r: 0x00, g: 0x00, b: 0x00 },
        0x01 => Pixel{ r: 0x00, g: 0x00, b: 0xaa },
        0x02 => Pixel{ r: 0x00, g: 0xaa, b: 0x00 },
        0x03 => Pixel{ r: 0x00, g: 0xaa, b: 0xaa },
        0x04 => Pixel{ r: 0xaa, g: 0x00, b: 0x00 },
        0x05 => Pixel{ r: 0xaa, g: 0x00, b: 0xaa },
        0x06 => Pixel{ r: 0xaa, g: 0x50, b: 0x00 },
        0x07 => Pixel{ r: 0xaa, g: 0xaa, b: 0xaa },
        0x08 => Pixel{ r: 0x55, g: 0x55, b: 0x55 },
        0x09 => Pixel{ r: 0x55, g: 0x55, b: 0xff },
        0x0a => Pixel{ r: 0x55, g: 0xff, b: 0x55 },
        0x0b => Pixel{ r: 0x55, g: 0xff, b: 0xff },
        0x0c => Pixel{ r: 0xff, g: 0x55, b: 0x55 },
        0x0d => Pixel{ r: 0xff, g: 0x55, b: 0xff },
        0x0e => Pixel{ r: 0xff, g: 0xff, b: 0x55 },
        0x0f => Pixel{ r: 0xff, g: 0xff, b: 0xff },
        _ => Pixel{ r: 0xff, g: 0xc0, b: 0xcb }, // pink
    }
}

fn _get_drawing_mask(color: u8, prio: u8, control: u8) -> u32 {
    let mut flag = 0;
    if color != 255 {
        flag = flag | DRAW_ENABLE_VISUAL;
    }
    if prio != 255 {
        flag = flag | DRAW_ENABLE_PRIORITY;
    }
    if control != 255 {
        flag = flag | DRAW_ENABLE_CONTROL;
    }
    flag
}

//#[derive(Clone, Copy)]
//struct PicColor {
//    col0: u8,
//    col1: u8
//}

struct Coord {
    x: u32,
    y: u32
}

struct Rect {
    left: u32,
    top: u32,
    right: u32,
    bottom: u32
}

/*
const EGA_DEFAULT_PALETTE: [ PicColor; EGA_PALETTE_SIZE ] = [
    PicColor( 0,  0), PicColor( 1,  1), PicColor( 2,  2), PicColor( 3,  3),
    PicColor( 4,  4), PicColor( 5,  5), PicColor( 6,  6), PicColor( 7,  7),
    PicColor( 8,  8), PicColor( 9,  9), PicColor(10, 10), PicColor(11, 11),
    PicColor(12, 12), PicColor(13, 13), PicColor(14, 14), PicColor( 8,  8),
    PicColor( 8,  8), PicColor( 0,  1), PicColor( 0,  2), PicColor( 0,  3),
    PicColor( 0,  4), PicColor( 0,  5), PicColor( 0,  6), PicColor( 8,  8),
    PicColor( 8,  8), PicColor(15,  9), PicColor(15, 10), PicColor(15, 11),
    PicColor(15, 12), PicColor(15, 13), PicColor(15, 14), PicColor(15, 15),
    PicColor( 0,  8), PicColor( 9,  1), PicColor( 2, 10), PicColor( 3, 11),
    PicColor( 4, 12), PicColor( 5, 13), PicColor( 6, 14), PicColor( 8,  8),
];
*/

const PATTERN_CIRCLES: [ [ u8; 30 ]; 8 ] = [
    [ 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x72, 0x02, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0xce, 0xf7, 0x7d, 0x0e, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x1c, 0x3e, 0x7f, 0x7f, 0x7f, 0x3e, 0x1c, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x38, 0xf8, 0xf3, 0xdf, 0x7f, 0xff, 0xfd, 0xf7, 0x9f, 0x3f, 0x38, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0x70, 0xc0, 0x1f, 0xfe, 0xe3, 0x3f, 0xff, 0xf7, 0x7f, 0xff, 0xe7, 0x3f, 0xfe, 0xc3, 0x1f, 0xf8, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0xf0, 0x01, 0xff, 0xe1, 0xff, 0xf8, 0x3f, 0xff, 0xdf, 0xff, 0xf7, 0xff, 0xfd, 0x7f, 0xff, 0x9f, 0xff, 0xe3, 0xff, 0xf0, 0x1f, 0xf0, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00 ],
    [ 0xe0, 0x03, 0xf8, 0x0f, 0xfc, 0x1f, 0xfe, 0x3f, 0xfe, 0x3f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xff, 0x7f, 0xfe, 0x3f, 0xfe, 0x3f, 0xfc, 0x1f, 0xf8, 0x0f, 0xe0, 0x03 ]
];

#[derive(TryFromPrimitive)]
#[derive(Debug)]
#[repr(u8)]
pub enum PicOp {
    SetColor = 0xf0,
    DisableVisual= 0xf1,
    SetPriority = 0xf2,
    DisablePriority = 0xf3,
    RelativePatterns = 0xf4,
    RelativeMediumLines = 0xf5,
    RelativeLongLines = 0xf6,
    RelativeShortLines = 0xf7,
    Fill = 0xf8,
    SetPattern = 0xf9,
    AbsolutePatterns = 0xfa,
    SetControl  = 0xfb,
    DisableControl = 0xfc,
    RelativeMediumPatterns = 0xfd,
    X = 0xfe,
    End = 0xff,
}

#[derive(TryFromPrimitive)]
#[derive(Debug)]
#[repr(u8)]
pub enum PicOpX {
    SetPaletteEntry = 0x00,
    SetPalette = 0x01,
    Mono0 = 0x02,
    Mono1 = 0x03,
    Mono2 = 0x04,
    Mono3 = 0x05,
    Mono4 = 0x06,
    EmbeddedView = 0x07,
    SetPriorityTable = 0x08,
}

// https://internals.rust-lang.org/t/clamp-function-for-primitive-types/4999
fn clamp<T: std::cmp::PartialOrd>(val: T, min: T, max: T) -> T {
    if val < min {
        min
    } else if val > max {
        max
    } else {
        val
    }
}

fn get_abs_coords(res: &mut stream::Streamer) -> Coord {
    let coord_prefix = res.get_byte();
    let mut x = res.get_byte() as u32;
    let mut y = res.get_byte() as u32;
    //info!("get_abs_coords(): x {} y {} prefix {}", x, y, coord_prefix);
    x = x | ((coord_prefix & 0xf0) as u32) << 4;
    y = y | ((coord_prefix & 0x0f) as u32) << 8;
    Coord{ x, y }
}

fn get_rel_coords(res: &mut stream::Streamer, base: &Coord) -> Coord {
    let input = res.get_byte();
    let mut x = base.x;
    if (input & 0x80) != 0 {
        x -= ((input >> 4) & 7) as u32;
    } else {
        x += (input >> 4) as u32;
    }
    let mut y = base.y;
    if (input & 0x08) != 0 {
        y -= (input & 0x07) as u32;
    } else {
        y += (input & 0x07) as u32;
    }
    Coord{ x, y }
}

pub struct Picture {
    visual: bmp::Image,
    priority: bmp::Image,
    control: bmp::Image,
}

impl Picture {
    pub fn new() -> Picture {
        let visual = Image::new(SCREEN_WIDTH, SCREEN_HEIGHT);
        let priority = Image::new(SCREEN_WIDTH, SCREEN_HEIGHT);
        let control = Image::new(SCREEN_WIDTH, SCREEN_HEIGHT);
        Picture{ visual, priority, control }
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), PictureError> {
        let mut draw_enable = DRAW_ENABLE_VISUAL | DRAW_ENABLE_PRIORITY;
        let mut priority: u8 = 0;
        let mut control: u8 = 0;
        let mut col: u8 = 0;
        let mut pattern_nr: u8 = 0;
        let mut pattern_code: u8 = 0;

        let mut palette = [ [ 0 as u8; EGA_PALETTE_SIZE ]; EGA_PALETTE_COUNT];
        //for n in 0..EGA_PALETTE_COUNT {
        //    for m in 0..EGA_PALETTE_SIZE {
        //        palette[n][m] = EGA_DEFAULT_PALETTE[m];
        //    }
        //}

        let mut res = stream::Streamer::new(data, 0);
        while !res.end_of_stream() {
            let opcode = res.get_byte();
            match PicOp::try_from(opcode) {
                Ok(PicOp::SetColor) => {
                    let code = res.get_byte() as usize;
                    info!("SetColor({})", code);
                    col = palette[code / 40][code % 40];
                    draw_enable = draw_enable | DRAW_ENABLE_VISUAL;
                },
                Ok(PicOp::DisableVisual) => {
                    info!("DisableVisual");
                    draw_enable = draw_enable & !DRAW_ENABLE_VISUAL;
                },
                Ok(PicOp::SetPriority) => {
                    let code = res.get_byte();
                    info!("SetPriority({})", code);
                    priority = code & 0xf;
                    draw_enable = draw_enable | DRAW_ENABLE_PRIORITY;
                },
                Ok(PicOp::DisablePriority) => {
                    info!("DisablePriority()");
                    draw_enable = draw_enable & !DRAW_ENABLE_PRIORITY;
                },
                Ok(PicOp::RelativePatterns) => {
                    info!("RelativePatterns()");
                    if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                        let code = res.get_byte();
                        pattern_nr = (code >> 1) & 0x7f;
                    }

                    let mut coord = get_abs_coords(&mut res);
                    self.draw_pattern(&coord, draw_enable, col, priority, control, pattern_code, pattern_nr);

                    while res.peek_byte() < 0xf0 {
                        coord = get_rel_coords(&mut res, &coord);
                        self.draw_pattern(&coord, draw_enable, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::RelativeMediumLines) => {
                    info!("RelativeMediumLines()");
                    let mut old_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let code = res.get_byte();
                        let mut y = old_coord.y;
                        if (code & 0x80) != 0 {
                            y = y - (code & 0x7f) as u32;
                        } else {
                            y = y + code as u32;
                        }
                        let code = res.get_byte();
                        let mut x = old_coord.x;
                        if (code & 0x80) != 0 {
                            x -= 128 - (code & 0x7f) as u32;
                        } else {
                            x += code as u32;
                        }
                        let coord = Coord{ x, y };
                        self.dither_line(&old_coord, &coord, col, priority, control, draw_enable);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::RelativeLongLines) => {
                    info!("RelativeLongLines()");
                    let mut old_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        self.dither_line(&old_coord, &coord, col, priority, control, draw_enable);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::RelativeShortLines) => {
                    info!("RelativeShortLines()");
                    let mut old_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_rel_coords(&mut res, &old_coord);
                        self.dither_line(&old_coord, &coord, col, priority, control, draw_enable);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::Fill) => {
                    info!("Fill()");
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        self.dither_fill(&coord, col, priority, control, draw_enable);
                    }
                },
                Ok(PicOp::SetPattern) => {
                    let code = res.get_byte();
                    info!("SetPattern({})", code);
                    pattern_code = code;
                },
                Ok(PicOp::AbsolutePatterns) => {
                    while res.peek_byte() < 0xf0 {
                        if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                            let code = res.get_byte();
                            pattern_nr = (code >> 1) & 0x7f;
                        }
                        let coord = get_abs_coords(&mut res);
                        self.draw_pattern(&coord, draw_enable, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::SetControl) => {
                    let code = res.get_byte();
                    info!("SetControl({})", code);
                    control = code & 0xf;
                    draw_enable = draw_enable | DRAW_ENABLE_CONTROL;
                },
                Ok(PicOp::DisableControl) => {
                    draw_enable = draw_enable & !DRAW_ENABLE_CONTROL;
                },
                Ok(PicOp::RelativeMediumPatterns) => {
                    info!("RelativeMediumPatterns()");
                    if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                        let code = res.get_byte();
                        pattern_nr = (code >> 1) & 0x7f;
                    }
                    let mut coord = get_abs_coords(&mut res);

                    self.draw_pattern(&coord, draw_enable, col, priority, control, pattern_code, pattern_nr);
                    while res.peek_byte() < 0xf0 {
                        if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                            let code = res.get_byte();
                            pattern_nr = (code >> 1) & 0x7f;
                        }

                        let code = res.get_byte();
                        let mut y = coord.y;
                        if (code & 0x80) != 0 {
                            y = y - (code & 0x7f) as u32;
                        } else {
                            y = y + code as u32;
                        }
                        let x = coord.x + res.get_byte() as u32;

                        coord = Coord{ x, y };
                        self.draw_pattern(&coord, draw_enable, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::X) => {
                    let opcodex = res.get_byte();
                    match PicOpX::try_from(opcodex) {
                        //Ok(PicOpX::SetPaletteEntry) => { return Err(PictureError::PicOpX) },
                        Ok(PicOpX::SetPalette) => {
                            let palette_index = res.get_byte() as usize;
                            info!("PicOpX::SetPalette({})", palette_index);
                            if palette_index >= EGA_PALETTE_COUNT {
                                return Err(PictureError::InvalidPalette(palette_index))
                            }
                            for n in 0..EGA_PALETTE_SIZE {
                                palette[palette_index][n] = res.get_byte();
                            }
                        },
                        //Ok(PicOpX::Mono0) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::Mono1) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::Mono2) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::Mono3) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::Mono4) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::EmbeddedView) => { return Err(PictureError::TODO) },
                        //Ok(PicOpX::SetPriorityTable) => { return Err(PictureError::TODO) },
                        Ok(v) => { return Err(PictureError::UnimplementedX(v)) },
                        Err(_) => {
                            return Err(PictureError::UnrecognizedOpcodeX(opcodex))
                        }
                    }
                },
                Ok(PicOp::End) => {
                    info!("End");
                    break
                }
                Err(_) => { return Err(PictureError::UnrecognizedOpcode(opcode)) }
            }
        }
        Ok(())
    }

    fn dither_line(&mut self, start_coord: &Coord, end_coord: &Coord, col: u8, priority: u8, control: u8, draw_enable: u32) {
        info!("dither_line: start {},{} end {},{} col {}", start_coord.x, start_coord.y, end_coord.x, end_coord.y, col);

        let mut left = clamp(start_coord.x, 0, SCREEN_WIDTH) as i32;
        let mut top = clamp(start_coord.y, 0, SCREEN_HEIGHT) as i32;
        let mut right = clamp(end_coord.x, 0, SCREEN_WIDTH) as i32;
        let mut bottom = clamp(end_coord.y, 0, SCREEN_HEIGHT) as i32;

        // Horizontal line
        if top == bottom {
            if right < left {
                let temp = right;
                right = left;
                left = temp;
            }
            for n in left..right + 1 {
                self.put_pixel(n, top, draw_enable, col, priority, control);
            }
            return
        }

        // Vertical line
        if left == right {
            if top > bottom {
                let temp = top;
                top = bottom;
                bottom = temp;
            }
            for n in top..bottom + 1 {
                self.put_pixel(left, n, draw_enable, col, priority, control);
            }
            return
        }

        // Sloped line
        let dy = bottom as i32 - top as i32;
        let dx = right as i32 - left as i32;
        let stepy: i32 = if dy < 0 { -1 } else { 1 };
        let stepx: i32 = if dx < 0 { -1 } else { 1 };
        let dy = dy.abs() * 2;
        let dx = dx.abs() * 2;

        // First and last pixel
        self.put_pixel(left, top, draw_enable, col, priority, control);
        self.put_pixel(right, bottom, draw_enable, col, priority, control);

        if dx > dy {
            // Going horizontal
            let mut fraction = dy - (dx / 2);
            while left != right {
                if fraction >= 0 {
                    top = top + stepy;
                    fraction = fraction - dx;
                }
                left = left + stepx;
                fraction = fraction + dy;
                self.put_pixel(left, top, draw_enable, col, priority, control);
            }
        } else {
            // Going vertical
            let mut fraction = dx - (dy / 2);
            while top != bottom {
                if fraction >= 0 {
                    left = left + stepx;
                    fraction = fraction - dy;
                }
                top = top + stepy;
                fraction = fraction + dx;
            }
            self.put_pixel(left, top, draw_enable, col, priority, control);
        }
    }

    fn dither_fill(&mut self, coord: &Coord, _col: u8, _priority: u8, _control: u8, _draw_enable: u32)
    {
        info!("dither_fill: {},{}", coord.x, coord.y);
    }

    fn draw_pattern(&mut self, coord: &Coord, draw_enable: u32, col: u8, priority: u8, control: u8, pattern_code: u8, _pattern_nr: u8)
    {
        info!("draw_pattern: {},{}", coord.x, coord.y);

        let size = (pattern_code & PATTERN_MASK_SIZE) as u32;
        let x = clamp(coord.x - size, 0, SCREEN_WIDTH - 1);
        let y = clamp(coord.y - size, 0, SCREEN_HEIGHT - 1);

        let rect = Rect{ left: x, top: y, right: x + (size * 2) + 1, bottom: y + (size * 2) + 1 };
        if (pattern_code & PATTERN_FLAG_RECTANGLE) != 0 {
            // Rectangle
            if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                warn!("draw_pattern: TODO: rect with pattern")
            } else {
                warn!("draw_pattern: TODO: rect without")
            }
        } else {
            // Circle
            if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                warn!("draw_pattern: TODO: circle with pattern")
            } else {
                self.draw_circle(&rect, size, draw_enable, col, priority, control);
            }
        }
    }

    fn draw_circle(&mut self, rect: &Rect, size: u32, draw_enable: u32, col: u8, priority: u8, control: u8)
    {
        let mut bit_num = 0;
        let mut byte_num: usize = 0;

        info!("draw_circle: {}, {} - {}, {} size {}", rect.left, rect.top, rect.right, rect.bottom, size);

        for y in rect.top..rect.bottom {
            for x in rect.left..rect.right {
                let bitmap = PATTERN_CIRCLES[size as usize][byte_num];
                if bit_num == 8 {
                    bit_num = 0;
                    byte_num += 1;
                }

                if (bitmap & (1 << bit_num)) != 0 {
                    self.put_pixel(x as i32, y as i32, draw_enable, col, priority, control);
                }
                bit_num += 1;
            }
        }
    }

    fn put_pixel(&mut self, x: i32, y: i32, draw_enable: u32, col: u8, priority: u8, control: u8)
    {
        if x < 0 || x >= SCREEN_WIDTH as i32 {
            warn!("put_pixel: x {} y {}, x out of range", x, y);
            return
        }
        if y < 0 || y >= SCREEN_HEIGHT as i32 {
            warn!("put_pixel: x {} y {}, y out of range", x, y);
            return
        }

        if (draw_enable & DRAW_ENABLE_VISUAL) != 0 {
            self.visual.set_pixel(x as u32, y as u32, ega_color_to_rgb(col));
        }

        if (draw_enable & DRAW_ENABLE_PRIORITY) != 0 {
            self.priority.set_pixel(x as u32, y as u32, ega_color_to_rgb(priority));
        }

        if (draw_enable & DRAW_ENABLE_CONTROL) != 0 {
            self.control.set_pixel(x as u32, y as u32, ega_color_to_rgb(control));
        }
    }
}


fn main() -> Result<(), PictureError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("usage: {} out pic_id", args[0]);
    }
    let extract_path = &args[1];
    let pic_id: i16 = args[2].parse().unwrap();

    let pic_data = std::fs::read(format!("{}/pic.{:03}", extract_path, pic_id))?;

    let mut pic = Picture::new();
    if let Err(x) = pic.load(&pic_data) {
        println!("load error: {:?}", x);
    }

    let _ = pic.visual.save("visual.bmp");
    let _ = pic.control.save("control.bmp");
    let _ = pic.priority.save("priority.bmp");
    Ok(())
}
