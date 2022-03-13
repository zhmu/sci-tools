extern crate bmp;
extern crate log;

extern crate scitools;

use scitools::{palette, stream};
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::collections::VecDeque;
use log::{info, warn};
use std::env;
use gif::{Encoder};
use std::fs::File;

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

const SCREEN_HEIGHT: i32 = 200;
const SCREEN_WIDTH: i32 = 320;

const DRAW_ENABLE_VISUAL: u32 = 1;
const DRAW_ENABLE_PRIORITY: u32 = 2;
const DRAW_ENABLE_CONTROL: u32 = 4;

const PATTERN_MASK_SIZE: u8 = 0x07;
const PATTERN_FLAG_RECTANGLE: u8 = 0x10;
const PATTERN_FLAG_USE_PATTERN: u8 = 0x20;

const EGA_PALETTE_COUNT: usize = 4;
const EGA_PALETTE_SIZE: usize = 40;

const IS_EGA: bool = true;

fn get_drawing_mask(color: u8, prio: u8, control: u8) -> u32 {
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

#[derive(Clone,Copy)]
struct Coord {
    x: i32,
    y: i32
}

struct Rect {
    left: i32,
    top: i32,
    right: i32,
    bottom: i32
}

const EGA_DEFAULT_PALETTE: [ u8; EGA_PALETTE_SIZE ] = [
    0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
    0x88, 0x99, 0xaa, 0xbb, 0xcc, 0xdd, 0xee, 0x88,
    0x88, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x88,
    0x88, 0xf9, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xff,
    0x08, 0x91, 0x2a, 0x3b, 0x4c, 0x5d, 0x6e, 0x88
];

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

const PATTERN_TEXTURE: [ u8; 32 ] = [
    0x20, 0x94, 0x02, 0x24, 0x90, 0x82, 0xa4, 0xa2,
    0x82, 0x09, 0x0a, 0x22, 0x12, 0x10, 0x42, 0x14,
    0x91, 0x4a, 0x91, 0x11, 0x08, 0x12, 0x25, 0x10,
    0x22, 0xa8, 0x14, 0x24, 0x00, 0x50, 0x24, 0x04
];

const PATTERN_OFFSET: [ u8; 128 ] = [
    0x00, 0x18, 0x30, 0xc4, 0xdc, 0x65, 0xeb, 0x48,
    0x60, 0xbd, 0x89, 0x05, 0x0a, 0xf4, 0x7d, 0x7d,
    0x85, 0xb0, 0x8e, 0x95, 0x1f, 0x22, 0x0d, 0xdf,
    0x2a, 0x78, 0xd5, 0x73, 0x1c, 0xb4, 0x40, 0xa1,
    0xb9, 0x3c, 0xca, 0x58, 0x92, 0x34, 0xcc, 0xce,
    0xd7, 0x42, 0x90, 0x0f, 0x8b, 0x7f, 0x32, 0xed,
    0x5c, 0x9d, 0xc8, 0x99, 0xad, 0x4e, 0x56, 0xa6,
    0xf7, 0x68, 0xb7, 0x25, 0x82, 0x37, 0x3a, 0x51,
    0x69, 0x26, 0x38, 0x52, 0x9e, 0x9a, 0x4f, 0xa7,
    0x43, 0x10, 0x80, 0xee, 0x3d, 0x59, 0x35, 0xcf,
    0x79, 0x74, 0xb5, 0xa2, 0xb1, 0x96, 0x23, 0xe0,
    0xbe, 0x05, 0xf5, 0x6e, 0x19, 0xc5, 0x66, 0x49,
    0xf0, 0xd1, 0x54, 0xa9, 0x70, 0x4b, 0xa4, 0xe2,
    0xe6, 0xe5, 0xab, 0xe4, 0xd2, 0xaa, 0x4c, 0xe3,
    0x06, 0x6f, 0xc6, 0x4a, 0xa4, 0x75, 0x97, 0xe1,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
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
    let mut x = res.get_byte() as i32;
    let mut y = res.get_byte() as i32;
    x = x | ((coord_prefix & 0xf0) as i32) << 4;
    y = y | ((coord_prefix & 0x0f) as i32) << 8;
    Coord{ x, y }
}

fn get_rel_coords(res: &mut stream::Streamer, base: &Coord) -> Coord {
    let input = res.get_byte();
    let mut x = base.x;
    if (input & 0x80) != 0 {
        x -= ((input >> 4) & 7) as i32;
    } else {
        x += (input >> 4) as i32;
    }
    let mut y = base.y;
    if (input & 0x08) != 0 {
        y -= (input & 0x07) as i32;
    } else {
        y += (input & 0x07) as i32;
    }
    Coord{ x, y }
}

fn get_rel_coords_med(res: &mut stream::Streamer, base: &Coord) -> Coord {
    let input = res.get_byte();
    let mut y = base.y;
    if (input & 0x80) != 0 {
        y -= (input & 0x7f) as i32;
    } else {
        y += input as i32;
    }
    let input = res.get_byte();
    let mut x = base.x;
    if (input & 0x80) != 0 {
        x -= (128 - (input & 0x7f)) as i32;
    } else {
        x += input as i32;
    }
    Coord{ x, y }
}

fn update_pattern_nr(res: &mut stream::Streamer, pattern_code: u8, pattern_nr: &mut u8)
{
    if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
        let code = res.get_byte();
        *pattern_nr = (code >> 1) & 0x7f;
    }
}

fn swap_i32(a: &mut i32, b: &mut i32) {
    let t = *b;
    *b = *a;
    *a = t;
}

pub struct Picture {
    visual: Vec<u8>,
    priority: Vec<u8>,
    control: Vec<u8>,
    port_x: i32,
    port_y: i32
}

impl Picture {
    pub fn new() -> Self {
        let screen_size = (SCREEN_WIDTH * SCREEN_HEIGHT) as usize;
        let visual = vec![0; screen_size];
        let priority = vec![0; screen_size];
        let control = vec![0; screen_size];
        let port_x = 0;
        let port_y = 10;
        Picture{ visual, priority, control, port_x, port_y }
    }

    pub fn clear(&mut self) {
        let mask = DRAW_ENABLE_VISUAL | DRAW_ENABLE_PRIORITY | DRAW_ENABLE_CONTROL;
        let white = 15;
        for y in self.port_y..SCREEN_HEIGHT {
            for x in self.port_x..SCREEN_WIDTH {
                self.put_pixel(x as i32, y as i32, mask, white, 0, 0);
            }
        }
    }

    fn offset_rect(&self, rect: &mut Rect) {
        rect.top = rect.top + self.port_y;
        rect.bottom = rect.bottom + self.port_y;
        rect.left = rect.left + self.port_x;
        rect.right= rect.right + self.port_x;
    }

    fn clamp_rect(&self, rect: &mut Rect) {
        rect.left = clamp(rect.left, 0, SCREEN_WIDTH);
        rect.right = clamp(rect.right, 0, SCREEN_WIDTH);
        rect.top = clamp(rect.top, 0, SCREEN_HEIGHT);
        rect.bottom = clamp(rect.bottom, 0, SCREEN_HEIGHT);
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), PictureError> {
        let mut priority: u8 = 255;
        let mut control: u8 = 255;
        let mut col: u8 = 0;
        let mut pattern_nr: u8 = 0;
        let mut pattern_code: u8 = 0;

        let mut palette = [ [ 0 as u8; EGA_PALETTE_SIZE ]; EGA_PALETTE_COUNT];

        if IS_EGA {
            for n in 0..EGA_PALETTE_COUNT {
                for m in 0..EGA_PALETTE_SIZE {
                    palette[n][m] = EGA_DEFAULT_PALETTE[m];
                }
            }
        }

        let mut res = stream::Streamer::new(data, 0);
        while !res.end_of_stream() {
            let opcode = res.get_byte();
            match PicOp::try_from(opcode) {
                Ok(PicOp::SetColor) => {
                    let code = res.get_byte();
                    info!("SetColor({})", code);
                    if IS_EGA {
                        let code = code as usize;
                        col = palette[code / EGA_PALETTE_SIZE][code % EGA_PALETTE_SIZE];
                        col = col ^ (col << 4);
                    } else {
                        col = code;
                    }
                },
                Ok(PicOp::DisableVisual) => {
                    info!("DisableVisual");
                    col = 255;
                },
                Ok(PicOp::SetPriority) => {
                    let code = res.get_byte();
                    info!("SetPriority({})", code);
                    priority = code & 0xf;
                },
                Ok(PicOp::DisablePriority) => {
                    info!("DisablePriority()");
                    priority = 255;
                },
                Ok(PicOp::RelativePatterns) => {
                    info!("RelativePatterns()");
                    update_pattern_nr(&mut res, pattern_code, &mut pattern_nr);

                    let mut coord = get_abs_coords(&mut res);
                    self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);

                    while res.peek_byte() < 0xf0 {
                        update_pattern_nr(&mut res, pattern_code, &mut pattern_nr);
                        coord = get_rel_coords(&mut res, &coord);
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::RelativeMediumLines) => {
                    info!("RelativeMediumLines()");
                    let mut prev_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_rel_coords_med(&mut res, &prev_coord);
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::RelativeLongLines) => {
                    info!("RelativeLongLines()");
                    let mut prev_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::RelativeShortLines) => {
                    info!("RelativeShortLines()");
                    let mut prev_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_rel_coords(&mut res, &prev_coord);
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::Fill) => {
                    info!("Fill()");
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        self.dither_fill(&coord, col, priority, control);
                   }
                },
                Ok(PicOp::SetPattern) => {
                    let code = res.get_byte();
                    info!("SetPattern({})", code);
                    pattern_code = code;
                },
                Ok(PicOp::AbsolutePatterns) => {
                    while res.peek_byte() < 0xf0 {
                        update_pattern_nr(&mut res, pattern_code, &mut pattern_nr);
                        let coord = get_abs_coords(&mut res);
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::SetControl) => {
                    let code = res.get_byte();
                    info!("SetControl({})", code);
                    control = code & 0xf;
                },
                Ok(PicOp::DisableControl) => {
                    control = 255;
                },
                Ok(PicOp::RelativeMediumPatterns) => {
                    info!("RelativeMediumPatterns()");
                    update_pattern_nr(&mut res, pattern_code, &mut pattern_nr);
                    let mut prev_coord = get_abs_coords(&mut res);

                    self.draw_pattern(&prev_coord, col, priority, control, pattern_code, pattern_nr);
                    while res.peek_byte() < 0xf0 {
                        update_pattern_nr(&mut res, pattern_code, &mut pattern_nr);
                        let coord = get_rel_coords_med(&mut res, &prev_coord);
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                        prev_coord = coord;
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
                    if IS_EGA {
                        self.dither();
                    }
                    break
                }
                Err(_) => { return Err(PictureError::UnrecognizedOpcode(opcode)) }
            }
        }
        Ok(())
    }

    fn dither(&mut self)
    {
        if true /* !undithering_enabled */ {
            // Provides dithering like the original game
            for y in 0..SCREEN_HEIGHT {
                for x in 0..SCREEN_WIDTH {
                    let offset = (y * SCREEN_WIDTH + x) as usize;
                    let mut color = self.visual[offset];
                    if (color & 0xf0) != 0 {
                        color = color ^ (color << 4);
                        if (x ^ y) & 1 != 0 {
                            color = color >> 4;
                        } else {
                            color = color & 0x0f;
                        }
                        self.visual[offset] = color;
                    }
                }
            }
        } else {
            // Uses the wider palette instead of dithering
            for y in 0..SCREEN_HEIGHT {
                for x in 0..SCREEN_WIDTH {
                    let offset = (y * SCREEN_WIDTH + x) as usize;
                    let mut color = self.visual[offset];
                    if (color & 0xf0) != 0 {
                        color = color ^ (color << 4);
                        let dithered_color;
                        if (color & 0xf0) != 0 {
                            dithered_color = color;
                        } else {
                            dithered_color = color << 4;
                        }
/*
                        if (x ^ y) & 1 != 0 {
                            color = color >> 4;
                        } else {
                            color = color & 0x0f;
                        }
*/
                        color = dithered_color;
                    }
                    self.visual[offset] = color;
                }
            }
        }
    }

    fn draw_line(&mut self, rect: &Rect, col: u8, priority: u8, control: u8) {
        let mask = get_drawing_mask(col, priority, control);
        let mut left = rect.left;
        let mut right = rect.right;
        let mut top = rect.top;
        let mut bottom = rect.bottom;

        // Horizontal line
        if top == bottom {
            if right < left { swap_i32(&mut left, &mut right); }
            for n in left..right + 1 {
                self.put_pixel(n, top, mask, col, priority, control);
            }
            return
        }

        // Vertical line
        if left == right {
            if top > bottom { swap_i32(&mut top, &mut bottom); }
            for n in top..bottom + 1 {
                self.put_pixel(left, n, mask, col, priority, control);
            }
            return
        }

        // Sloped line
        let dy = bottom - top;
        let dx = right - left;
        let stepy: i32 = if dy < 0 { -1 } else { 1 };
        let stepx: i32 = if dx < 0 { -1 } else { 1 };
        let dy = dy.abs() * 2;
        let dx = dx.abs() * 2;

        // First and last pixel
        self.put_pixel(left, top, mask, col, priority, control);
        self.put_pixel(right, bottom, mask, col, priority, control);

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
                self.put_pixel(left, top, mask, col, priority, control);
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
                self.put_pixel(left, top, mask, col, priority, control);
            }
        }
    }

    fn is_fill_match(&self, x: i32, y: i32, match_mask: u32, check_col: u8, check_priority: u8, check_control: u8) -> bool
    {
        let offset = (y * SCREEN_WIDTH + x) as usize;
        let is_ega = true;
        if (match_mask & DRAW_ENABLE_VISUAL) != 0 {
            let mut visual_color = self.visual[offset];
            if is_ega {
                if ((x ^ y) & 1) != 0 {
                    visual_color = (visual_color ^ (visual_color >> 4)) & 0xf;
                } else {
                    visual_color = visual_color & 0xf;
                }
            }
            if visual_color == check_col {
                return true;
            }
        }

        if (match_mask & DRAW_ENABLE_PRIORITY) != 0 && self.priority[offset] == check_priority {
            return true;
        }
        if (match_mask & DRAW_ENABLE_CONTROL) != 0 && self.control[offset] == check_control {
            return true;
        }
        false
    }

    fn dither_fill(&mut self, coord: &Coord, col: u8, priority: u8, control: u8)
    {
        let p = Coord{ x: coord.x + self.port_x, y: coord.y + self.port_y};

        let pixel_offset = (p.y * SCREEN_WIDTH + p.x) as usize;
        let mut search_color = self.visual[pixel_offset];
        let search_priority = self.priority[pixel_offset];
        let search_control = self.control[pixel_offset];
        if IS_EGA {
            if ((p.x ^ p.y) & 1) != 0 {
                search_color = (search_color ^ (search_color >> 4)) & 0x0f;
            } else {
                search_color = search_color & 0x0f;
            }
        }

        // Abort drawing on several criterion
        let mut screen_mask = get_drawing_mask(col, priority, control);
        if (screen_mask & DRAW_ENABLE_VISUAL) != 0 {
            let color_white = 15;
            if col == color_white || search_color != color_white { return; }
        } else if (screen_mask & DRAW_ENABLE_PRIORITY) != 0 {
            if priority == 0 || search_priority != 0 { return; }
        } else if (screen_mask & DRAW_ENABLE_CONTROL) != 0 {
            if control == 0 || search_control != 0 { return; }
        }

        // Remove screens that already have the correct value
        if (screen_mask & DRAW_ENABLE_VISUAL) != 0 && search_color == col {
            screen_mask = screen_mask & !DRAW_ENABLE_VISUAL;
        }
        if (screen_mask & DRAW_ENABLE_PRIORITY) != 0 && search_priority == priority {
            screen_mask = screen_mask & !DRAW_ENABLE_PRIORITY;
        }
        if (screen_mask & DRAW_ENABLE_CONTROL) != 0 && search_control == control {
            screen_mask = screen_mask & !DRAW_ENABLE_CONTROL;
        }
        if screen_mask == 0 { return; }

        let mut match_mask: u32 = 0;
        if (screen_mask & DRAW_ENABLE_VISUAL) != 0 {
            match_mask = DRAW_ENABLE_VISUAL;
        } else if (screen_mask & DRAW_ENABLE_PRIORITY) != 0 {
            match_mask = DRAW_ENABLE_PRIORITY;
        } else if (screen_mask & DRAW_ENABLE_CONTROL) != 0 {
            match_mask = DRAW_ENABLE_CONTROL;
        }

        let border_left = self.port_x as i32;
        let border_right = self.port_x + SCREEN_WIDTH - 1;
        let border_top = self.port_y;
        let border_bottom = clamp(self.port_y + SCREEN_HEIGHT - 1, 0, SCREEN_HEIGHT - 1); // XXX ???

        let mut stack: VecDeque<Coord> = VecDeque::new();
        stack.push_back(p);

        loop {
            let p = stack.pop_front();
            if p.is_none() { break; }
            let p = p.unwrap();

            if !self.is_fill_match(p.x, p.y, match_mask, search_color, search_priority, search_control) { continue; }

            self.put_pixel(p.x, p.y, screen_mask, col, priority, control);
            let mut cur_to_left = p.x;
            let mut cur_to_right = p.x;

            while cur_to_left > border_left && self.is_fill_match(cur_to_left - 1, p.y, match_mask, search_color, search_priority, search_control) {
                cur_to_left -= 1;
                self.put_pixel(cur_to_left, p.y, screen_mask, col, priority, control);
            }
            while cur_to_right < border_right && self.is_fill_match(cur_to_right + 1, p.y, match_mask, search_color, search_priority, search_control) {
                cur_to_right += 1;
                self.put_pixel(cur_to_right, p.y, screen_mask, col, priority, control);
            }

            let mut a_set = false;
            let mut b_set = false;
            while cur_to_left <= cur_to_right {
                if p.y > border_top && self.is_fill_match(cur_to_left, p.y - 1, match_mask, search_color, search_priority, search_control) {
                    if !a_set {
                        stack.push_back(Coord{ x: cur_to_left, y: p.y - 1 });
                        a_set = true;
                    }
                } else {
                    a_set = false;
                }

                if p.y < border_bottom && self.is_fill_match(cur_to_left, p.y + 1, match_mask, search_color, search_priority, search_control) {
                    if !b_set {
                        stack.push_back(Coord{ x: cur_to_left, y: p.y + 1 });
                        b_set = true;
                    }
                } else {
                    b_set = false;
                }

                cur_to_left += 1;
            }
        }
    }

    fn draw_rectangle_with_pattern(&mut self, rect: &Rect, col: u8, priority: u8, control: u8, pattern_nr: u8) {
        let mask = get_drawing_mask(col, priority, control);
        let mut pattern_offset: usize = PATTERN_OFFSET[pattern_nr as usize].into();

        for y in rect.top..rect.bottom {
            for x in rect.left..rect.right {
                let pattern_byte = PATTERN_TEXTURE[pattern_offset >> 3];
                if ((pattern_byte >> (7 - (pattern_offset & 7))) & 1) != 0 {
                    self.put_pixel(x, y, mask, col, priority, control);
                }
                pattern_offset += 1;
                if pattern_offset == 255 { pattern_offset = 0 };
            }
        }
    }

    fn draw_rectangle(&mut self, rect: &Rect, col: u8, priority: u8, control: u8) {
        let mask = get_drawing_mask(col, priority, control);
        for y in rect.top..rect.bottom {
            for x in rect.left..rect.right {
                self.put_pixel(x, y, mask, col, priority, control);
            }
        }
    }

    fn draw_pattern(&mut self, coord: &Coord, col: u8, priority: u8, control: u8, pattern_code: u8, pattern_nr: u8)
    {
        let size = (pattern_code & PATTERN_MASK_SIZE) as i32;
        let x = clamp(coord.x - size, 0, SCREEN_WIDTH - 1);
        let y = clamp(coord.y - size, 0, SCREEN_HEIGHT - 1);

        let mut rect = Rect{ left: x, top: y, right: x + (size * 2) + 2, bottom: y + (size * 2) + 1 };
        self.offset_rect(&mut rect);
        self.clamp_rect(&mut rect);

        if (pattern_code & PATTERN_FLAG_RECTANGLE) != 0 {
            if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                self.draw_rectangle_with_pattern(&rect, col, priority, control, pattern_nr);
            } else {
                self.draw_rectangle(&rect, col, priority, control);
            }
        } else {
            if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
                self.draw_circle_with_pattern(&rect, size, col, priority, control, pattern_nr);
            } else {
                self.draw_circle(&rect, size, col, priority, control);
            }
        }
    }

    fn draw_circle(&mut self, rect: &Rect, size: i32, col: u8, priority: u8, control: u8)
    {
        let mask = get_drawing_mask(col, priority, control);
        let mut bit_num = 0;
        let circle_data = PATTERN_CIRCLES[size as usize];

        let mut pattern_offset: usize = 0;
        let mut bitmap = circle_data[pattern_offset];
        for y in rect.top..rect.bottom {
            for x in rect.left..rect.right {
                if bit_num == 8 {
                    pattern_offset += 1;
                    bitmap = circle_data[pattern_offset];
                    bit_num = 0;
                }

                if (bitmap & 1) != 0 {
                    self.put_pixel(x, y, mask, col, priority, control);
                }
                bit_num += 1;
                bitmap = bitmap >> 1;
            }
        }
    }

    fn draw_circle_with_pattern(&mut self, rect: &Rect, size: i32, col: u8, priority: u8, control: u8, pattern_nr: u8)
    {
        let mask = get_drawing_mask(col, priority, control);
        let mut bit_num = 0;
        let circle_data = PATTERN_CIRCLES[size as usize];
        let mut pattern_offset: usize = PATTERN_OFFSET[pattern_nr as usize].into();

        let mut circle_offset: usize = 0;
        let mut bitmap = circle_data[circle_offset];
        for y in rect.top..rect.bottom {
            for x in rect.left..rect.right {
                if bit_num == 8 {
                    circle_offset += 1;
                    bitmap = circle_data[circle_offset];
                    bit_num = 0;
                }
                if (bitmap & 1) != 0 {
                    let pattern_byte = PATTERN_TEXTURE[pattern_offset >> 3];
                    if ((pattern_byte >> (7 - (pattern_offset & 7))) & 1) != 0 {
                        self.put_pixel(x, y, mask, col, priority, control);
                    }
                    pattern_offset += 1;
                    if pattern_offset == 255 { pattern_offset = 0; }
                }
                bit_num += 1;
                bitmap = bitmap >> 1;
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

        let offset = (y * SCREEN_WIDTH + x) as usize;
        if (draw_enable & DRAW_ENABLE_VISUAL) != 0 {
            self.visual[offset] = col;
        }

        if (draw_enable & DRAW_ENABLE_PRIORITY) != 0 {
            self.priority[offset] = priority;
        }

        if (draw_enable & DRAW_ENABLE_CONTROL) != 0 {
            self.control[offset] = control;
        }
    }
}

fn create_gif(fname: &str) -> gif::Encoder<File> {
    let mut palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut palette);

    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), PictureError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} path/pic.nnn", args[0]);
    }
    let pic_path = &args[1];
    let pic_data = std::fs::read(pic_path)?;

    let mut pic = Picture::new();
    pic.clear();
    if let Err(x) = pic.load(&pic_data) {
        println!("load error: {:?}", x);
    }

    let mut visual_gif = create_gif("visual.gif");
    store_gif_bitmap(&mut visual_gif, &pic.visual);

    let mut priority_gif = create_gif("priority.gif");
    store_gif_bitmap(&mut priority_gif, &pic.priority);

    let mut control_gif = create_gif("control.gif");
    store_gif_bitmap(&mut control_gif, &pic.control);

    Ok(())
}
