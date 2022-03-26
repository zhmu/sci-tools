use packed_struct::prelude::*;
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::collections::VecDeque;
use std::io::Cursor;
use byteorder::ReadBytesExt;
use crate::{cel, palette};

#[derive(Debug)]
pub enum PictureError {
    IoError(std::io::Error),
    InvalidPalette(usize),
    UnrecognizedOpcode(u8),
    UnrecognizedOpcodeX(u8),
    Unimplemented(PicOp),
    UnimplementedX(PicOpX),
    ObsoleteOpcode(u8),
    UnimplementedSpecial(VGAPicOpSpecial),
}

impl From<std::io::Error> for PictureError {
    fn from(error: std::io::Error) -> Self {
       PictureError::IoError(error)
    }
}

pub const SCREEN_HEIGHT: i32 = 200;
pub const SCREEN_WIDTH: i32 = 320;

const NO_COLOR: u8 = 255;

const DRAW_ENABLE_VISUAL: u32 = 1;
const DRAW_ENABLE_PRIORITY: u32 = 2;
const DRAW_ENABLE_CONTROL: u32 = 4;

const PATTERN_MASK_SIZE: u8 = 0x07;
const PATTERN_FLAG_RECTANGLE: u8 = 0x10;
const PATTERN_FLAG_USE_PATTERN: u8 = 0x20;

const EGA_PALETTE_COUNT: usize = 4;
const EGA_PALETTE_SIZE: usize = 40;

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct VGAPicHeader {
    pub header_size: u16,
    pub n_priorities: u8,
    pub pri_line_count: u8,
    pub cel_count: u8,
    pub dummy: u8,
    pub vanish_x: u16,
    pub vanish_y: u16,
    pub view_angle: u16,
    pub vector_size: u32,
    pub vector_offset: u32,
    pub pri_cel_offset: u32,
    pub control_cel_offset: u32,
    pub palette_offset: u32
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct VGAPicHeader2 {
    pub visual_header_offset: u32,
    pub polygon_offset: u32,
    pub pri_len: u16,
}

#[derive(TryFromPrimitive)]
#[derive(Debug)]
#[repr(u8)]
pub enum VGAPicOp {
    SetColor = 0xf0,
    ClearColor = 0xf1,
    SetPriority = 0xf2,
    ClearPriority = 0xf3,
    ShortBrush = 0xf4,
    MediumLines = 0xf5,
    AbsoluteLines = 0xf6,
    ShortLines = 0xf7,
    Fill = 0xf8,
    SetBrushSize = 0xf9,
    AbsoluteBrush = 0xfa,
    SetControl = 0xfb,
    ClearControl = 0xfc,
    MediumBrush= 0xfd,
    Special = 0xfe,
    End = 0xff,
}

#[derive(TryFromPrimitive)]
#[derive(Debug)]
#[repr(u8)]
pub enum VGAPicOpSpecial {
    DrawBitmap = 0x01,
    SetColorMap = 0x02,
}

fn get_drawing_mask(color: u8, prio: u8, control: u8) -> u32 {
    let mut flag = 0;
    if color != NO_COLOR {
        flag = flag | DRAW_ENABLE_VISUAL;
    }
    if prio != NO_COLOR {
        flag = flag | DRAW_ENABLE_PRIORITY;
    }
    if control != NO_COLOR {
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

fn is_next_byte_a_command(rdr: &mut Cursor<&[u8]>) -> bool {
    let offset = rdr.position();
    let next_byte = rdr.read_u8().unwrap_or(0);
    rdr.set_position(offset);
    next_byte >= 0xf0
}

fn get_abs_coords(rdr: &mut Cursor<&[u8]>) -> Result<Coord, std::io::Error> {
    let coord_prefix = rdr.read_u8()?;
    let mut x = rdr.read_u8()? as i32;
    let mut y = rdr.read_u8()? as i32;
    x = x | ((coord_prefix & 0xf0) as i32) << 4;
    y = y | ((coord_prefix & 0x0f) as i32) << 8;
    Ok(Coord{ x, y })
}

fn get_rel_coords(rdr: &mut Cursor<&[u8]>, base: &Coord) -> Result<Coord, std::io::Error> {
    let input = rdr.read_u8()?;
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
    Ok(Coord{ x, y })
}

fn get_rel_coords_med(rdr: &mut Cursor<&[u8]>, base: &Coord) -> Result<Coord, std::io::Error> {
    let input = rdr.read_u8()?;
    let mut y = base.y;
    if (input & 0x80) != 0 {
        y -= (input & 0x7f) as i32;
    } else {
        y += input as i32;
    }
    let input = rdr.read_u8()?;
    let mut x = base.x;
    if (input & 0x80) != 0 {
        x -= (128 - (input & 0x7f)) as i32;
    } else {
        x += input as i32;
    }
    Ok(Coord{ x, y })
}

fn update_pattern_nr(rdr: &mut Cursor<&[u8]>, pattern_code: u8, pattern_nr: &mut u8)
{
    if (pattern_code & PATTERN_FLAG_USE_PATTERN) != 0 {
        if let Ok(code) = rdr.read_u8() {
            *pattern_nr = (code >> 1) & 0x7f;
        }
    }
}

fn swap_i32(a: &mut i32, b: &mut i32) {
    let t = *b;
    *b = *a;
    *a = t;
}

pub struct Picture {
    pub visual: Vec<u8>,
    pub priority: Vec<u8>,
    pub control: Vec<u8>,
    port_x: i32,
    port_y: i32,
    is_ega: bool
}

impl Picture {
    pub fn new() -> Self {
        let screen_size = (SCREEN_WIDTH * SCREEN_HEIGHT) as usize;
        let visual = vec![0; screen_size];
        let priority = vec![0; screen_size];
        let control = vec![0; screen_size];
        let port_x = 0;
        let port_y = 10;
        Picture{ visual, priority, control, port_x, port_y, is_ega: false }
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

    pub fn load_ega(&mut self, data: &[u8]) -> Result<(), PictureError> {
        let mut priority: u8 = NO_COLOR;
        let mut control: u8 = NO_COLOR;
        let mut col: u8 = 0;
        let mut pattern_nr: u8 = 0;
        let mut pattern_code: u8 = 0;

        let mut palette = [ [ 0 as u8; EGA_PALETTE_SIZE ]; EGA_PALETTE_COUNT];
        self.is_ega = true;
        if self.is_ega {
            for n in 0..EGA_PALETTE_COUNT {
                for m in 0..EGA_PALETTE_SIZE {
                    palette[n][m] = EGA_DEFAULT_PALETTE[m];
                }
            }
        }

        let mut rdr = Cursor::new(data);
        loop {
            let opcode = rdr.read_u8()?;
            match PicOp::try_from(opcode) {
                Ok(PicOp::SetColor) => {
                    let code = rdr.read_u8()?;
                    if self.is_ega {
                        let code = code as usize;
                        col = palette[code / EGA_PALETTE_SIZE][code % EGA_PALETTE_SIZE];
                        col = col ^ (col << 4);
                    } else {
                        col = code;
                    }
                },
                Ok(PicOp::DisableVisual) => {
                    col = NO_COLOR;
                },
                Ok(PicOp::SetPriority) => {
                    let code = rdr.read_u8()?;
                    priority = code & 0xf;
                },
                Ok(PicOp::DisablePriority) => {
                    priority = NO_COLOR;
                },
                Ok(PicOp::RelativePatterns) => {
                    update_pattern_nr(&mut rdr, pattern_code, &mut pattern_nr);

                    let mut coord = get_abs_coords(&mut rdr)?;
                    self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);

                    while !is_next_byte_a_command(&mut rdr) {
                        update_pattern_nr(&mut rdr, pattern_code, &mut pattern_nr);
                        coord = get_rel_coords(&mut rdr, &coord)?;
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::RelativeMediumLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_rel_coords_med(&mut rdr, &prev_coord)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::RelativeLongLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_abs_coords(&mut rdr)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::RelativeShortLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_rel_coords(&mut rdr, &prev_coord)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, col, priority, control);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::Fill) => {
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_abs_coords(&mut rdr)?;
                        self.dither_fill(&coord, col, priority, control);
                   }
                },
                Ok(PicOp::SetPattern) => {
                    let code = rdr.read_u8()?;
                    pattern_code = code;
                },
                Ok(PicOp::AbsolutePatterns) => {
                    while !is_next_byte_a_command(&mut rdr) {
                        update_pattern_nr(&mut rdr, pattern_code, &mut pattern_nr);
                        let coord = get_abs_coords(&mut rdr)?;
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                    }
                },
                Ok(PicOp::SetControl) => {
                    let code = rdr.read_u8()?;
                    control = code & 0xf;
                },
                Ok(PicOp::DisableControl) => {
                    control = NO_COLOR;
                },
                Ok(PicOp::RelativeMediumPatterns) => {
                    update_pattern_nr(&mut rdr, pattern_code, &mut pattern_nr);
                    let mut prev_coord = get_abs_coords(&mut rdr)?;

                    self.draw_pattern(&prev_coord, col, priority, control, pattern_code, pattern_nr);
                    while !is_next_byte_a_command(&mut rdr) {
                        update_pattern_nr(&mut rdr, pattern_code, &mut pattern_nr);
                        let coord = get_rel_coords_med(&mut rdr, &prev_coord)?;
                        self.draw_pattern(&coord, col, priority, control, pattern_code, pattern_nr);
                        prev_coord = coord;
                    }
                },
                Ok(PicOp::X) => {
                    let opcodex = rdr.read_u8()?;
                    match PicOpX::try_from(opcodex) {
                        //Ok(PicOpX::SetPaletteEntry) => { return Err(PictureError::PicOpX) },
                        Ok(PicOpX::SetPalette) => {
                            let palette_index = rdr.read_u8()? as usize;
                            if palette_index >= EGA_PALETTE_COUNT {
                                return Err(PictureError::InvalidPalette(palette_index))
                            }
                            for n in 0..EGA_PALETTE_SIZE {
                                palette[palette_index][n] = rdr.read_u8()?;
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
                    if self.is_ega {
                        self.ega_dither();
                    }
                    return Ok(())
                }
                Err(_) => { return Err(PictureError::UnrecognizedOpcode(opcode)) }
            }
        }
    }

    pub fn load_vga(&mut self, data: &[u8], load_palette: bool, palette: &mut [ u8; 768 ]) -> Result<(), PictureError> {
        let pic = VGAPicHeader::unpack_from_slice(&data[0..32]).unwrap();
        let pic2 = VGAPicHeader2::unpack_from_slice(&data[32..42]).unwrap();

        self.is_ega = false;
        if load_palette {
            let pal_data = &data[pic.palette_offset as usize..];
            palette::parse_vga_palette(&pal_data, palette);
        }

        if pic.cel_count > 0 {
            let cel_offset = pic2.visual_header_offset as usize;
            let mut cel = cel::Cel::new();
            cel.load(&data, cel_offset);

            let pen_x = 0;
            let pen_y = 0;
            for y in 0..(cel.height as usize) {
                for x in 0..(cel.width as usize) {
                    let pixel = cel.visual[(y * cel.width as usize) + x];
                    self.visual[(pen_y + y) * SCREEN_WIDTH as usize + pen_x as usize + x] = pixel;
                }
            }
        }

        let mut rdr = Cursor::new(&data[pic.vector_offset as usize..]);
        let mut c_color: u8 = NO_COLOR;
        let mut v_color: u8 = NO_COLOR;
        let mut p_color: u8 = NO_COLOR;
        loop {
            let opcode = rdr.read_u8()?;
            match VGAPicOp::try_from(opcode) {
                Ok(VGAPicOp::SetColor) => {
                    v_color = rdr.read_u8()?;
                },
                Ok(VGAPicOp::ClearColor) => {
                    v_color = NO_COLOR;
                },
                Ok(VGAPicOp::SetPriority) => {
                    let color = rdr.read_u8()?;
                    p_color = color << 4;
                },
                Ok(VGAPicOp::ClearPriority) => {
                    p_color = NO_COLOR;
                },
                Ok(VGAPicOp::SetControl) => {
                    c_color = rdr.read_u8()?;
                },
                Ok(VGAPicOp::ClearControl) => {
                    c_color = NO_COLOR;
                },
                Ok(VGAPicOp::ShortBrush) => {
                    return Err(PictureError::UnrecognizedOpcode(opcode))
                },
                Ok(VGAPicOp::MediumBrush) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(VGAPicOp::AbsoluteBrush) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(VGAPicOp::ShortLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_rel_coords(&mut rdr, &prev_coord)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, c_color, p_color, v_color);
                        prev_coord = coord;
                    }
                },
                Ok(VGAPicOp::MediumLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_rel_coords_med(&mut rdr, &prev_coord)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, c_color, p_color, v_color);
                        prev_coord = coord;
                    }
                },
                Ok(VGAPicOp::AbsoluteLines) => {
                    let mut prev_coord = get_abs_coords(&mut rdr)?;
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_abs_coords(&mut rdr)?;
                        let mut rect = Rect{ left: prev_coord.x, top: prev_coord.y, right: coord.x, bottom: coord.y };
                        self.offset_rect(&mut rect);
                        self.clamp_rect(&mut rect);
                        self.draw_line(&rect, c_color, p_color, v_color);
                        prev_coord = coord;
                    }
                },
                Ok(VGAPicOp::Fill) => {
                    while !is_next_byte_a_command(&mut rdr) {
                        let coord = get_abs_coords(&mut rdr)?;
                        self.dither_fill(&coord, c_color, p_color, v_color);
                    }
                },
                Ok(VGAPicOp::SetBrushSize) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(VGAPicOp::Special) => {
                    let opcodex = rdr.read_u8()?;
                    match VGAPicOpSpecial::try_from(opcodex) {
                        Ok(v) => { return Err(PictureError::UnimplementedSpecial(v)) },
                        Err(_) => {
                            return Err(PictureError::UnrecognizedOpcodeX(opcodex))
                        }
                    }
                },
                Ok(VGAPicOp::End) => {
                    break
                }
                Err(_) => { return Err(PictureError::UnrecognizedOpcode(opcode)) }
            }
        }
        Ok(())
    }

    fn ega_dither(&mut self)
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
        if self.is_ega {
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
        if x < 0 || x >= SCREEN_WIDTH { return }
        if y < 0 || y >= SCREEN_HEIGHT { return }

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
