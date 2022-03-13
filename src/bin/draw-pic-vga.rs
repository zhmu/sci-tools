extern crate log;

extern crate scitools;

use packed_struct::prelude::*;
use scitools::{palette, stream};
use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::env;
use gif::{Encoder};
use std::fs::File;

#[derive(Debug)]
pub enum PictureError {
    IoError(std::io::Error),
    InvalidPalette(usize),
    ObsoleteOpcode(u8),
    UnrecognizedOpcode(u8),
    UnrecognizedOpcodeX(u8),
    Unimplemented(PicOp),
    UnimplementedSpecial(PicOpSpecial),
}

impl From<std::io::Error> for PictureError {
    fn from(error: std::io::Error) -> Self {
       PictureError::IoError(error)
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct PicHeader {
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
pub struct PicHeader2 {
    pub visual_header_offset: u32,
    pub polygon_offset: u32,
    pub pri_len: u16,
}

// XXX

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct CelHeader {
    pub x_dim: u16,
    pub y_dim: u16,
    pub x_off: u16,
    pub y_off: u16,
    pub skip_color: u8,
    pub compress_type: u8,
    pub comp_remap_count: u16,
    pub compress_size: u32,
    pub control_size: u32,
    pub palette_offset: u32,
    pub data_offset: u32,
    pub color_offset: u32,
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct CelHeader2 {
    pub compress_remap_offset: u32,
}

const SCREEN_HEIGHT: u32 = 200;
const SCREEN_WIDTH: u32 = 320;

struct Coord {
    x: u32,
    y: u32
}

#[derive(TryFromPrimitive)]
#[derive(Debug)]
#[repr(u8)]
pub enum PicOp {
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
pub enum PicOpSpecial {
    DrawBitmap = 0x01,
    SetColorMap  = 0x02,
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
    visual: Vec<u8>,
    priority: Vec<u8>,
    control: Vec<u8>
}

fn draw_cel(data: &[u8], cel_offset: usize, pen_x: u16, pen_y: u16, visual: &mut [u8])
{
    let cel = CelHeader::unpack_from_slice(&data[cel_offset..cel_offset+32]).unwrap();
    let _cel2 = CelHeader2::unpack_from_slice(&data[cel_offset+32..cel_offset+36]).unwrap();

    let mut color_offset = cel.color_offset as usize;
    let mut data_offset = cel.data_offset as usize;

    let mut line = vec![ 0u8; 320 ];

    for y in 0..(cel.y_dim as usize) {
        // Decompress line
        let mut line_pos: usize = 0;
        while line_pos < (cel.x_dim as usize) {
            let control = data[data_offset];
            data_offset += 1;
            if (control & 0x80) == 0 {
                for _ in 0..control {
                    let color = data[color_offset];
                    color_offset += 1;
                    line[line_pos] = color;
                    line_pos += 1;
                }
            } else /* (control & 0x80) != 0 */ {
                let color;
                if (control & 0x40) == 0 {
                    color = data[color_offset];
                    color_offset += 1;
                } else /* (control & 0x40) != 0 */ {
                    color = cel.skip_color;
                }
                for _ in 0..(control & 0x3f) {
                    line[line_pos] = color;
                    line_pos += 1;
                }
            }
        }

        // Copy line
        for x in 0..(cel.x_dim as usize) {
            // TODO remapping of palette
            visual[((pen_y as usize + y) * (SCREEN_WIDTH as usize) + (pen_x as usize + x)) as usize] = line[x];
        }
    }
}

impl Picture {
    pub fn new() -> Self {
        let screen_size = (SCREEN_WIDTH * SCREEN_HEIGHT) as usize;
        let visual = vec![0; screen_size];
        let priority = vec![0; screen_size];
        let control = vec![0; screen_size];
        Picture{ visual, priority, control }
    }

    pub fn load(&mut self, data: &[u8], load_palette: bool, palette: &mut [ u8; 768 ]) -> Result<(), PictureError> {
        let pic = PicHeader::unpack_from_slice(&data[0..32]).unwrap();
        let pic2 = PicHeader2::unpack_from_slice(&data[32..42]).unwrap();

        if load_palette {
            let pal_data = &data[pic.palette_offset as usize..];
            palette::parse_vga_palette(&pal_data, palette);
        }

        println!("pic: n_prio {} cel_count {} vector_offset {}", pic.n_priorities, pic.cel_count, pic.vector_offset);
        if pic.cel_count > 0 {
            let cel_offset = pic2.visual_header_offset as usize;
            let pen_x = 0;
            let pen_y = 0;
            draw_cel(&data, cel_offset, pen_x, pen_y, self.visual.as_mut_slice());
        }

        let mut res = stream::Streamer::new(&data[pic.vector_offset as usize..], 0);
        let mut c_color: u8 = 0xff;
        let mut v_color: u8 = 0xff;
        let mut p_color: u8 = 0xff;
        while !res.end_of_stream() {
            let opcode = res.get_byte();
            match PicOp::try_from(opcode) {
                Ok(PicOp::SetColor) => {
                    v_color = res.get_byte();
                },
                Ok(PicOp::ClearColor) => {
                    v_color = 0xff;
                },
                Ok(PicOp::SetPriority) => {
                    let color = res.get_byte();
                    p_color = color << 4;
                },
                Ok(PicOp::ClearPriority) => {
                    p_color = 0xff;
                },
                Ok(PicOp::ShortBrush) => {
                    return Err(PictureError::UnrecognizedOpcode(opcode))
                },
                Ok(PicOp::MediumLines) => {
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
                        self.line(&old_coord, &coord, c_color, p_color, v_color);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::AbsoluteLines) => {
                    let mut old_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        self.line(&old_coord, &coord, c_color, p_color, v_color);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::ShortLines) => {
                    let mut old_coord = get_abs_coords(&mut res);
                    while res.peek_byte() < 0xf0 {
                        let coord = get_rel_coords(&mut res, &old_coord);
                        self.line(&old_coord, &coord, c_color, p_color, v_color);
                        old_coord = coord;
                    }
                },
                Ok(PicOp::Fill) => {
                    while res.peek_byte() < 0xf0 {
                        let coord = get_abs_coords(&mut res);
                        self.dither_fill(&coord, c_color, p_color, v_color);
                    }
                },
                Ok(PicOp::SetBrushSize) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(PicOp::AbsoluteBrush) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(PicOp::SetControl) => {
                    c_color = res.get_byte();
                },
                Ok(PicOp::ClearControl) => {
                    c_color = 0xff;
                },
                Ok(PicOp::MediumBrush) => {
                    return Err(PictureError::ObsoleteOpcode(opcode))
                },
                Ok(PicOp::Special) => {
                    let opcodex = res.get_byte();
                    match PicOpSpecial::try_from(opcodex) {
                        Ok(v) => { return Err(PictureError::UnimplementedSpecial(v)) },
                        Err(_) => {
                            return Err(PictureError::UnrecognizedOpcodeX(opcodex))
                        }
                    }
                },
                Ok(PicOp::End) => {
                    break
                }
                Err(_) => { return Err(PictureError::UnrecognizedOpcode(opcode)) }
            }
        }
        Ok(())
    }

    fn line(&mut self, start_coord: &Coord, end_coord: &Coord, c_color: u8, p_color: u8, v_color: u8) {
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
                self.put_pixel(n, top, c_color, p_color, v_color);
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
                self.put_pixel(left, n, c_color, p_color, v_color);
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
        self.put_pixel(left, top, c_color, p_color, v_color);
        self.put_pixel(right, bottom, c_color, p_color, v_color);

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
                self.put_pixel(left, top, c_color, p_color, v_color);
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
            self.put_pixel(left, top, c_color, p_color, v_color);
        }
    }

    fn dither_fill(&mut self, coord: &Coord, _c_color: u8, _p_color: u8, _v_color: u8)
    {
    }

    fn put_pixel(&mut self, x: i32, y: i32, c_color: u8, p_color: u8, v_color: u8)
    {
        if x < 0 || x >= SCREEN_WIDTH as i32 {
            return
        }
        if y < 0 || y >= SCREEN_HEIGHT as i32 {
            return
        }

        let offset = (y as u32 * SCREEN_WIDTH + x as u32) as usize;
        if v_color != 0xff {
            self.visual[offset] = v_color;
        }

        if p_color != 0xff {
            self.priority[offset] = p_color;
        }

        if c_color != 0xff {
            self.control[offset] = c_color;
        }
    }
}

fn create_gif(fname: &str, width: u16, height: u16, palette: &[ u8; 768 ]) -> gif::Encoder<File> {
    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, width, height, palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, width: u16, height: u16, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(width, height, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), PictureError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 && args.len() != 3 {
        panic!("usage: {} path/pic.num [path/palette.num]", args[0]);
    }
    let pic_path = &args[1];
    let pic_data = std::fs::read(pic_path)?;

    let mut palette = [ 0u8; 768 ];
    let mut got_palette = false;
    if args.len() > 2 {
        let pal_path = &args[2];
        let pal_data = std::fs::read(pal_path)?;

        palette::parse_vga_palette(&pal_data, &mut palette);
        got_palette = true;
    }

    let mut pic = Picture::new();
    if let Err(x) = pic.load(&pic_data, !got_palette, &mut palette) {
        println!("load error: {:?}", x);
    }

    let mut control_gif = create_gif("/tmp/control.gif", SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut control_gif, SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &pic.control);
    let mut prio_gif = create_gif("/tmp/prio.gif", SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut prio_gif, SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &pic.priority);
    let mut vis_gif = create_gif("/tmp/vis.gif", SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut vis_gif, SCREEN_WIDTH as u16, SCREEN_HEIGHT as u16, &pic.visual);

    Ok(())
}
