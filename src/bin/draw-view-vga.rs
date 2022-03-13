extern crate scitools;

use std::env;
use packed_struct::prelude::*;
use byteorder::ReadBytesExt;
use std::io::{Cursor, Seek, SeekFrom};
use std::fs::File;
use gif::{Encoder};

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ViewHeader {
    pub header_size: u16, // excluding this field
    pub num_loops: u8,
    pub v_flags: u8,
    pub hisplit_flag: u8,
    pub dummy2: u8,
    pub cel_count: u16,
    pub palette_offset: u32,
    pub loop_header_size: u8,
    pub cel_header_size: u8,
    pub animation_offset: u32,
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct LoopHeader {
    pub alt_loop: u8,
    pub flags: u8,
    pub num_cels: u8,
    pub dummy: u8,
    pub start_cel: u8,
    pub ending_cel: u8,
    pub repeat_count: u8,
    pub step_size: u8,
    pub palette_offset: u32,
    pub cel_offset: u32,
}

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

#[derive(Debug)]
pub enum ViewError {
    IoError(std::io::Error),
}

impl From<std::io::Error> for ViewError {
    fn from(error: std::io::Error) -> Self {
       ViewError::IoError(error)
    }
}

fn set_palette_rgb(palette: &mut [u8], index: usize, r: u8, g: u8, b: u8)
{
    let index = index * 3;
    palette[index + 0] = r;
    palette[index + 1] = g;
    palette[index + 2] = b;
}

fn blend_colors(c1: u8, c2: u8) -> u8 {
    let c1 = c1 as f32;
    let c2 = c2 as f32;
    let t =
        (c1 / 255.0).powf(2.2 / 1.0) * 255.0 +
        (c2 / 255.0).powf(2.2 / 1.0) * 255.0;
    (0.5 + (0.5 * t / 255.0).powf(1.0 / 2.2) * 255.0) as u8
}

fn create_gif(fname: &str, width: u16, height: u16) -> gif::Encoder<File> {
    let mut palette = [ 0u8; 768 ];
    set_palette_rgb(&mut palette,   0, 0x000, 0x000, 0x000);
    set_palette_rgb(&mut palette,   1, 0x000, 0x000, 0x0AA);
    set_palette_rgb(&mut palette,   2, 0x000, 0x0AA, 0x000);
    set_palette_rgb(&mut palette,   3, 0x000, 0x0AA, 0x0AA);
    set_palette_rgb(&mut palette,   4, 0x0AA, 0x000, 0x000);
    set_palette_rgb(&mut palette,   5, 0x0AA, 0x000, 0x0AA);
    set_palette_rgb(&mut palette,   6, 0x0AA, 0x055, 0x000);
    set_palette_rgb(&mut palette,   7, 0x0AA, 0x0AA, 0x0AA);
    set_palette_rgb(&mut palette,   8, 0x055, 0x055, 0x055);
    set_palette_rgb(&mut palette,   9, 0x055, 0x055, 0x0FF);
    set_palette_rgb(&mut palette,  10, 0x055, 0x0FF, 0x055);
    set_palette_rgb(&mut palette,  11, 0x055, 0x0FF, 0x0FF);
    set_palette_rgb(&mut palette,  12, 0x0FF, 0x055, 0x055);
    set_palette_rgb(&mut palette,  13, 0x0FF, 0x055, 0x0FF);
    set_palette_rgb(&mut palette,  14, 0x0FF, 0x0FF, 0x055);
    set_palette_rgb(&mut palette,  15, 0x0FF, 0x0FF, 0x0FF);
    for n in 16..256 {
        let col1: usize = (n % 16) * 3;
        let col2: usize = (n / 16) * 3;
        let r = blend_colors(palette[col1 + 0], palette[col2 + 0]);
        let g = blend_colors(palette[col1 + 1], palette[col2 + 1]);
        let b = blend_colors(palette[col1 + 2], palette[col2 + 2]);
        set_palette_rgb(&mut palette, n, r, g, b);
    }

    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, width, height, &palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, width: u16, height: u16, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(width, height, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

pub struct View {
}

impl View {
    pub fn new() -> Self {
        View{ }
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), ViewError> {
        let view = ViewHeader::unpack_from_slice(&data[0..18]).unwrap();

        for n in 0..(view.num_loops as u16) {
            let loop_offset = ((n * view.loop_header_size as u16) + view.header_size + 2) as usize;
            let lop = LoopHeader::unpack_from_slice(&data[loop_offset..loop_offset+16]).unwrap();
            for m in 0..(lop.num_cels as u32) {
                let cel_offset = ((m * view.cel_header_size as u32) + lop.cel_offset) as usize;
                let cel = CelHeader::unpack_from_slice(&data[cel_offset..cel_offset+32]).unwrap();
                let _cel2 = CelHeader2::unpack_from_slice(&data[cel_offset+32..cel_offset+36]).unwrap();

                let mut visual = vec![ 0u8; (cel.x_dim * cel.y_dim) as usize ];

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
                        // TODO remapping
                        visual[y * cel.x_dim as usize + x] = line[x];
                    }
                }

                let mut visual_gif = create_gif(format!("/tmp/{}_{}.gif", n, m).as_str(), cel.x_dim, cel.y_dim);
                store_gif_bitmap(&mut visual_gif, cel.x_dim, cel.y_dim, &visual);
            }

        }
        Ok(())
    }
}

fn main() -> Result<(), ViewError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} path/view.nnn", args[0]);
    }
    let view_path = &args[1];
    let view_data = std::fs::read(view_path)?;

    let mut view = View::new();
    if let Err(x) = view.load(&view_data) {
        println!("load error: {:?}", x);
    }

    Ok(())
}
