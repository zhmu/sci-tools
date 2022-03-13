extern crate scitools;

use std::env;
use byteorder::{LittleEndian, ReadBytesExt};
use std::io::{Cursor, Seek, SeekFrom};
use std::fs::File;
use gif::{Encoder};

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
        let mut rdr = Cursor::new(&data);
        let num_image_groups = rdr.read_u16::<LittleEndian>()?;
        let mirrored_flags = rdr.read_u16::<LittleEndian>()?;
        rdr.seek(SeekFrom::Current(4))?; // skip
        let mut cell_list_indices = vec![ 0u16; num_image_groups as usize ];
        for n in 0..(num_image_groups as usize) {
            cell_list_indices[n] = rdr.read_u16::<LittleEndian>()?;
        }

        for n in 0..num_image_groups {
            let offset = cell_list_indices[n as usize];
            rdr.seek(SeekFrom::Start(offset.into()))?;

            let num_image_cells = rdr.read_u16::<LittleEndian>()?;
            rdr.seek(SeekFrom::Current(2))?; // skip
            let mut image_cell_indices = vec![ 0u16; num_image_cells as usize ];

            for n in 0..(num_image_cells as usize) {
                image_cell_indices[n] = rdr.read_u16::<LittleEndian>()?;
            }

            for m in 0..num_image_cells {
                let offset = image_cell_indices[m as usize];
                rdr.seek(SeekFrom::Start(offset.into()))?;

                let x_size = rdr.read_u16::<LittleEndian>()?;
                let y_size = rdr.read_u16::<LittleEndian>()?;
                let _x_place_mod = rdr.read_u8()?;
                let _y_place_mod = rdr.read_u8()?;
                let _color_key  = rdr.read_u8()?; // transparency value

                let mut visual = vec![ 0u8; (x_size * y_size) as usize ];

                let mut x: u16 = 0;
                let mut y: u16 = 0;
                while y < y_size {
                    let byte = rdr.read_u8()?;
                    let color = byte & 0xf;
                    let repeat = byte >> 4;
                    for _ in 0..repeat {
                        visual[(y * x_size + x) as usize] = color;
                        x += 1;
                        if x >= x_size {
                            x = 0;
                            y += 1;
                        }
                    }
                }

                let mut visual_gif = create_gif(format!("/tmp/{}_{}.gif", n, m).as_str(), x_size, y_size);
                store_gif_bitmap(&mut visual_gif, x_size, y_size, &visual);
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
