extern crate scitools;

use packed_struct::prelude::*;
use std::env;
use bmp::{Image, Pixel};

#[derive(Debug)]
pub enum FontError {
    IoError(std::io::Error),
}

impl From<std::io::Error> for FontError {
    fn from(error: std::io::Error) -> Self {
       FontError::IoError(error)
    }
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct FontHeader {
    pub low_char: u16,
    pub high_char: u16,
    pub point_size: u16,
}

pub struct Font {
}

impl Font {
    pub fn new() -> Self {
        Font { }
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), FontError> {
        //let res = stream::Streamer::new(data, 0);

        let mut img = Image::new(256, 256);
        for (x, y) in img.coordinates() { img.set_pixel(x, y, bmp::consts::BLACK); }
        let mut x: u16 = 0;
        let mut y: u16 = 0;

        let header = FontHeader::unpack_from_slice(&data[0..6]).unwrap();
        for n in header.low_char..header.high_char {
            let offset: usize = (6 + n as u16 * 2).into();
            let mut index: usize = (data[offset + 0] as u16 + (data[offset + 1] as u16) * 256).into();

            let c_wide = data[index]; index += 1;
            let c_high = data[index]; index += 1;
            println!("n {} c_high {} c_wide {}", n, c_high, c_wide);

            if x + header.point_size > img.get_width() as u16 {
                x = 0;
                y += header.point_size;
            }

            for m in 0..c_high {
                let mut byte = data[index]; index += 1;
                let mut n: u16 = 0;
                let mut bl: u8 = 0;
                loop {
                    //let dim_mask = 0xffu8; // style & 1 == 0, penY & 1 == 0
                    let carry = (byte & 0x80) != 0;
                    byte = byte << 1;
                    if carry {
                        img.set_pixel((x + n as u16) as u32, (y + m as u16) as u32, bmp::consts::RED);
                    } else {
                        img.set_pixel((x + n as u16) as u32, (y + m as u16) as u32, bmp::consts::BLUE);
                    }
                    bl += 1;
                    n += 1;
                    if bl == c_wide { break; }
                    if (bl & 7) == 0 {
                        byte = data[index];
                        index += 1;
                    }
                }
            }
            x += header.point_size;
        }

        img.save("/tmp/f.bmp")?;
        Ok(())
    }
}

fn main() -> Result<(), FontError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} data/font.nnn", args[0]);
    }
    let font_path = &args[1];
    let font_data = std::fs::read(font_path)?;

    let mut font = Font::new();
    if let Err(x) = font.load(&font_data) {
        println!("load error: {:?}", x);
    }

    Ok(())
}
