extern crate scitools;

use std::env;
use scitools::{palette, font};
use gif::{Encoder};
use std::fs::File;

fn create_gif(fname: &str, width: u16, height: u16, palette: &[ u8; 768 ]) -> gif::Encoder<File> {
    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, width, height, palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, width: u16, height: u16, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(width, height, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), font::FontError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} data/font.nnn", args[0]);
    }
    let font_path = &args[1];
    let font_data = std::fs::read(font_path)?;

    let mut font = font::Font::new();
    if let Err(x) = font.load(&font_data) {
        println!("load error: {:?}", x);
    }

    let mut palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut palette);

    let width: u16 = 256;
    let height: u16 = 128;
    let mut visual = vec![ 0u8; (width * height) as usize ];
    let mut base_x: u16 = 0;
    let mut base_y: u16 = 0;
    let color = 4;
    for n in 0..(font.get_number_of_chars() as u8) {
        let s = (n as char).to_string();
        let char_width = font.get_char_width(n.into()) as u16;
        if base_x + char_width >= width {
            base_x = 0;
            base_y += font.get_height();
        }
        font.render(&s, &mut |x, y| {
            visual[((base_y + y) * width + base_x + x) as usize] = color;
        });
        base_x += char_width;
    }

    let mut gif = create_gif("/tmp/font.gif", width, height, &palette);
    store_gif_bitmap(&mut gif, width, height, &visual);

    Ok(())
}
