extern crate scitools;

use scitools::{palette, picture};
use std::env;
use gif::{Encoder};
use std::fs::File;

fn create_gif(fname: &str, palette: &[ u8; 768 ]) -> gif::Encoder<File> {
    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), picture::PictureError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} path/pic.nnn", args[0]);
    }
    let pic_path = &args[1];
    let pic_data = std::fs::read(pic_path)?;

    let mut pic = picture::Picture::new();
    pic.clear();
    if let Err(x) = pic.load_ega(&pic_data) {
        println!("load error: {:?}", x);
    }

    let mut palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut palette);

    let mut visual_gif = create_gif("visual.gif", &palette);
    store_gif_bitmap(&mut visual_gif, &pic.visual);

    let mut priority_gif = create_gif("priority.gif", &palette);
    store_gif_bitmap(&mut priority_gif, &pic.priority);

    let mut control_gif = create_gif("control.gif", &palette);
    store_gif_bitmap(&mut control_gif, &pic.control);

    Ok(())
}
