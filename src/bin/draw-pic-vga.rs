extern crate log;

extern crate scitools;

use scitools::{palette, picture};
use std::env;
use std::fs::File;
use gif::Encoder;

fn create_gif(fname: &str, width: u16, height: u16, palette: &[ u8; 768 ]) -> gif::Encoder<File> {
    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, width, height, palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, width: u16, height: u16, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(width, height, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), picture::PictureError> {
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

    let mut pic = picture::Picture::new();
    if let Err(x) = pic.load_vga(&pic_data, !got_palette, &mut palette) {
        println!("load error: {:?}", x);
    }

    let mut control_gif = create_gif("/tmp/control.gif", picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut control_gif, picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &pic.control);
    let mut prio_gif = create_gif("/tmp/prio.gif", picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut prio_gif, picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &pic.priority);
    let mut vis_gif = create_gif("/tmp/vis.gif", picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &palette);
    store_gif_bitmap(&mut vis_gif, picture::SCREEN_WIDTH as u16, picture::SCREEN_HEIGHT as u16, &pic.visual);

    Ok(())
}
