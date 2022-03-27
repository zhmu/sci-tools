extern crate scitools;

use scitools::{palette, view};
use std::env;
use std::fs::File;
use gif::{Encoder};

fn create_gif(fname: &str, palette: &[ u8; 768 ], width: u16, height: u16) -> gif::Encoder<File> {
    let gif_file = File::create(fname).unwrap();
    let gif_encoder = Encoder::new(gif_file, width, height, palette);
    gif_encoder.unwrap()
}

fn store_gif_bitmap(encoder: &mut gif::Encoder<File>, width: u16, height: u16, bits: &[u8]) {
    let frame = gif::Frame::from_indexed_pixels(width, height, &bits, None);
    encoder.write_frame(&frame).unwrap();
}

fn main() -> Result<(), view::ViewError> {
    env_logger::init();

    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("usage: {} path/view.nnn", args[0]);
    }
    let view_path = &args[1];
    let view_data = std::fs::read(view_path)?;

    let mut view = view::View::new();
    if let Err(x) = view.load(&view_data) {
        println!("load error: {:?}", x);
    }

    let mut ega_palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut ega_palette);

    for (g, group) in view.groups.iter().enumerate() {
        for (n, image) in group.images.iter().enumerate() {
            let fname = format!("/tmp/view_{}_{}.gif", g, n);
            let mut visual_gif = create_gif(&fname, &ega_palette, image.width, image.height);
            store_gif_bitmap(&mut visual_gif, image.width, image.height, &image.visual);
        }
    }

    Ok(())
}
