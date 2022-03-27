extern crate scitools;

use scitools::{palette, picture};
use std::env;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::rect::Point;
use sdl2::pixels;
use std::time::Duration;
use sdl2::render::Canvas;
use sdl2::video::Window;

#[derive(Debug)]
pub enum SciError {
    UsageError(String),
    SdlError(String),
    IoError(std::io::Error),
    PictureError(scitools::picture::PictureError),
}

impl From<std::io::Error> for SciError {
    fn from(error: std::io::Error) -> Self {
       SciError::IoError(error)
    }
}

impl From<picture::PictureError> for SciError {
    fn from(error: picture::PictureError) -> Self {
       SciError::PictureError(error)
    }
}

fn render_pic(canvas: &mut Canvas<Window>, pic: &picture::Picture, ega_palette: &[u8])
{
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let color = pic.visual[(y * SCREEN_WIDTH + x) as usize] as usize;
            let r = ega_palette[3 * color + 0];
            let g = ega_palette[3 * color + 1];
            let b = ega_palette[3 * color + 2];
            canvas.set_draw_color(pixels::Color::RGB(r, g, b));
            canvas.draw_point(Point::new(x as i32, y as i32)).expect("unable to draw point");
        }
    }
}

const SCALE: f32 = 2.0;
const SCREEN_WIDTH: u32 = 320;
const SCREEN_HEIGHT: u32 = 200;

fn main() -> Result<(), SciError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(SciError::UsageError(format!("usage: {} path/pic.nnn", args[0])));
    }
    let pic_path = &args[1];
    let pic_data = std::fs::read(pic_path)?;

    let sdl_context = sdl2::init().map_err(|e| SciError::SdlError(e.to_string()))?;
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem.window("SCI or die", (SCREEN_WIDTH as f32 * SCALE) as u32, (SCREEN_HEIGHT as f32 * SCALE) as u32)
        .build()
        .map_err(|e| SciError::SdlError(e.to_string()))?;
    let mut canvas = window
        .into_canvas()
        .accelerated()
        .build()
        .map_err(|e| SciError::SdlError(e.to_string()))?;
    canvas.set_scale(SCALE, SCALE).expect("cannot set scale");

    let mut pic = picture::Picture::new();
    pic.clear();
    pic.load_ega(&pic_data)?;

    let mut ega_palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut ega_palette);

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut running = true;
    while running {
        render_pic(&mut canvas, &pic, &ega_palette);

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    running = false;
                },
                _ => {}
            }
        }

        canvas.present();
        std::thread::sleep(Duration::from_millis(100));
    }
    Ok(())
}
