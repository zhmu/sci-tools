extern crate scitools;

mod render;
mod resman;

use scitools::{palette, picture, view, script};
use scitools::restype::*;
use std::env;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use std::time::Duration;

#[derive(Debug)]
pub enum SciError {
    UsageError(String),
    SdlError(String),
    IoError(std::io::Error),
    PictureError(picture::PictureError),
    ViewError(view::ViewError),
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

impl From<view::ViewError> for SciError {
    fn from(error: view::ViewError) -> Self {
       SciError::ViewError(error)
    }
}

fn main() -> Result<(), SciError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(SciError::UsageError(format!("usage: {} path", args[0])));
    }
    let mut resource_manager = resman::ResourceManager::new(args[1].clone());

    let script_res = resource_manager.get(ResourceType::Script, 0);
    let script = script::Script::new(0, &script_res.data)?;

    let pic_res = resource_manager.get(ResourceType::Picture, 300);
    let view_res = resource_manager.get(ResourceType::View, 0);

    let sdl_context = sdl2::init().map_err(|e| SciError::SdlError(e.to_string()))?;
    let video_subsystem = sdl_context.video().unwrap();
    let window = video_subsystem.window("SCI or die", (render::SCREEN_WIDTH as f32 * render::SCALE) as u32, (render::SCREEN_HEIGHT as f32 * render::SCALE) as u32)
        .build()
        .map_err(|e| SciError::SdlError(e.to_string()))?;
    let mut canvas = window
        .into_canvas()
        .accelerated()
        .build()
        .map_err(|e| SciError::SdlError(e.to_string()))?;
    canvas.set_scale(render::SCALE, render::SCALE).expect("cannot set scale");

    let mut pic = picture::Picture::new();
    pic.clear();
    pic.load_ega(&pic_res.data)?;

    let mut view = view::View::new();
    view.load(&view_res.data)?;

    let mut ega_palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut ega_palette);

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut running = true;
    while running {
        render::render_pic(&mut canvas, &pic, &ega_palette);
        render::render_view(&mut canvas, &view, &ega_palette, 0, 0, 100, 100);

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
