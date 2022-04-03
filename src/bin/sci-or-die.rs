extern crate scitools;

use scitools::{palette, picture, view, script};
use std::env;
use std::collections::HashMap;
use std::rc::Rc;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::rect::Point;
use sdl2::pixels;
use sdl2::render::Canvas;
use sdl2::video::Window;
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

fn get_palette_rgb(ega_palette: &[u8], color: u8) -> pixels::Color {
    let color = color as usize;
    let r = ega_palette[3 * color + 0];
    let g = ega_palette[3 * color + 1];
    let b = ega_palette[3 * color + 2];
    pixels::Color::RGB(r, g, b)
}

fn put_pixel(canvas: &mut Canvas<Window>, x: i32, y: i32, color: pixels::Color) {
    canvas.set_draw_color(color);
    canvas.draw_point(Point::new(x, y)).expect("unable to draw point");
}

fn render_pic(canvas: &mut Canvas<Window>, pic: &picture::Picture, ega_palette: &[u8])
{
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let color = pic.visual[(y * SCREEN_WIDTH + x) as usize];
            let color = get_palette_rgb(&ega_palette, color);
            put_pixel(canvas, x, y, color);
        }
    }
}

fn render_view(canvas: &mut Canvas<Window>, view: &view::View, ega_palette: &[u8], group: usize, image: usize, base_x: i32, base_y: i32)
{
    let image = &view.groups[group].images[image];
    let x_mod: i32 = image.x_place_mod.into();
    let y_mod: i32 = image.y_place_mod.into();
    for y in 0..image.height as i32 {
        for x in 0..image.width as i32 {
            let color = image.visual[((y * image.width as i32) + x as i32) as usize];
            if color == image.color_key { continue; }
            let color = get_palette_rgb(&ega_palette, color);
            put_pixel(canvas, base_x + x_mod + x, base_y + y_mod + y, color);
        }
    }
}

#[derive(PartialEq,Eq,Hash,Clone,Copy)]
enum ResourceType {
    View = 0,
    Picture = 1,
    Script = 2,
    Text = 3,
    Sound = 4,
    Memory = 5,
    Vocab = 6,
    Font = 7,
    Cursor = 8,
    Patch = 9,
    Bitmap = 10,
    Palette = 11,
    Wave = 12,
    Audio = 13,
    Sync = 14,
    Msg = 15,
    Map = 16,
    Heap = 17,
    Audio36 = 18,
    Sync36 = 19,
    Xlate = 20,
}

fn resource_type_to_str(id: ResourceType) -> &'static str {
    match id {
        ResourceType::View => "view",
        ResourceType::Picture => "pic",
        ResourceType::Script => "script",
        ResourceType::Text => "text",
        ResourceType::Sound => "sound",
        ResourceType::Memory => "memory",
        ResourceType::Vocab => "vocab",
        ResourceType::Font => "font",
        ResourceType::Cursor => "cursor",
        ResourceType::Patch => "patch",
        ResourceType::Bitmap => "bitmap",
        ResourceType::Palette => "palette",
        ResourceType::Wave => "wave",
        ResourceType::Audio => "audio",
        ResourceType::Sync => "sync",
        ResourceType::Msg => "msg",
        ResourceType::Map => "map",
        ResourceType::Heap => "heap",
        ResourceType::Audio36 => "audio36",
        ResourceType::Sync36 => "sync36",
        ResourceType::Xlate => "xlate",
    }
}

#[derive(PartialEq,Eq,Hash,Clone,Copy)]
struct ResourceID {
    pub rtype: ResourceType,
    pub num: u16,
}

struct Resource {
    pub id: ResourceID,
    pub data: Vec<u8>,
}

struct ResourceManager {
    path: String,
    cache: HashMap<ResourceID, Rc<Resource>>,
}

impl ResourceManager {
    pub fn new(path: String) -> Self {
        ResourceManager{ path, cache: HashMap::new() }
    }

    pub fn get(&mut self, rtype: ResourceType, num: u16) -> Rc<Resource> {
        let id = ResourceID{ rtype, num };
        let resource_path = &self.path;
        let entry = self.cache.entry(id).or_insert_with(|| {
            let path = format!("{}/{}.{:03}", resource_path, resource_type_to_str(id.rtype), id.num);
            let data = match std::fs::read(path) {
                Ok(data) => data,
                Err(_) => Vec::new()
            };
            Rc::new(Resource{ id, data })
        });
        entry.clone()
    }
}

const SCALE: f32 = 2.0;
const SCREEN_WIDTH: i32 = 320;
const SCREEN_HEIGHT: i32 = 200;

fn main() -> Result<(), SciError> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(SciError::UsageError(format!("usage: {} path", args[0])));
    }
    let mut resource_manager = ResourceManager::new(args[1].clone());

    let script_res = resource_manager.get(ResourceType::Script, 0);
    let script = script::Script::new(0, &script_res.data)?;

    let pic_res = resource_manager.get(ResourceType::Picture, 300);
    let view_res = resource_manager.get(ResourceType::View, 0);

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
    pic.load_ega(&pic_res.data)?;

    let mut view = view::View::new();
    view.load(&view_res.data)?;

    let mut ega_palette = [ 0u8; 768 ];
    palette::fill_ega_colours(&mut ega_palette);

    let mut event_pump = sdl_context.event_pump().unwrap();
    let mut running = true;
    while running {
        render_pic(&mut canvas, &pic, &ega_palette);
        render_view(&mut canvas, &view, &ega_palette, 0, 0, 100, 100);

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
