use sdl2::pixels;
use sdl2::rect::Point;
use sdl2::render::Canvas;
use sdl2::video::Window;

use scitools::{picture, view};

pub const SCALE: f32 = 2.0;
pub const SCREEN_WIDTH: i32 = 320;
pub const SCREEN_HEIGHT: i32 = 200;

fn get_palette_rgb(ega_palette: &[u8], color: u8) -> pixels::Color {
    let color = color as usize;
    let r = ega_palette[3 * color + 0];
    let g = ega_palette[3 * color + 1];
    let b = ega_palette[3 * color + 2];
    pixels::Color::RGB(r, g, b)
}

pub fn put_pixel(canvas: &mut Canvas<Window>, x: i32, y: i32, color: pixels::Color) {
    canvas.set_draw_color(color);
    canvas.draw_point(Point::new(x, y)).expect("unable to draw point");
}

pub fn render_pic(canvas: &mut Canvas<Window>, pic: &picture::Picture, ega_palette: &[u8])
{
    for y in 0..SCREEN_HEIGHT {
        for x in 0..SCREEN_WIDTH {
            let color = pic.visual[(y * SCREEN_WIDTH + x) as usize];
            let color = get_palette_rgb(&ega_palette, color);
            put_pixel(canvas, x, y, color);
        }
    }
}

pub fn render_view(canvas: &mut Canvas<Window>, view: &view::View, ega_palette: &[u8], group: usize, image: usize, base_x: i32, base_y: i32)
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
