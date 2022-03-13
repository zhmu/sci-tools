fn blend_colors(c1: u8, c2: u8) -> u8 {
    let c1 = c1 as f32;
    let c2 = c2 as f32;
    let t =
        (c1 / 255.0).powf(2.2 / 1.0) * 255.0 +
        (c2 / 255.0).powf(2.2 / 1.0) * 255.0;
    (0.5 + (0.5 * t / 255.0).powf(1.0 / 2.2) * 255.0) as u8
}

fn set_palette_rgb(palette: &mut [u8; 768], index: usize, r: u8, g: u8, b: u8)
{
    let index = index * 3;
    palette[index + 0] = r;
    palette[index + 1] = g;
    palette[index + 2] = b;
}

pub fn fill_ega_colours(palette: &mut [u8; 768]) {
    set_palette_rgb(palette,   0, 0x000, 0x000, 0x000);
    set_palette_rgb(palette,   1, 0x000, 0x000, 0x0AA);
    set_palette_rgb(palette,   2, 0x000, 0x0AA, 0x000);
    set_palette_rgb(palette,   3, 0x000, 0x0AA, 0x0AA);
    set_palette_rgb(palette,   4, 0x0AA, 0x000, 0x000);
    set_palette_rgb(palette,   5, 0x0AA, 0x000, 0x0AA);
    set_palette_rgb(palette,   6, 0x0AA, 0x055, 0x000);
    set_palette_rgb(palette,   7, 0x0AA, 0x0AA, 0x0AA);
    set_palette_rgb(palette,   8, 0x055, 0x055, 0x055);
    set_palette_rgb(palette,   9, 0x055, 0x055, 0x0FF);
    set_palette_rgb(palette,  10, 0x055, 0x0FF, 0x055);
    set_palette_rgb(palette,  11, 0x055, 0x0FF, 0x0FF);
    set_palette_rgb(palette,  12, 0x0FF, 0x055, 0x055);
    set_palette_rgb(palette,  13, 0x0FF, 0x055, 0x0FF);
    set_palette_rgb(palette,  14, 0x0FF, 0x0FF, 0x055);
    set_palette_rgb(palette,  15, 0x0FF, 0x0FF, 0x0FF);
    for n in 16..256 {
        let col1: usize = (n % 16) * 3;
        let col2: usize = (n / 16) * 3;
        let r = blend_colors(palette[col1 + 0], palette[col2 + 0]);
        let g = blend_colors(palette[col1 + 1], palette[col2 + 1]);
        let b = blend_colors(palette[col1 + 2], palette[col2 + 2]);
        set_palette_rgb(palette, n, r, g, b);
    }
}
