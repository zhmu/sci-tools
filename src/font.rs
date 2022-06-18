use packed_struct::prelude::*;

#[derive(Debug)]
pub enum FontError {
    IoError(std::io::Error),
    FirstCharIsNotZero,
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

pub struct Glyph {
    height: u8,
    width: u8,
    offset: usize,
}

impl Glyph {
    fn render(&self, data: &[u8], plot: &mut dyn FnMut(u16, u16)) {
        let mut index = self.offset;
        for m in 0..self.height {
            let mut byte = data[index]; index += 1;
            let mut n: u16 = 0;
            let mut bl: u8 = 0;
            loop {
                //let dim_mask = 0xffu8; // style & 1 == 0, penY & 1 == 0
                let carry = (byte & 0x80) != 0;
                byte = byte << 1;
                if carry {
                    plot(n, m as u16);
                }
                bl += 1;
                n += 1;
                if bl == self.width { break; }
                if (bl & 7) == 0 {
                    byte = data[index];
                    index += 1;
                }
            }
        }
    }
}

pub struct Font {
    point_size: u16,
    glyphs: Vec<Glyph>,
    data: Vec<u8>,
}

impl Font {
    pub fn new() -> Self {
        Font { point_size: 0, glyphs: Vec::new(), data: Vec::new() }
    }

    pub fn load(&mut self, data: &[u8]) -> Result<(), FontError> {
        let header = FontHeader::unpack_from_slice(&data[0..6]).unwrap();
        // SCI does not properly account for non-zero first chars
        if header.low_char != 0 { return Err(FontError::FirstCharIsNotZero); }

        self.point_size = header.point_size;
        self.data = data.to_vec();
        for n in header.low_char..header.high_char {
            let offset: usize = (6 + n as u16 * 2).into();
            let mut offset: usize = (data[offset + 0] as u16 + (data[offset + 1] as u16) * 256).into();

            let width = data[offset]; offset += 1;
            let height = data[offset]; offset += 1;
            self.glyphs.push(Glyph{ height, width, offset });
        }
        Ok(())
    }

    pub fn get_height(&self) -> u16 {
        self.point_size
    }

    pub fn render(&self, s: &str, plot: &mut dyn FnMut(u16, u16)) {
        let mut char_x: u16 = 0;
        let mut char_y: u16 = 0;
        for ch in s.chars() {
            if ch == '\n' {
                char_x = 0;
                char_y += self.point_size;
                continue;
            }
            let mut glyph_plot = |x, y| {
                plot(char_x + x, char_y + y);
            };
            let glyph = &self.glyphs[ch as usize];
            glyph.render(&self.data, &mut glyph_plot);
            char_x += glyph.width as u16;
        }
    }

    pub fn get_number_of_chars(&self) -> usize {
        self.glyphs.len()
    }

    pub fn get_char_width(&self, ch: usize) -> u8 {
        self.glyphs[ch].width
    }
}
