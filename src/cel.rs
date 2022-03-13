use packed_struct::prelude::*;

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct CelHeader {
    pub x_dim: u16,
    pub y_dim: u16,
    pub x_off: u16,
    pub y_off: u16,
    pub skip_color: u8,
    pub compress_type: u8,
    pub comp_remap_count: u16,
    pub compress_size: u32,
    pub control_size: u32,
    pub palette_offset: u32,
    pub data_offset: u32,
    pub color_offset: u32,
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct CelHeader2 {
    pub compress_remap_offset: u32,
}

pub struct Cel {
    pub width: u16,
    pub height: u16,
    pub visual: Vec<u8>
}

impl Cel {
    pub fn new() -> Self {
        Cel{ width: 0, height: 0, visual: vec![ ] }
    }

    pub fn load(&mut self, data: &[u8], cel_offset: usize)
    {
        let cel = CelHeader::unpack_from_slice(&data[cel_offset..cel_offset+32]).unwrap();
        let _cel2 = CelHeader2::unpack_from_slice(&data[cel_offset+32..cel_offset+36]).unwrap();
        self.width = cel.x_dim;
        self.height = cel.y_dim;

        let mut color_offset = cel.color_offset as usize;
        let mut data_offset = cel.data_offset as usize;

        let mut line = vec![ 0u8; 320 ];

        self.visual = vec! [ 0u8; (cel.x_dim * cel.y_dim) as usize ];
        for y in 0..(cel.y_dim as usize) {
            // Decompress line
            let mut line_pos: usize = 0;
            while line_pos < (cel.x_dim as usize) {
                let control = data[data_offset];
                data_offset += 1;
                if (control & 0x80) == 0 {
                    for _ in 0..control {
                        let color = data[color_offset];
                        color_offset += 1;
                        line[line_pos] = color;
                        line_pos += 1;
                    }
                } else /* (control & 0x80) != 0 */ {
                    let color;
                    if (control & 0x40) == 0 {
                        color = data[color_offset];
                        color_offset += 1;
                    } else /* (control & 0x40) != 0 */ {
                        color = cel.skip_color;
                    }
                    for _ in 0..(control & 0x3f) {
                        line[line_pos] = color;
                        line_pos += 1;
                    }
                }
            }

            // Copy line
            for x in 0..(cel.x_dim as usize) {
                // TODO remapping of palette
                self.visual[(y * cel.x_dim as usize) as usize + x] = line[x];
            }
        }
    }
}
