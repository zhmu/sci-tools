use std::io::{Cursor, Seek, SeekFrom};
use std::convert::TryFrom;
use num_enum::TryFromPrimitive;
use byteorder::{LittleEndian, ReadBytesExt};
use std::str;

use crate::{disassemble, opcode};

#[derive(TryFromPrimitive,PartialEq,Debug)]
#[repr(u16)]
pub enum BlockType {
    Terminator = 0,
    Object = 1,
    Code = 2,
    Synonyms = 3,
    Said = 4,
    Strings = 5,
    Class = 6,
    Exports = 7,
    Pointers = 8,
    PreloadText = 9,
    LocalVars = 10
}

pub struct ScriptBlock<'a> {
    pub r#type: BlockType,
    pub base: usize,
    pub data: &'a [u8]
}

pub struct Script<'a> {
    pub id: i16,
    pub blocks: Vec<ScriptBlock<'a>>
}

impl<'a> Script<'a> {
    pub fn new(id: i16, input: &'a [u8]) -> Result<Script<'a>, std::io::Error> {
        let mut rdr = Cursor::new(&input);

        let mut blocks: Vec<ScriptBlock> = Vec::new();
        while (rdr.position() as usize) < input.len() {
            let block_type = rdr.read_u16::<LittleEndian>()?;
            let block_type = BlockType::try_from(block_type).unwrap();
            if block_type == BlockType::Terminator {
                break;
            }

            let mut block_size = (rdr.read_u16::<LittleEndian>()? - 4) as usize;
            let base = rdr.position() as usize;
            if base + block_size > input.len() {
                println!("warning: block size {} too large, truncating to {}", block_size, input.len() - base);
                block_size = input.len() - base
            }
            let block_data = &input[base..base + block_size];
            rdr.seek(SeekFrom::Current(block_size as i64))?;

            blocks.push(ScriptBlock{ r#type: block_type, base, data: block_data });
        }

        Ok(Script{ id, blocks })
    }

    pub fn get_string(&self, address: usize) -> Option<&str> {
        for block in &self.blocks {
            if block.r#type == BlockType::Strings {
                if address >= block.base && address < block.base + block.data.len() {
                    let data = get_string(&block.data[address - block.base..]);
                    return Some(data);
                }
            }
        }
        None
    }
}

pub fn get_string(data: &[u8]) -> &str {
    let nul_byte_end = data.iter()
        .position(|&c| c == b'\0')
        .unwrap_or(data.len());
    str::from_utf8(&data[0..nul_byte_end]).unwrap_or("<corrupt>")
}

// Note: always uses the first argument
pub fn relpos0_to_absolute_offset(ins: &disassemble::Instruction) -> u16
{
    let a_type = &ins.opcode.arg[0];
    let a_value: usize = ins.args[0].into();
    let offset: usize = ins.offset as usize + ins.bytes.len();
    match a_type {
        opcode::Arg::RelPos8 => {
            if (a_value & 0x80) != 0 {
                panic!("implement signed bits here");
            }
            let j_offset: usize = offset + a_value;
            j_offset as u16
        }
        opcode::Arg::RelPos16 => {
            let j_offset = (offset + a_value) & 0xffff;
            j_offset as u16
        }
        _ => { panic!("only to be called with relative positions"); }
    }
}

pub fn does_kcall_return_void(nr: u16) -> bool {
    // 0x1b and 0x31 conditionally return things
    return match nr {
        0x01 | 0x03 | 0x07 | 0x09 | 0x0b | 0x11 | 0x12 | 0x15 | 0x16 | 0x17 |
        0x18 | 0x19 | 0x1a | 0x1d | 0x1e | 0x20 | 0x22 | 0x23 | 0x26 | 0x28 |
        0x2a | 0x2c | 0x2e | 0x31 | 0x33 | 0x3b | 0x3c | 0x3d | 0x3f | 0x4f |
        0x50 | 0x53 | 0x54 | 0x57 | 0x6a | 0x71 => { true },
        _ => { false }
    }
}
