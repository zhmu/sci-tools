use crate::opcode;
use crate::script;
use std::io::Cursor;
use byteorder::{ReadBytesExt, LittleEndian};

pub type ArgType = u16;

pub struct Instruction<'a> {
    pub offset: usize,
    pub bytes: &'a [u8],
    pub opcode: &'static opcode::Opcode,
    pub args: Vec<ArgType>,
}

pub struct Disassembler<'a> {
    block: &'a script::ScriptBlock<'a>,
    rdr: Cursor<&'a [u8]>,
}

impl<'a> Disassembler<'a> {
    pub fn new(block: &'a script::ScriptBlock) -> Disassembler<'a> {
        let rdr = Cursor::new(block.data);
        Disassembler{ block, rdr }
    }
}

impl<'a> Iterator for Disassembler<'a> {
    type Item = Instruction<'a>;

    fn next(&mut self) -> Option<Instruction<'a>> {
        let offset = self.rdr.position() as usize;
        let opcode = self.rdr.read_u8();
        if opcode.is_err() { return None }

        let opcode = &opcode::OPCODES[opcode.unwrap() as usize];
        let mut args: Vec<ArgType> = Vec::new();
        for arg in opcode.arg {
            match arg {
                opcode::Arg::RelPos8 | opcode::Arg::Imm8 => {
                    let value = self.rdr.read_u8();
                    if value.is_err() { return None }
                    args.push(value.unwrap().into());
                },
                opcode::Arg::RelPos16 | opcode::Arg::Imm16 => {
                    let value = self.rdr.read_u16::<LittleEndian>();
                    if value.is_err() { return None }
                    args.push(value.unwrap());
                },
            }
        }
        let bytes = &self.rdr.get_ref()[offset..self.rdr.position() as usize];
        Some(Instruction{ offset: self.block.base + offset, bytes, opcode, args })
    }
}
