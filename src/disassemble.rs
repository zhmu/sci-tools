use crate::opcode;
use crate::script;
use crate::stream;

pub type ArgType = u16;

fn get_u8(stream: &mut stream::Streamer) -> ArgType {
    stream.get_byte() as ArgType
}

fn get_u16(stream: &mut stream::Streamer) -> ArgType {
    let a = stream.get_byte() as ArgType;
    let b = stream.get_byte() as ArgType;
    (b << 8) + a
}

pub struct Instruction<'a> {
    pub offset: usize,
    pub bytes: &'a [u8],
    pub opcode: &'static opcode::Opcode,
    pub args: Vec<ArgType>,
}

pub struct Disassembler<'a> {
    block: &'a script::ScriptBlock<'a>,
    stream: stream::Streamer<'a>
}

impl<'a> Disassembler<'a> {
    pub fn new(block: &'a script::ScriptBlock, input_pos: usize) -> Disassembler<'a> {
        let stream = stream::Streamer::new(&block.data, input_pos);
        Disassembler{ block, stream }
    }
}

impl<'a> Iterator for Disassembler<'a> {
    type Item = Instruction<'a>;

    fn next(&mut self) -> Option<Instruction<'a>> {
        if self.stream.end_of_stream() {
            return None
        }

        let offset = self.stream.get_offset();
        let opcode = &opcode::OPCODES[self.stream.get_byte() as usize];
        let mut args: Vec<ArgType> = Vec::new();
        for arg in opcode.arg {
            match arg {
                opcode::Arg::RelPos8 | opcode::Arg::Imm8 => {
                    args.push(get_u8(&mut self.stream));
                },
                opcode::Arg::RelPos16 | opcode::Arg::Imm16 => {
                    args.push(get_u16(&mut self.stream));
                },
            }
        }
        let bytes = &self.block.data[offset..self.stream.get_offset()];
        Some(Instruction{ offset: self.block.base + offset, bytes, opcode, args })
    }
}
