use phf::phf_map;
use byteorder::ReadBytesExt;
use std::io::Cursor;

use crate::{script, vocab};

#[derive(Debug)]
pub enum SaidError {
    IoError(std::io::Error),
}

impl From<std::io::Error> for SaidError {
    fn from(error: std::io::Error) -> Self {
       SaidError::IoError(error)
    }
}
pub static SAID_OPERATOR_MAP: phf::Map<u8, char> = phf_map!{
    240u8 => ',',
    241u8 => '&',
    242u8 => '/',
    243u8 => '(',
    244u8 => ')',
    245u8 => '[',
    246u8 => ']',
    247u8 => '#',
    248u8 => '<',
    249u8 => '>',
};

pub struct SaidItem {
    pub offset: usize,
    pub said: String
}

pub struct Said {
    pub items: Vec<SaidItem>
}

impl Said {
    pub fn new(block: &script::ScriptBlock, vocab: &vocab::Vocab000) -> Result<Said, SaidError> {
        let mut rdr = Cursor::new(&block.data);
        let mut current_position: usize = 0;
        let mut current_said: String = "".to_string();

        let mut items: Vec<SaidItem> = Vec::new();
        loop {
            let token = rdr.read_u8();
            if token.is_err() { break; }
            let token = token.unwrap();
            if token == 0xff {
                items.push(SaidItem{ offset: block.base + current_position, said: current_said });
                current_said = "".to_string();
                current_position = rdr.position() as usize;
            } else if token >= 0xf0 {
                current_said += &format!("{} ", SAID_OPERATOR_MAP[&token]).to_string();
            } else {
                let byte = rdr.read_u8();
                if byte.is_err() { break; }
                let group = ((token as u16) << 8) | byte.unwrap() as u16;

                let words = vocab.get_words_by_group(group);
                current_said += "{";
                for w in words {
                    current_said += &format!(" {}", w.word);
                }
                current_said += " }";
            }
        }
        Ok(Said{ items })
    }
}
