use byteorder::{LittleEndian, ReadBytesExt, ByteOrder};
use std::io::Cursor;
use std::str;

#[derive(Debug)]
pub enum VocabError {
    IoError(std::io::Error),
    VocabTriesToCopyFromNothing
}

impl From<std::io::Error> for VocabError {
    fn from(error: std::io::Error) -> Self {
       VocabError::IoError(error)
    }
}

pub struct Vocab997 {
    pub words: Vec<String>
}

impl Vocab997 {
    pub fn new(input: &[u8]) -> Result<Vocab997, std::io::Error> {
        let count = LittleEndian::read_u16(&input[0..=2]) as usize;

        let mut words: Vec<String> = Vec::with_capacity(count);
        for n in 0..count {
            let offset = LittleEndian::read_u16(&input[2 + n * 2..=4 + n * 2]) as usize;
            let length = LittleEndian::read_u16(&input[offset..=offset + 2]) as usize;
            let s = str::from_utf8(&input[offset+2..offset+2+length]).unwrap();
            words.push(s.to_string());
        }

        Ok(Vocab997{ words })
    }

    pub fn get_selector_name(&self, id: usize) -> &String {
        &self.words[id]
    }
}

pub struct Word {
    pub word: String,
    pub class: u16,
    pub group: u16
}

pub struct Vocab000 {
    pub words: Vec<Word>,
}

impl Vocab000 {
    pub fn new(input: &[u8]) -> Result<Vocab000, VocabError> {
        let mut rdr = Cursor::new(&input);
        for _ in 'A'..='Z' {
            let _offset = rdr.read_u16::<LittleEndian>()?;
        }

        let mut words: Vec<Word> = Vec::new();
        loop {
            let copy_amount = rdr.read_u8();
            if copy_amount.is_err() { break; }
            let copy_amount = copy_amount.unwrap() as usize;

            let mut cur_word: String;
            if let Some(prev_word) = &words.last() {
                cur_word = prev_word.word[0..copy_amount].to_string();
            } else {
                cur_word = "".to_string();
                if copy_amount != 0 {
                    return Err(VocabError::VocabTriesToCopyFromNothing);
                }
            }

            loop {
                let ch = rdr.read_u8()?;
                cur_word.push((ch & 0x7f) as char);
                if (ch & 0x80) != 0 { break; }
            }

            let id1 = rdr.read_u8()? as u32;
            let id2 = rdr.read_u8()? as u32;
            let id3 = rdr.read_u8()? as u32;
            let id = (id1 << 16) | (id2 << 8) | id3;

            let class = ((id >> 12) & 0xfff) as u16;
            let group = (id & 0xfff) as u16;

            words.push(Word{ word: cur_word, class, group });
        }

        Ok(Vocab000{ words })
    }

    pub fn get_words_by_group(&self, group: u16) -> Vec<&Word> {
        let mut result: Vec<&Word> = Vec::new();
        for word in &self.words {
            if word.group == group {
                result.push(word);
            }
        }
        result
    }

}

pub struct Vocab996 {
    classes: Vec<u16>
}

impl Vocab996 {
    pub fn new(input: &[u8]) -> Result<Vocab996, VocabError> {
        let mut classes: Vec<u16> = Vec::new();

        let mut rdr = Cursor::new(&input);
        loop {
            let must_be_zero = rdr.read_u16::<LittleEndian>();
            if must_be_zero .is_err() { break; }
            assert_eq!(0, must_be_zero.unwrap());
            let script = rdr.read_u16::<LittleEndian>()?;

            classes.push(script);
        }

        Ok(Vocab996{ classes })
    }

    pub fn get_script(&self, class_id: u16) -> Option<u16> {
        let class_id = class_id as usize;
        if class_id >= self.classes.len() {
            return None;
        }
        Some(self.classes[class_id])
    }
}
