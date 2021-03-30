use std::io;

#[derive(Debug)]
pub enum ResourceError {
    IoError(io::Error),
    EndOfStream,
}

pub struct Resource {
    data: Vec<u8>,
    position: usize
}

impl Resource {
    pub fn new(path: &str) -> Result<Resource, ResourceError> {
        let data = std::fs::read(path).map_err(ResourceError::IoError)?;
        Ok(Resource{ data, position: 0 })
    }

    pub fn end_of_resource(&self) -> bool {
        self.position >= self.data.len()
    }

    pub fn peek_byte(&mut self) -> Result<u8, ResourceError> {
        if self.end_of_resource() {
            Err(ResourceError::EndOfStream)
        } else {
            Ok(self.data[self.position])
        }
    }

    pub fn get_byte(&mut self) -> Result<u8, ResourceError> {
        let value = self.peek_byte()?;
        self.position += 1;
        Ok(value)
    }

    pub fn get_bytes(&mut self, len: usize) -> Result<&[u8], ResourceError> {
        if self.position + len > self.data.len() {
            Err(ResourceError::EndOfStream)
        } else {
            let slice = &self.data[self.position..self.position + len];
            self.position += len;
            Ok(slice)
        }
    }
}

pub mod stream;
pub mod decompress;
pub mod resource;
pub mod opcode;
pub mod script;
pub mod disassemble;
pub mod vocab;
pub mod said;
pub mod object_class;
pub mod graph_lib;
pub mod code;
pub mod reduce;
