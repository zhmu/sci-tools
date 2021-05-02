use crate::{object_class, script, vocab};

use std::collections::HashMap;

#[derive(Debug)]
pub enum ClassError {
    IoError(std::io::Error),
    ObjectClassError(object_class::ObjectClassError),
    NoSuchClass(u16),
}

impl From<std::io::Error> for ClassError {
    fn from(error: std::io::Error) -> Self {
       ClassError::IoError(error)
    }
}

impl From<object_class::ObjectClassError> for ClassError {
    fn from(error: object_class::ObjectClassError) -> Self {
       ClassError::ObjectClassError(error)
    }
}

pub struct ClassDefinitions<'a> {
    path: String,
    class_vocab: &'a vocab::Vocab996,
    definitions: HashMap<u16, object_class::ObjectClass>,
}

impl<'a> ClassDefinitions<'a> {
    pub fn new(path: String, class_vocab: &'a vocab::Vocab996) -> Self {
        ClassDefinitions{ path, definitions: HashMap::new(), class_vocab }
    }

    pub fn find_class(&mut self, class_id: u16) -> Result<&object_class::ObjectClass, ClassError> {
        if !self.definitions.contains_key(&class_id) {
            let script_id = self.class_vocab.get_script(class_id);
            if script_id.is_none() { return Err(ClassError::NoSuchClass(class_id)); }
            let script_id = script_id.unwrap();

            let script_data = std::fs::read(format!("{}/script.{:03}", self.path, script_id))?;
            let script = script::Script::new(script_id as i16, &script_data)?;

            for block in &script.blocks {
                match block.r#type {
                    script::BlockType::Class => {
                        let object_class = object_class::ObjectClass::new(&script, &block, true)?;
                        let species = object_class.get_species();
                        self.definitions.insert(species, object_class);
                    },
                    _ => { }
                }
            }
            assert!(self.definitions.contains_key(&class_id));
        }
        Ok(self.definitions.get(&class_id).unwrap())
    }
}

