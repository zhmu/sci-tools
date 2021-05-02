use crate::{object_class, script, vocab};

use std::collections::HashMap;

pub struct ClassDefinitions {
    definitions: HashMap<u16, object_class::ObjectClass>,
}

impl ClassDefinitions {
    pub fn new(path: String, class_vocab: &vocab::Vocab996) -> Self {
        let mut definitions: HashMap<u16, object_class::ObjectClass> = HashMap::new();
        for class_id in 0..class_vocab.get_number_of_classes() {
            let script_id = class_vocab.get_script(class_id as u16);
            if script_id.is_none() { continue; }
            let script_id = script_id.unwrap();

            let script_data = std::fs::read(format!("{}/script.{:03}", path, script_id));
            if script_data.is_err() { continue; }
            let script_data = script_data.unwrap();
            let script = script::Script::new(script_id as i16, &script_data);
            if script.is_err() { continue; }
            let script = script.unwrap();

            for block in &script.blocks {
                match block.r#type {
                    script::BlockType::Class => {
                        let object_class = object_class::ObjectClass::new(&script, &block, true);
                        if let Ok(object_class) = object_class {
                            let species = object_class.get_species();
                            definitions.insert(species, object_class);
                        }
                    },
                    _ => { }
                }
            }
        }
        ClassDefinitions{ definitions }
    }

    pub fn find_class(&self, class_id: u16) -> Option<&object_class::ObjectClass> {
        self.definitions.get(&class_id)
    }
}

