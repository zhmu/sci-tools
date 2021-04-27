use crate::script;

use byteorder::{LittleEndian, ReadBytesExt};
use std::io::Cursor;
use std::str;

const _SELECTOR_INDEX_SPECIES: usize = 0;
const _SELECTOR_INDEX_SUPERCLASS: usize = 1;
const _SELECTOR_INDEX_INFO: usize = 2;
const SELECTOR_INDEX_NAME: usize = 3;

#[derive(Debug)]
pub enum ObjectClassError{
    IoError(std::io::Error),
    Corrupt,
    TrailingData,
    StringPointerOutOfRange,
}

impl From<std::io::Error> for ObjectClassError {
    fn from(error: std::io::Error) -> Self {
       ObjectClassError::IoError(error)
    }
}

pub struct SelectorValue {
    pub selector: u16,
    pub selector_id: Option<u16> // only classes have these
}

pub struct FunctionValue {
    pub selector: u16,
    pub offset: u16
}

#[derive(Eq,PartialEq)]
pub enum ObjectClassType {
    Object,
    Class
}

pub struct ObjectClass<'a> {
    pub r#type: ObjectClassType,
    pub name: &'a str,
    pub properties: Vec<SelectorValue>,
    pub functions: Vec<FunctionValue>
}

impl<'a> ObjectClass<'a> {
    pub fn new(script: &'a script::Script, block: &'a script::ScriptBlock, is_class: bool) -> Result<ObjectClass<'a>, ObjectClassError> {
        let mut rdr = Cursor::new(&block.data);
        let block_magic = rdr.read_u16::<LittleEndian>()?;
        if block_magic != 0x1234 {
            println!("  corrupt object magic {:x}, skipping", block_magic);
            return Err(ObjectClassError::Corrupt)
        }
        let _local_var_offset = rdr.read_u16::<LittleEndian>()?;
        let _selector_list_offset = rdr.read_u16::<LittleEndian>()?;
        let number_vs = rdr.read_u16::<LittleEndian>()? as usize;
        let mut properties: Vec<SelectorValue> = Vec::with_capacity(number_vs);
        for _ in 0..number_vs {
            let selector = rdr.read_u16::<LittleEndian>()?;
            properties.push(SelectorValue{ selector, selector_id: None })
        }

        if is_class {
            // Selector IDs
            for n in 0..number_vs {
                let id = rdr.read_u16::<LittleEndian>()?;
                properties[n].selector_id = Some(id);
            }
        }

        let number_fs = rdr.read_u16::<LittleEndian>()? as usize;
        let mut functions: Vec<FunctionValue> = Vec::with_capacity(number_fs);
        // Function selectors
        for _ in 0..number_fs {
            let selector = rdr.read_u16::<LittleEndian>()?;
            functions.push(FunctionValue{ selector, offset: 0} );
        }
        let zero = rdr.read_u16::<LittleEndian>()?;
        if zero != 0 {
            println!("  corrupt object zero {:x}, skipping", zero);
            return Err(ObjectClassError::Corrupt)
        }

        // Function offsets
        for n in 0..number_fs {
            let offset = rdr.read_u16::<LittleEndian>()?;
            functions[n].offset = offset;
        }

        if rdr.position() != rdr.get_ref().len() as u64 {
            println!("  still unconsumed data (position {} != length {}), skipping", rdr.position(), rdr.get_ref().len());
            return Err(ObjectClassError::TrailingData)
        }

        let name_offset = properties[SELECTOR_INDEX_NAME].selector as usize;
        let name;
        if name_offset == 0 {
            name = "(nil)";
        } else {
            match script.get_string(name_offset) {
                Some(x) => { name = x },
                None => { return Err(ObjectClassError::StringPointerOutOfRange) }
            }
        }
        let oc_type;
        if is_class {
            oc_type = ObjectClassType::Class;
        } else {
            oc_type = ObjectClassType::Object;
        }
        Ok(ObjectClass{ name, r#type: oc_type, properties, functions })
    }
}
