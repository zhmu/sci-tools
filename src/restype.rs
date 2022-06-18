use num_enum::TryFromPrimitive;
use std::convert::TryFrom;

#[derive(TryFromPrimitive)]
#[derive(PartialEq,Eq,Hash,Clone,Copy)]
#[repr(u8)]
pub enum ResourceType {
    View = 0,
    Picture = 1,
    Script = 2,
    Text = 3,
    Sound = 4,
    Memory = 5,
    Vocab = 6,
    Font = 7,
    Cursor = 8,
    Patch = 9,
    Bitmap = 10,
    Palette = 11,
    Wave = 12,
    Audio = 13,
    Sync = 14,
    Msg = 15,
    Map = 16,
    Heap = 17,
    Audio36 = 18,
    Sync36 = 19,
    Xlate = 20,
}

#[derive(PartialEq,Eq,Hash,Clone,Copy)]
pub struct ResourceID {
    pub rtype: ResourceType,
    pub num: u16,
}

pub fn resource_type_to_str(id: ResourceType) -> &'static str {
    match id {
        ResourceType::View => "view",
        ResourceType::Picture => "pic",
        ResourceType::Script => "script",
        ResourceType::Text => "text",
        ResourceType::Sound => "sound",
        ResourceType::Memory => "memory",
        ResourceType::Vocab => "vocab",
        ResourceType::Font => "font",
        ResourceType::Cursor => "cursor",
        ResourceType::Patch => "patch",
        ResourceType::Bitmap => "bitmap",
        ResourceType::Palette => "palette",
        ResourceType::Wave => "wave",
        ResourceType::Audio => "audio",
        ResourceType::Sync => "sync",
        ResourceType::Msg => "msg",
        ResourceType::Map => "map",
        ResourceType::Heap => "heap",
        ResourceType::Audio36 => "audio36",
        ResourceType::Sync36 => "sync36",
        ResourceType::Xlate => "xlate",
    }
}

pub fn u8_to_resource_type(rtype: u8) -> Option<ResourceType> {
    ResourceType::try_from(rtype).ok()
}
