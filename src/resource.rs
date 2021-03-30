use packed_struct::prelude::*;

use std::fs::File;
use std::io::{Read,Seek,SeekFrom};

pub struct ResourceEntry {
    pub r_type: u8,
    pub r_number: u16,
    pub r_filenum: u8,
    pub r_offset: u64
}

pub struct ResourceData {
    pub header: ResourceHeader,
    pub data: Vec<u8>
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ResourceHeader {
    pub id: u16,
    pub comp_size: u16,
    pub decomp_size: u16,
    pub comp_method: u16
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ResourceMapEntry {
    type_number: u16,
    filenum_offset: u32
}

pub fn get_resource_data(path: &str, entry: &ResourceEntry) -> Result<ResourceData, std::io::Error> {
    let res_fname = format!("{}/resource.{:03}", path, entry.r_filenum);

    let mut res_file = File::open(&res_fname).unwrap();

    res_file.seek(SeekFrom::Start(entry.r_offset))?;
    let mut r_hdr: [u8; 8] = [ 0; 8 ];
    res_file.read(&mut r_hdr)?;
    let mut header = ResourceHeader::unpack_from_slice(&r_hdr).unwrap();
    header.comp_size -= 4;

    let mut data: Vec<u8> = vec![ 0u8; header.comp_size as usize ];
    res_file.read(&mut data)?;
    Ok(ResourceData{ header, data })
}

pub fn read_resource_map(input: &[u8]) -> Vec<ResourceEntry> {
    let mut entries: Vec<ResourceEntry> = Vec::new();

    let mut position: usize = 0;
    while position + 6 <= input.len() {
        let entry = &input[position..position+6];
        position += 6;

        let rme = ResourceMapEntry::unpack_from_slice(&entry).unwrap();
        if rme.type_number == 0xffff && rme.filenum_offset == 0xffffffff {
            break
        }

        let entry = ResourceEntry{
            r_type: (rme.type_number >> 11) as u8,
            r_number: rme.type_number & 0x7ff,
            r_filenum: (rme.filenum_offset >> 26) as u8,
            r_offset: (rme.filenum_offset & 0x3ffffff) as u64
        };
        entries.push(entry);
    }
    entries
}
