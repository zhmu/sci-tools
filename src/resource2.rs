use packed_struct::prelude::*;

use std::fs::File;
use std::io::{Read,Seek,SeekFrom};

pub struct ResourceEntry {
    pub r_type: u8,
    pub r_number: u16,
    pub r_offset: u64
}

pub struct ResourceData {
    pub header: ResourceHeader,
    pub data: Vec<u8>
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ResourceDirectoryHeader {
    type_number: u8,
    offset: u16
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ResourceMapEntry {
    pub id: u16,
    pub woffset1: u8,
    pub woffset2: u8,
    pub woffset3: u8,
}

#[derive(PackedStruct)]
#[packed_struct(endian="lsb")]
pub struct ResourceHeader {
    pub res_type: u8,
    pub id: u16,
    pub segment_length: u16,
    pub length: u16,
    pub compress_used: u16
}

pub fn get_resource_data(path: &str, entry: &ResourceEntry) -> Result<ResourceData, std::io::Error> {
    let res_fname = format!("{}/resource.{:03}", path, 0);

    let mut res_file = File::open(&res_fname).unwrap();

    res_file.seek(SeekFrom::Start(entry.r_offset))?;
    let mut r_hdr: [u8; 9] = [ 0; 9 ];
    res_file.read(&mut r_hdr)?;
    let header = ResourceHeader::unpack_from_slice(&r_hdr).unwrap();

    let mut data: Vec<u8> = vec![ 0u8; header.segment_length as usize ];
    res_file.read(&mut data)?;
    Ok(ResourceData{ header, data })
}

pub fn read_resource_map(input: &[u8]) -> Vec<ResourceEntry> {
    let mut res_types: Vec<ResourceDirectoryHeader> = Vec::new();
    let mut position: usize = 0;
    loop {
        let entry = &input[position..position + 3];
        position += 3;

        let rhe = ResourceDirectoryHeader::unpack_from_slice(&entry).unwrap();
        let last_one = rhe.type_number == 255;
        println!("dir: type {} offset {}", rhe.type_number, rhe.offset);
        res_types.push(rhe);
        if last_one { break; }
    }

    let mut entries: Vec<ResourceEntry> = Vec::new();
    for n in 0..res_types.len() - 1 {
        let type_number = res_types[n].type_number;

        position = res_types[n].offset as usize;
        let position_end = res_types[n + 1].offset as usize;
        while position < position_end {
            let rme = ResourceMapEntry::unpack_from_slice(&input[position..position + 5]).unwrap();
            position += 5;

            let offset: u32 = ((rme.woffset1 as u32) << 1) + ((rme.woffset2 as u32) << 9) + ((rme.woffset3 as u32) << 17);
            //println!("type {} id {} offset {}", type_number, rme.id, offset);
            let entry = ResourceEntry{
                r_type: type_number,
                r_number: rme.id,
                r_offset: offset as u64
            };
            entries.push(entry);
        }
    }
    entries
}
