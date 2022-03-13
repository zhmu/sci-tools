extern crate scitools;

use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use phf::phf_map;
use std::fs::File;
use std::io::Write;
use explode::explode;

use scitools::resource2;
use scitools::decompress;

#[derive(TryFromPrimitive)]
#[repr(u16)]
enum CompressionMethod {
    None = 0,
    LZW = 1,
    Huffman = 2
}

static RESOURCE_TYPE_MAP: phf::Map<u8, &'static str> = phf_map!{
    0u8 => "view",
    1u8 => "pic",
    2u8 => "script",
    3u8 => "text",
    4u8 => "sound",
    5u8 => "memory",
    6u8 => "vocab",
    7u8 => "font",
    8u8 => "cursor",
    9u8 => "patch",
    10u8 => "bitmap",
    11u8 => "palette",
    12u8 => "wave",
    13u8 => "audio",
    14u8 => "sync",
    15u8 => "msg",
    16u8 => "map",
    17u8 => "heap",
    18u8 => "audio36",
    19u8 => "sync36",
    20u8 => "xlate",
};

fn main() -> Result<(), std::io::Error> {
    let data_path = "qfg3-data";
    let out_path = "qfg3-out";

    let resource_map_data = std::fs::read(format!("{}/resource.map", data_path))?;

    let resources = resource2::read_resource_map(&resource_map_data);
    for entry in resources.iter() {
        let resource = resource2::get_resource_data(data_path, &entry)?;
        let rh = &resource.header;
        let res_type = rh.res_type & 0x7f;
        println!("  resource: {}.{:03} segment_length {} length {} compress_used {}",
            RESOURCE_TYPE_MAP[&res_type], rh.id, rh.segment_length, rh.length, rh.compress_used);

        let mut output: Vec<u8> = Vec::new();
        if rh.compress_used != 0 {
            match explode::explode(&resource.data) {
                Ok(data) => {
                    println!("    => Explode: {} bytes decompressed", data.len());
                    output = data;
                },
                Err(err) => {
                    println!("    !! could not decompress ({}), skipping", err);
                },
            }
        } else {
            output = resource.data;
        }

        if !output.is_empty() {
            let out_fname = format!("{}/{}.{:03}", out_path, RESOURCE_TYPE_MAP[&res_type], entry.r_number);
            let mut r_file = File::create(out_fname)?;
            r_file.write_all(&output)?;
        }
    }
    Ok(())
}
