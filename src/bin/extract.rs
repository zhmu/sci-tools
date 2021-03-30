extern crate scitools;

use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use phf::phf_map;
use std::fs::File;
use std::io::Write;

use scitools::resource;
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
    12u8 => "cdaudio"
};

fn main() -> Result<(), std::io::Error> {
    let data_path = "qfg1-data";
    let out_path = "out";

    let resource_map_data = std::fs::read(format!("{}/resource.map", data_path))?;

    let resources = resource::read_resource_map(&resource_map_data);
    for entry in resources.iter() {
        println!("{}.{:03} filenum {} offset {}", RESOURCE_TYPE_MAP[&entry.r_type], entry.r_number, entry.r_filenum, entry.r_offset);

        let resource = resource::get_resource_data(data_path, &entry)?;
        let rh = &resource.header;
        let rh_type = (rh.id >> 11) as u8;
        let rh_id = rh.id & 0x7ff;
        println!("  resource: {}.{:03} comp_size {} decomp_size {} comp_method {}",
            RESOURCE_TYPE_MAP[&rh_type], rh_id, rh.comp_size, rh.decomp_size, rh.comp_method);

        let mut output: Vec<u8> = Vec::with_capacity(rh.decomp_size as usize);
        match CompressionMethod::try_from(rh.comp_method) {
            Ok(CompressionMethod::LZW) => {
                decompress::decompress_lzw(&resource.data, &mut output);
                println!("    => LZW: {} bytes decompressed", output.len());
            },
            Ok(CompressionMethod::Huffman) => {
                decompress::decompress_huffman(&resource.data, &mut output);
                println!("    => Huffman: {} bytes decompressed", output.len());
            },
            Ok(CompressionMethod::None) => {
                output = resource.data.clone();
                println!("    => None: {} bytes copied", output.len());
            },
            _ => { }
        }

        if !output.is_empty() {
            let out_fname = format!("{}/{}.{:03}", out_path, RESOURCE_TYPE_MAP[&entry.r_type], entry.r_number);
            let mut r_file = File::create(out_fname)?;
            r_file.write_all(&output)?;
        } else {
            println!("    !! could not decompress, skipping");
        }
    }
    Ok(())
}
