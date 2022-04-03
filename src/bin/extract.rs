extern crate scitools;

use num_enum::TryFromPrimitive;
use std::convert::TryFrom;
use std::env;
use std::fs::File;
use std::io::Write;

use scitools::{restype, resource};
use scitools::decompress;

#[derive(TryFromPrimitive)]
#[repr(u16)]
enum CompressionMethod {
    None = 0,
    LZW = 1,
    Huffman = 2
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("usage: {} data-path out-path", args[0]);
    }
    let data_path = &args[1];
    let out_path = &args[2];

    let resource_map_data = std::fs::read(format!("{}/resource.map", data_path))?;

    let resources = resource::read_resource_map(&resource_map_data);
    for entry in resources.iter() {
        let r_type = restype::u8_to_resource_type(entry.r_type).unwrap();
        let r_type_str = restype::resource_type_to_str(r_type);
        println!("{}.{:03} filenum {} offset {}", r_type_str, entry.r_number, entry.r_filenum, entry.r_offset);

        let resource = resource::get_resource_data(data_path, &entry)?;
        let rh = &resource.header;
        let rh_type = (rh.id >> 11) as u8;
        let rh_id = rh.id & 0x7ff;
        if rh_type != entry.r_type || rh_id != entry.r_number {
            panic!("resource mismatch, expected id {} got {}, type {} got {}", entry.r_number, rh_id, entry.r_type, rh_type);
        }

        println!("  resource: {}.{:03} comp_size {} decomp_size {} comp_method {}",
            r_type_str, rh_id, rh.comp_size, rh.decomp_size, rh.comp_method);

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
            let out_fname = format!("{}/{}.{:03}", out_path, r_type_str, entry.r_number);
            let mut r_file = File::create(out_fname)?;
            r_file.write_all(&output)?;
        } else {
            println!("    !! could not decompress, skipping");
        }
    }
    Ok(())
}
