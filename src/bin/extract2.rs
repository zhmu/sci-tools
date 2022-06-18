extern crate scitools;

use std::env;
use std::fs::File;
use std::io::Write;
use explode::explode;
use scitools::{restype, resource2};

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 {
        panic!("usage: {} data-path out-path", args[0]);
    }
    let data_path = &args[1];
    let out_path = &args[2];

    let resource_map_data = std::fs::read(format!("{}/resource.map", data_path))?;

    let resources = resource2::read_resource_map(&resource_map_data);
    for entry in resources.iter() {
        let resource = resource2::get_resource_data(data_path, &entry)?;
        let rh = &resource.header;
        let r_type = rh.res_type & 0x7f;
        let r_type = restype::u8_to_resource_type(r_type).unwrap();
        let r_type_str = restype::resource_type_to_str(r_type);

        println!("  resource: {}.{:03} segment_length {} length {} compress_used {}",
            r_type_str, rh.id, rh.segment_length, rh.length, rh.compress_used);

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
            let out_fname = format!("{}/{}.{:03}", out_path, r_type_str, entry.r_number);
            let mut r_file = File::create(out_fname)?;
            r_file.write_all(&output)?;
        }
    }
    Ok(())
}
