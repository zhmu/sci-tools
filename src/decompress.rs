use crate::bitstream;

#[derive(Copy, Clone)]
struct Token {
    offset: usize,
    length: usize
}

pub fn decompress_lzw(input: &[u8], output: &mut Vec<u8>) {
    let mut stream = bitstream::Streamer::new(input);
    let mut tokens = [ Token{ offset: 0, length: 0 }; 4096 ];

    let mut token_lastlength: usize;
    let mut num_bits: u32 = 9;
    let mut cur_token: usize = 0x102;
    let mut end_token: usize = 0x1ff;
    while !stream.end_of_stream() {
        let token = stream.get_bits_lsb(num_bits);
        if token == 0x100 {
            num_bits = 9;
            cur_token = 0x102;
            end_token = 0x1ff;
            continue
        } else if token == 0x101 {
            break
        }

        if token <= 0xff {
            token_lastlength = 1;
            output.push(token as u8);
        } else {
            let ref_token = &tokens[token as usize];
            token_lastlength = ref_token.length + 1;
            for n in 0..token_lastlength {
                let b = output[ref_token.offset + n];
                output.push(b);
            }
        }

        if cur_token > end_token && num_bits < 12 {
            num_bits += 1;
            end_token = (end_token << 1) + 1;
        }

        if cur_token <= end_token {
            tokens[cur_token] = Token{ offset: output.len() - token_lastlength, length: token_lastlength };
            cur_token += 1;
        }
    }
}

fn get_huffman_code(stream: &mut bitstream::Streamer, nodes: &Vec<u8>) -> u16
{
    let mut node_index: usize = 0;
    let mut next: usize;
    while nodes[node_index + 1] != 0 {
        if stream.get_bits_msb(1) != 0 {
            next = (nodes[node_index + 1] & 0xf) as usize; // 4 lo bits
            if next == 0 {
                return 0x100 | (stream.get_bits_msb(8) as u16);
            }
        } else {
            next = (nodes[node_index + 1] >> 4) as usize; // 4 hi bits
        }
        node_index += next * 2;
    }
    nodes[node_index] as u16 | ((nodes[node_index + 1] as u16) << 8)
}

pub fn decompress_huffman(input: &[u8], output: &mut Vec<u8>) {
    let mut stream = bitstream::Streamer::new(input);
    let num_nodes: u16 = 2 * stream.get_byte() as u16;
    let terminator: u16 = 0x100 | (stream.get_byte() as u16);

    let mut nodes: Vec<u8> = Vec::with_capacity(num_nodes as usize);
    for _ in 0..num_nodes {
        nodes.push(stream.get_byte());
    }

    while !stream.end_of_stream() {
        let c = get_huffman_code(&mut stream, &nodes);
        if c == terminator {
            break;
        }
        output.push(c as u8);
    }
}
