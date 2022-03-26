pub struct Streamer<'a> {
    input: &'a [u8],
    input_pos: usize,
    bits: u32,
    nbits: u32,
}

impl<'a> Streamer<'a> {
    pub fn new(input: &'a [u8]) -> Streamer {
        Streamer{ input, bits: 0, nbits: 0, input_pos: 0 }
    }

    pub fn end_of_stream(&self) -> bool {
        self.input_pos >= self.input.len()
    }

    pub fn get_byte(&mut self) -> u8 {
        let mut b: u8 = 0;
        if !self.end_of_stream() {
            b = self.input[self.input_pos];
            self.input_pos += 1;
        }
        b
    }

    fn fetch_bits_lsb(&mut self) {
        while self.nbits <= 24 {
            let b = (self.get_byte() as u32) << self.nbits;
            self.bits |= b;
            self.nbits += 8;
        }
    }

    fn fetch_bits_msb(&mut self) {
        while self.nbits <= 24 {
            let b = (self.get_byte() as u32) << (24 - self.nbits);
            self.bits |= b;
            self.nbits += 8;
        }
    }

    pub fn get_bits_lsb(&mut self, n: u32) -> u32 {
        if self.nbits < n {
            self.fetch_bits_lsb();
        }
        let result = self.bits & !(0xffff_ffff << n);
        self.bits >>= n;
        self.nbits -= n;
        result
    }

    pub fn get_bits_msb(&mut self, n: u32) -> u32 {
        if self.nbits < n {
            self.fetch_bits_msb();
        }
        let result = self.bits >> (32 - n);
        self.bits <<= n;
        self.nbits -= n;
        result
    }
}
