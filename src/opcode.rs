pub enum Arg {
    RelPos8,
    RelPos16,
    Imm8,
    Imm16,
}

pub enum Special {
    None,
    Invalid,
    Ret,
    Jump,
    KCall,
    CallI,
    Call0,
    CallE
}

pub struct Opcode {
    pub name: &'static str,
    pub arg: &'static [Arg],
    pub special: Special
}

/*
static O_NAME_BITS_56: phf::Map<u8, char> = phf_map!{
    0u8 => 'l',
    1u8 => 's',
    2u8 => '+',
    3u8 => '-',
};
static O_NAME_BITS_3: phf::Map<u8, char> = phf_map!{
    0u8 => 'a',
    1u8 => 's',
};
static O_NAME_BITS_12: phf::Map<u8, char> = phf_map!{
    0u8 => 'g',
    1u8 => 'l',
    2u8 => 't',
    3u8 => 'p',
};

fn generate_opcode_7f_to_ff(op: &mut HashMap<u8, Opcode>) {
    for opcode in 0x80..=0xff {
        let mut name: String;
        name = O_NAME_BITS_56[&((opcode >> 5) & 3)].to_string();
        name += &O_NAME_BITS_3[&((opcode >> 3) & 1)].to_string();
        name += &O_NAME_BITS_12[&((opcode >> 1) & 3)].to_string();
        if (opcode & (1 << 4)) != 0 {
            name += "i";
        }

        let arg: &[ Arg; 1 ];
        if (opcode & (1 << 0)) != 0 {
            name += ".b";
            arg = &[ Arg::Imm8 ];
        } else {
            name += ".w";
            arg = &[ Arg::Imm16 ];
        }
        op.insert(opcode, Opcode{ name, arg, special: Special::None });
    }
}

pub fn make_opcode_map() -> HashMap<u8, Opcode> {
    let mut op: HashMap<u8, Opcode> = HashMap::new();
    op.insert(0x00, Opcode::new("bnot.w",     &[ ],                                           Special::None));
    op.insert(0x01, Opcode::new("bnot.b",     &[ ],                                           Special::None));
    op.insert(0x02, Opcode::new("add.w",      &[ ],                                           Special::None));
    op.insert(0x03, Opcode::new("add.b",      &[ ],                                           Special::None));
    op.insert(0x04, Opcode::new("sub.w",      &[ ],                                           Special::None));
    op.insert(0x05, Opcode::new("sub.b",      &[ ],                                           Special::None));
    op.insert(0x06, Opcode::new("mul.w",      &[ ],                                           Special::None));
    op.insert(0x07, Opcode::new("mul.b",      &[ ],                                           Special::None));
    op.insert(0x08, Opcode::new("div.w",      &[ ],                                           Special::None));
    op.insert(0x09, Opcode::new("div.b",      &[ ],                                           Special::None));
    op.insert(0x0a, Opcode::new("mod.w",      &[ ],                                           Special::None));
    op.insert(0x0b, Opcode::new("mod.b",      &[ ],                                           Special::None));
    op.insert(0x0c, Opcode::new("shr.w",      &[ ],                                           Special::None));
    op.insert(0x0d, Opcode::new("shr.b",      &[ ],                                           Special::None));
    op.insert(0x0e, Opcode::new("shl.w",      &[ ],                                           Special::None));
    op.insert(0x0f, Opcode::new("shl.b",      &[ ],                                           Special::None));
    op.insert(0x10, Opcode::new("xor.w",      &[ ],                                           Special::None));
    op.insert(0x11, Opcode::new("xor.b",      &[ ],                                           Special::None));
    op.insert(0x12, Opcode::new("and.w",      &[ ],                                           Special::None));
    op.insert(0x13, Opcode::new("and.b",      &[ ],                                           Special::None));
    op.insert(0x14, Opcode::new("or.w",       &[ ],                                           Special::None));
    op.insert(0x15, Opcode::new("or.b",       &[ ],                                           Special::None));
    op.insert(0x16, Opcode::new("neg.w",      &[ ],                                           Special::None));
    op.insert(0x17, Opcode::new("neg.b",      &[ ],                                           Special::None));
    op.insert(0x18, Opcode::new("not.w",      &[ ],                                           Special::None));
    op.insert(0x19, Opcode::new("not.b",      &[ ],                                           Special::None));
    op.insert(0x1a, Opcode::new("eq?.w",      &[ ],                                           Special::None));
    op.insert(0x1b, Opcode::new("eq?.b",      &[ ],                                           Special::None));
    op.insert(0x1c, Opcode::new("ne?.w",      &[ ],                                           Special::None));
    op.insert(0x1d, Opcode::new("ne?.b",      &[ ],                                           Special::None));
    op.insert(0x1e, Opcode::new("gt?.w",      &[ ],                                           Special::None));
    op.insert(0x1f, Opcode::new("gt?.b",      &[ ],                                           Special::None));
    op.insert(0x20, Opcode::new("ge?.w",      &[ ],                                           Special::None));
    op.insert(0x21, Opcode::new("ge?.b",      &[ ],                                           Special::None));
    op.insert(0x22, Opcode::new("lt?.w",      &[ ],                                           Special::None));
    op.insert(0x23, Opcode::new("lt?.b",      &[ ],                                           Special::None));
    op.insert(0x24, Opcode::new("le?.w",      &[ ],                                           Special::None));
    op.insert(0x25, Opcode::new("le?.b",      &[ ],                                           Special::None));
    op.insert(0x26, Opcode::new("ugt?.w",     &[ ],                                           Special::None));
    op.insert(0x27, Opcode::new("ugt?.b",     &[ ],                                           Special::None));
    op.insert(0x28, Opcode::new("uge?.w",     &[ ],                                           Special::None));
    op.insert(0x29, Opcode::new("uge?.b",     &[ ],                                           Special::None));
    op.insert(0x2a, Opcode::new("ult?.w",     &[ ],                                           Special::None));
    op.insert(0x2b, Opcode::new("ult?.b",     &[ ],                                           Special::None));
    op.insert(0x2c, Opcode::new("ule?.w",     &[ ],                                           Special::None));
    op.insert(0x2d, Opcode::new("ule?.b",     &[ ],                                           Special::None));
    op.insert(0x2e, Opcode::new("bt.w",       &[ Arg::RelPos16 ],                             Special::Jump));
    op.insert(0x2f, Opcode::new("bt.b",       &[ Arg::RelPos8 ],                              Special::Jump));
    op.insert(0x30, Opcode::new("bnt.w",      &[ Arg::RelPos16 ],                             Special::Jump));
    op.insert(0x31, Opcode::new("bnt.b",      &[ Arg::RelPos8 ],                              Special::Jump));
    op.insert(0x32, Opcode::new("jmp.w",      &[ Arg::RelPos16 ],                             Special::Jump));
    op.insert(0x33, Opcode::new("jmp.b",      &[ Arg::RelPos8 ],                              Special::Jump));
    op.insert(0x34, Opcode::new("ldi.w",      &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x35, Opcode::new("ldi.b",      &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x36, Opcode::new("push.w",     &[ ],                                           Special::None));
    op.insert(0x37, Opcode::new("push.b",     &[ ],                                           Special::None));
    op.insert(0x38, Opcode::new("pushi.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x39, Opcode::new("pushi.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x3a, Opcode::new("toss.w",     &[ ],                                           Special::None));
    op.insert(0x3b, Opcode::new("toss.b",     &[ ],                                           Special::None));
    op.insert(0x3c, Opcode::new("dup.w",      &[ ],                                           Special::None));
    op.insert(0x3d, Opcode::new("dup.b",      &[ ],                                           Special::None));
    op.insert(0x3e, Opcode::new("link.w",     &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x3f, Opcode::new("link.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x40, Opcode::new("call.w",     &[ Arg::RelPos16, Arg::Imm8 ],                  Special::CallI));
    op.insert(0x41, Opcode::new("call.b",     &[ Arg::RelPos8, Arg::Imm8 ],                   Special::CallI));
    op.insert(0x42, Opcode::new("callk.w",    &[ Arg::Imm16, Arg::Imm8 ],                     Special::KCall));
    op.insert(0x43, Opcode::new("callk.b",    &[ Arg::Imm8, Arg::Imm8 ],                      Special::KCall));
    op.insert(0x44, Opcode::new("callb.w",    &[ Arg::Imm16, Arg::Imm8 ],                     Special::Call0));
    op.insert(0x45, Opcode::new("callb.b",    &[ Arg::Imm8, Arg::Imm8 ],                      Special::Call0));
    op.insert(0x46, Opcode::new("calle.w",    &[ Arg::Imm16, Arg::Imm16, Arg::Imm8 ],         Special::CallE));
    op.insert(0x47, Opcode::new("calle.b",    &[ Arg::Imm8, Arg::Imm8, Arg::Imm8 ],           Special::CallE));
    op.insert(0x48, Opcode::new("ret.w",      &[ ],                                           Special::Ret));
    op.insert(0x49, Opcode::new("ret.b",      &[ ],                                           Special::Ret));
    op.insert(0x4a, Opcode::new("send.w",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x4b, Opcode::new("send.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x4c, Opcode::new("?4c",        &[ ],                                           Special::Invalid));
    op.insert(0x4d, Opcode::new("?4d",        &[ ],                                           Special::Invalid));
    op.insert(0x4e, Opcode::new("?4e",        &[ ],                                           Special::Invalid));
    op.insert(0x4f, Opcode::new("?4f",        &[ ],                                           Special::Invalid));
    op.insert(0x50, Opcode::new("class.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x51, Opcode::new("class.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x52, Opcode::new("?52",        &[ ],                                           Special::Invalid));
    op.insert(0x53, Opcode::new("?53",        &[ ],                                           Special::Invalid));
    op.insert(0x54, Opcode::new("self.w",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x55, Opcode::new("self.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x56, Opcode::new("super.w",    &[ Arg::Imm16, Arg::Imm8 ],                     Special::None));
    op.insert(0x57, Opcode::new("super.b",    &[ Arg::Imm8, Arg::Imm8 ],                      Special::None));
    op.insert(0x58, Opcode::new("&rest.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x59, Opcode::new("&rest.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x5a, Opcode::new("lea.w",      &[ Arg::Imm16, Arg::Imm16 ],                    Special::None));
    op.insert(0x5b, Opcode::new("lea.b",      &[ Arg::Imm8, Arg::Imm8 ],                      Special::None));
    op.insert(0x5c, Opcode::new("selfid.w",   &[ ],                                           Special::None));
    op.insert(0x5d, Opcode::new("selfid.b",   &[ ],                                           Special::None));
    op.insert(0x5e, Opcode::new("?5e",        &[ ],                                           Special::Invalid));
    op.insert(0x5f, Opcode::new("?5f",        &[ ],                                           Special::Invalid));
    op.insert(0x60, Opcode::new("pprev.w",    &[ ],                                           Special::None));
    op.insert(0x61, Opcode::new("pprev.b",    &[ ],                                           Special::None));
    op.insert(0x62, Opcode::new("ptoa.w",     &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x63, Opcode::new("ptoa.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x64, Opcode::new("atop.w",     &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x65, Opcode::new("atop.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x66, Opcode::new("ptos.w",     &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x67, Opcode::new("ptos.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x68, Opcode::new("stop.w",     &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x69, Opcode::new("stop.b",     &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x6a, Opcode::new("iptoa.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x6b, Opcode::new("iptoa.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x6c, Opcode::new("dptoa.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x6d, Opcode::new("dptoa.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x6e, Opcode::new("iptos.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x6f, Opcode::new("iptos.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x70, Opcode::new("dptos.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x71, Opcode::new("dptos.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x72, Opcode::new("lofsa.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x73, Opcode::new("lofsa.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x74, Opcode::new("lofss.w",    &[ Arg::Imm16 ],                                Special::None));
    op.insert(0x75, Opcode::new("lofss.b",    &[ Arg::Imm8 ],                                 Special::None));
    op.insert(0x76, Opcode::new("push0.w",    &[ ],                                           Special::None));
    op.insert(0x77, Opcode::new("push0.b",    &[ ],                                           Special::None));
    op.insert(0x78, Opcode::new("push1.w",    &[ ],                                           Special::None));
    op.insert(0x79, Opcode::new("push1.b",    &[ ],                                           Special::None));
    op.insert(0x7a, Opcode::new("push2.w",    &[ ],                                           Special::None));
    op.insert(0x7b, Opcode::new("push2.b",    &[ ],                                           Special::None));
    op.insert(0x7c, Opcode::new("pushself.w", &[ ],                                           Special::None));
    op.insert(0x7d, Opcode::new("pushself.b", &[ ],                                           Special::None));
    op.insert(0x7e, Opcode::new("?7e",        &[ ],                                           Special::Invalid));
    op.insert(0x7f, Opcode::new("?7f",        &[ ],                                           Special::Invalid));
    generate_opcode_7f_to_ff(&mut op);
    op
}
*/

pub static OPCODES: [Opcode; 256] = [
    Opcode{ name: "bnot.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "bnot.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "add.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "add.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "sub.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "sub.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "mul.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "mul.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "div.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "div.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "mod.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "mod.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "shr.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "shr.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "shl.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "shl.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "xor.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "xor.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "and.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "and.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "or.w",                 arg: &[ ],                                      special: Special::None },
    Opcode{ name: "or.b",                 arg: &[ ],                                      special: Special::None },
    Opcode{ name: "neg.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "neg.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "not.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "not.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "eq?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "eq?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ne?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ne?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "gt?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "gt?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ge?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ge?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "lt?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "lt?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "le?.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "le?.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ugt?.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ugt?.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "uge?.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "uge?.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ult?.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ult?.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ule?.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ule?.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "bt.w",                 arg: &[ Arg::RelPos16 ],                        special: Special::Jump },
    Opcode{ name: "bt.b",                 arg: &[ Arg::RelPos8 ],                         special: Special::Jump },
    Opcode{ name: "bnt.w",                arg: &[ Arg::RelPos16 ],                        special: Special::Jump },
    Opcode{ name: "bnt.b",                arg: &[ Arg::RelPos8 ],                         special: Special::Jump },
    Opcode{ name: "jmp.w",                arg: &[ Arg::RelPos16 ],                        special: Special::Jump },
    Opcode{ name: "jmp.b",                arg: &[ Arg::RelPos8 ],                         special: Special::Jump },
    Opcode{ name: "ldi.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ldi.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "push.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "pushi.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "pushi.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "toss.w",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "toss.b",               arg: &[ ],                                      special: Special::None },
    Opcode{ name: "dup.w",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "dup.b",                arg: &[ ],                                      special: Special::None },
    Opcode{ name: "link.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "link.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "call.w",               arg: &[ Arg::RelPos16, Arg::Imm8 ],             special: Special::CallI },
    Opcode{ name: "call.b",               arg: &[ Arg::RelPos8, Arg::Imm8 ],              special: Special::CallI },
    Opcode{ name: "callk.w",              arg: &[ Arg::Imm16, Arg::Imm8 ],                special: Special::KCall },
    Opcode{ name: "callk.b",              arg: &[ Arg::Imm8, Arg::Imm8 ],                 special: Special::KCall },
    Opcode{ name: "callb.w",              arg: &[ Arg::Imm16, Arg::Imm8 ],                special: Special::Call0 },
    Opcode{ name: "callb.b",              arg: &[ Arg::Imm8, Arg::Imm8 ],                 special: Special::Call0 },
    Opcode{ name: "calle.w",              arg: &[ Arg::Imm16, Arg::Imm16, Arg::Imm8 ],    special: Special::CallE },
    Opcode{ name: "calle.b",              arg: &[ Arg::Imm8, Arg::Imm8, Arg::Imm8 ],      special: Special::CallE },
    Opcode{ name: "ret.w",                arg: &[ ],                                      special: Special::Ret },
    Opcode{ name: "ret.b",                arg: &[ ],                                      special: Special::Ret },
    Opcode{ name: "send.w",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "send.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "?4c",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?4d",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?4e",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?4f",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "class.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "class.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "?52",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?53",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "self.w",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "self.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "super.w",              arg: &[ Arg::Imm16, Arg::Imm8 ],                special: Special::None },
    Opcode{ name: "super.b",              arg: &[ Arg::Imm8, Arg::Imm8 ],                 special: Special::None },
    Opcode{ name: "&rest.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "&rest.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lea.w",                arg: &[ Arg::Imm16, Arg::Imm16 ],               special: Special::None },
    Opcode{ name: "lea.b",                arg: &[ Arg::Imm8, Arg::Imm8 ],                 special: Special::None },
    Opcode{ name: "selfid.w",             arg: &[ ],                                      special: Special::None },
    Opcode{ name: "selfid.b",             arg: &[ ],                                      special: Special::None },
    Opcode{ name: "?5e",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?5f",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "pprev.w",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "pprev.b",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "ptoa.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ptoa.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "atop.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "atop.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ptos.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ptos.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "stop.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "stop.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "iptoa.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "iptoa.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "dptoa.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "dptoa.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "iptos.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "iptos.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "dptos.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "dptos.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lofsa.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lofsa.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lofss.w",              arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lofss.b",              arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "push0.w",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push0.b",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push1.w",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push1.b",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push2.w",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "push2.b",              arg: &[ ],                                      special: Special::None },
    Opcode{ name: "pushself.w",           arg: &[ ],                                      special: Special::None },
    Opcode{ name: "pushself.b",           arg: &[ ],                                      special: Special::None },
    Opcode{ name: "?7e",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "?7f",                  arg: &[ ],                                      special: Special::Invalid },
    Opcode{ name: "lag.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lag.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lal.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lal.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lat.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lat.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lap.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lap.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsg.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsg.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsl.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsl.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lst.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lst.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsp.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsp.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lagi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lagi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lali.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lali.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lati.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lati.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lapi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lapi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsgi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsgi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsli.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsli.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lsti.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lsti.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "lspi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "lspi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sag.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sag.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sal.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sal.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sat.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sat.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sap.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sap.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssg.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssg.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssl.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssl.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sst.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sst.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssp.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssp.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sagi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sagi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sali.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sali.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sati.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sati.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sapi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sapi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssgi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssgi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssli.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssli.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "ssti.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "ssti.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "sspi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "sspi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+ag.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+ag.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+al.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+al.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+at.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+at.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+ap.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+ap.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sg.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sg.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sl.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sl.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+st.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+st.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sp.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sp.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+agi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+agi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+ali.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+ali.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+ati.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+ati.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+api.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+api.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sgi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sgi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sli.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sli.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+sti.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+sti.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "+spi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "+spi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-ag.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-ag.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-al.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-al.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-at.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-at.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-ap.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-ap.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sg.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sg.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sl.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sl.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-st.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-st.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sp.w",                arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sp.b",                arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-agi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-agi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-ali.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-ali.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-ati.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-ati.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-api.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-api.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sgi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sgi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sli.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sli.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-sti.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-sti.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
    Opcode{ name: "-spi.w",               arg: &[ Arg::Imm16 ],                           special: Special::None },
    Opcode{ name: "-spi.b",               arg: &[ Arg::Imm8 ],                            special: Special::None },
];
