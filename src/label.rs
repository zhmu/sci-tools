use crate::{disassemble, intermediate, script};

use std::collections::HashMap;

pub type LabelMap = HashMap<intermediate::Offset, String>;

fn generate_code_labels(block: &script::ScriptBlock, labels: &mut LabelMap) {
    let disasm = disassemble::Disassembler::new(&block, 0);
    for ins in disasm {
        let ii = intermediate::convert_instruction(&ins);
        let ic = ii.ops.last().unwrap();
        match ic {
            intermediate::IntermediateCode::Call(addr, _) => {
                let label = format!("local_{:x}", addr);
                labels.insert(*addr, label);
            },
            intermediate::IntermediateCode::BranchAlways(addr) |
            intermediate::IntermediateCode::Branch{ taken_offset: addr, next_offset: _, cond: _ } => {
                let label = format!("local_{:x}", addr);
                labels.insert(*addr, label);
            },
            _ => { }
        }
    }
}

pub fn find_code_labels(script: &script::Script) -> LabelMap {
    let mut labels: LabelMap = LabelMap::new();
    for block in &script.blocks {
        match block.r#type {
            script::BlockType::Code => {
                generate_code_labels(&block, &mut labels);
            },
            _ => { }
        };
    }
    labels
}

