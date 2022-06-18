use crate::{disassemble, intermediate, script, translate};

use std::collections::HashMap;

pub type LabelMap = HashMap<intermediate::Offset, String>;

fn generate_code_labels(block: &script::ScriptBlock, labels: &mut LabelMap) {
    let mut translator = translate::Translator::new();

    let disasm = disassemble::Disassembler::new(&block);
    for ins in disasm {
        let ii = translator.convert(&ins);
        if ii.ops.is_empty() { continue; }
        let ic = ii.ops.last().unwrap();
        match ic {
            intermediate::IntermediateCode::Assign(_, expr) => {
                match expr {
                    intermediate::Expression::Call(addr, _) => {
                        let label = format!("local_{:x}", addr);
                        labels.insert(*addr, label);
                    },
                    _ => { },
                }
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

