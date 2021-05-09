use crate::{code, intermediate, script, disassemble, label};

use std::collections::{HashMap, HashSet};

fn fill_out_index(offset_index: &mut code::OffsetIndex, offset_to_index: &HashMap<intermediate::Offset, usize>) -> () {
    match offset_index {
        code::OffsetIndex::Offset(offset) => {
            match offset_to_index.get(offset) {
                Some(index) => { *offset_index = code::OffsetIndex::Index(*index) },
                None => { panic!("offset {:x} not found", offset) }
            }
        },
        code::OffsetIndex::None => { },
        _ => { panic!("not an offset?"); }
    }
}

fn is_unconditional_branch(ii: &intermediate::Instruction) -> Option<intermediate::Offset> {
    let ic = ii.ops.last().unwrap();
    match ic {
        intermediate::IntermediateCode::BranchAlways(a) => { Some(*a) },
        _ => { None }
    }
}

fn is_conditional_branch(ii: &intermediate::Instruction) -> Option<(intermediate::Offset, intermediate::Offset)> {
    let ic = ii.ops.last().unwrap();
    match ic {
        intermediate::IntermediateCode::Branch{ taken_offset, next_offset, cond: _ } => { Some((*taken_offset, *next_offset)) },
        _ => { None }
    }
}

fn must_split(ii: &intermediate::Instruction) -> bool {
    let ic = ii.ops.last().unwrap();
    match ic {
        intermediate::IntermediateCode::BranchAlways(_) => { true },
        intermediate::IntermediateCode::Branch{ taken_offset: _, next_offset: _, cond: _ } => { true },
        intermediate::IntermediateCode::Return() => { true },
        _ => { false }
    }
}

fn split_instructions_from_block(block: &mut code::CodeBlock, offset: intermediate::Offset) -> Vec<intermediate::Instruction> {
    for (n, ii) in block.code.instructions.iter().enumerate() {
        if ii.offset == offset {
            return block.code.instructions.drain(n..).collect();
        }
    }
    unreachable!();
}

pub fn split_code_in_blocks<'a>(script_block: &'a script::ScriptBlock, labels: &label::LabelMap) -> Vec<code::CodeBlock<'a>> {
    let mut blocks: Vec<code::CodeBlock> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script_block, 0);
    let mut block_offsets: HashSet<intermediate::Offset> = HashSet::new();

    // Split on specific instruction
    let mut instructions: Vec<intermediate::Instruction> = Vec::new();
    for ins in disasm {
        instructions.push(intermediate::convert_instruction(&ins));
        let ii = instructions.last().unwrap().clone();
        if !must_split(&ii) { continue; }

        let mut block = code::CodeBlock::new(&script_block, code::CodeFragment{ instructions: instructions.drain(..).collect() });
        if let Some(next_offset) = is_unconditional_branch(&ii) {
            block.branch_index_always = code::OffsetIndex::Offset(next_offset);
        } else if let Some((true_offset, next_offset)) = is_conditional_branch(&ii) {
            block.branch_index_true = code::OffsetIndex::Offset(true_offset);
            block.branch_index_false = code::OffsetIndex::Offset(next_offset);
        }

        let block_offset = block.code.get_start_offset();
        blocks.push(block);
        block_offsets.insert(block_offset);
    }

    // Sometimes a lone 'bnot.w' (0) is added to the instructions; this is the
    // only unprocessed instructions block we will accept
    if !instructions.is_empty() {
        assert_eq!(instructions.len(), 1);
        let ii = instructions.first().unwrap();
        assert_eq!(script_block.data[ii.offset as usize - script_block.base], 0x00);
    }

    // Ensure all block_offsets are split
    for (offset, _) in labels {
        if block_offsets.contains(offset) { continue; }
        let offset = *offset;
        for (n, block) in &mut blocks.iter_mut().enumerate() {
            if offset < block.code.get_start_offset() { continue; }
            if offset >= block.code.get_end_offset() { continue; }

            let instructions = split_instructions_from_block(block, offset);
            let mut new_block = code::CodeBlock::new(&script_block, code::CodeFragment{ instructions });
            // Move branches to new block
            new_block.branch_index_true = block.branch_index_true;
            new_block.branch_index_false = block.branch_index_false;
            new_block.branch_index_always = block.branch_index_always;
            // Ensure the previous block unconditionally branches to the new one
            block.branch_index_true = code::OffsetIndex::None;
            block.branch_index_false = code::OffsetIndex::None;
            block.branch_index_always = code::OffsetIndex::Offset(offset);

            blocks.insert(n + 1, new_block);
            break;
        }
    }

    let mut offset_to_index: HashMap<intermediate::Offset, usize> = HashMap::new();
    for (n, block) in blocks.iter().enumerate() {
        offset_to_index.insert(block.code.get_start_offset(), n);
    }

    // Rewrite offsets to indices
    for block in &mut blocks {
       fill_out_index(&mut block.branch_index_always, &offset_to_index);
       fill_out_index(&mut block.branch_index_true, &offset_to_index);
       fill_out_index(&mut block.branch_index_false, &offset_to_index);
    }

    blocks
}

