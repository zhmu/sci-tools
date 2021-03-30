extern crate scitools;

use scitools::{disassemble, script, vocab, said, object_class, graph_lib, reduce, code};
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::env;
use std::fs::File;
use std::fmt;

use petgraph::graph::NodeIndex;
use petgraph::algo::kosaraju_scc;
use petgraph::{Incoming, Outgoing};
use petgraph::visit::{NodeRef, EdgeRef};
use petgraph::visit::NodeIndexable;
use petgraph::visit::IntoNodeReferences;

type LabelMap = HashMap<usize, String>;

const DEBUG_VM: bool = false;

#[derive(Debug)]
enum ScriptError {
    IoError(std::io::Error),
    VocabError(vocab::VocabError),
    SaidError(said::SaidError),
    ObjectClassError(object_class::ObjectClassError),
}

impl From<std::io::Error> for ScriptError {
    fn from(error: std::io::Error) -> Self {
       ScriptError::IoError(error)
    }
}

impl From<vocab::VocabError> for ScriptError {
    fn from(error: vocab::VocabError) -> Self {
       ScriptError::VocabError(error)
    }
}

impl From<said::SaidError> for ScriptError {
    fn from(error: said::SaidError) -> Self {
       ScriptError::SaidError(error)
    }
}

impl From<object_class::ObjectClassError> for ScriptError {
    fn from(error: object_class::ObjectClassError) -> Self {
       ScriptError::ObjectClassError(error)
    }
}

fn generate_code_labels(block: &script::ScriptBlock, labels: &mut LabelMap) {
    let disasm = disassemble::Disassembler::new(&block, 0);
    for ins in disasm {
        if is_call(&ins) {
            let j_offset = script::relpos0_to_absolute_offset(&ins);
            let label = format!("local_{:x}", j_offset);
            labels.insert(j_offset, label);
        }
        if is_unconditional_branch(&ins) || is_conditional_branch(&ins) {
            let j_offset = script::relpos0_to_absolute_offset(&ins);
            let label = format!("local_{:x}", j_offset);
            labels.insert(j_offset, label);
        }
    }
}

fn find_code_labels(script: &script::Script) -> Result<LabelMap, ScriptError> {
    let mut labels: LabelMap = LabelMap::new();
    for block in &script.blocks {
        match block.r#type {
            script::BlockType::Code => {
                generate_code_labels(&block, &mut labels);
            },
            _ => { }
        };
    }
    Ok(labels)
}

fn is_always_branch(ins: &disassemble::Instruction) -> bool {
    let opcode = ins.bytes[0];
    match opcode {
        0x40 | 0x41 /* call */ => { return true },
        //0x42 | 0x43 /* callk */ => { return true },
        //0x44 | 0x45 /* callb */ => { return true },
        //0x46 | 0x47 /* calle */ => { return true },
        0x48 | 0x49 /* ret */ => { return true },
        _ => { return false }
    }
}

fn is_call(ins: &disassemble::Instruction) -> bool {
    let opcode = ins.bytes[0];
    opcode == 0x40 || opcode == 0x41 /* call */
}

fn is_unconditional_branch(ins: &disassemble::Instruction) -> bool {
    let opcode = ins.bytes[0];
    opcode == 0x32 || opcode == 0x33 /* jmp */
}

fn is_return(ins: &disassemble::Instruction) -> bool {
    let opcode = ins.bytes[0];
    opcode == 0x48 || opcode == 0x49 /* ret */
}

fn is_conditional_branch(ins: &disassemble::Instruction) -> bool {
    let opcode = ins.bytes[0];
    match opcode {
        0x2e | 0x2f /* bt */ => { return true },
        0x30 | 0x31 /* bnt */ => { return true },
        _ => { return false }
    }
}

enum OffsetIndex {
    Offset(usize),
    Index(usize),
}

struct CodeBlock<'a> {
    script: &'a script::ScriptBlock<'a>,
    code: code::CodeFragment,
    branch_index_always: Option<OffsetIndex>,
    branch_index_true: Option<OffsetIndex>,
    branch_index_false: Option<OffsetIndex>,
}

impl<'a> CodeBlock<'a> {
    fn new(script: &'a script::ScriptBlock, code: code::CodeFragment) -> CodeBlock<'a> {
        CodeBlock{ script, code, branch_index_always: None, branch_index_true: None, branch_index_false: None }
    }
}

fn fill_out_index(offset_index: &mut Option<OffsetIndex>, offset_to_index: &HashMap<usize, usize>) -> () {
    if let Some(oi) = offset_index {
        match oi {
            OffsetIndex::Offset(offset) => {
                match offset_to_index.get(offset) {
                    Some(index) => { *oi = OffsetIndex::Index(*index) },
                    None => { panic!("offset {:x} not found", offset) }
                }
            },
            _ => { panic!("not an offset?"); }
        }
    }
}

fn must_split(ins: &disassemble::Instruction) -> bool {
    is_always_branch(&ins) || is_unconditional_branch(&ins) || is_conditional_branch(&ins)
}

fn split_code_in_blocks<'a>(script_block: &'a script::ScriptBlock, labels: &LabelMap) -> Vec<CodeBlock<'a>> {
    let mut blocks: Vec<CodeBlock> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script_block, 0);
    let mut current_block_offset: usize = script_block.base;
    let mut block_offsets: HashSet<usize> = HashSet::new();

    // Split on specific instruction
    for ins in disasm {
        if must_split(&ins) {
            let next_offset = ins.offset + ins.bytes.len();
            let length = next_offset - current_block_offset;
            let mut block = CodeBlock::new(&script_block, code::CodeFragment{ offset: current_block_offset, length });

            if is_always_branch(&ins) || is_unconditional_branch(&ins) {
                if is_unconditional_branch(&ins) {
                    let next_offset = script::relpos0_to_absolute_offset(&ins);
                    block.branch_index_always = Some(OffsetIndex::Offset(next_offset));
                } else if !is_return(&ins) {
                    block.branch_index_always = Some(OffsetIndex::Offset(next_offset));
                }
            } else if is_conditional_branch(&ins) {
                let true_offset = script::relpos0_to_absolute_offset(&ins);
                block.branch_index_true = Some(OffsetIndex::Offset(true_offset));
                block.branch_index_false = Some(OffsetIndex::Offset(next_offset));
            }

            let block_offset = block.code.offset;
            blocks.push(block);
            block_offsets.insert(block_offset);
            current_block_offset = next_offset;
        }
    }

    // Ensure all block_offsets are split
    for (offset, _label) in labels {
        if !block_offsets.contains(offset) {
            let offset = *offset;
            for (n, block) in &mut blocks.iter_mut().enumerate() {
                if offset >= block.code.offset && offset < block.code.offset + block.code.length {
                    let mut new_block = CodeBlock::new(&script_block, code::CodeFragment{ offset, length: block.code.length - (offset - block.code.offset) });
                    block.code.length = offset - block.code.offset;
                    new_block.branch_index_true = block.branch_index_true.take();
                    new_block.branch_index_false = block.branch_index_false.take();
                    new_block.branch_index_always = block.branch_index_always.take();
                    block.branch_index_always = Some(OffsetIndex::Offset(offset));
                    blocks.insert(n + 1, new_block);
                    break;
                }
            }
        }
    }

    let mut offset_to_index: HashMap<usize, usize> = HashMap::new();
    for (n, block) in blocks.iter().enumerate() {
        offset_to_index.insert(block.code.offset, n);
    }

    // Rewrite offsets to indices
    for block in &mut blocks {
       fill_out_index(&mut block.branch_index_always, &offset_to_index);
       fill_out_index(&mut block.branch_index_true, &offset_to_index);
       fill_out_index(&mut block.branch_index_false, &offset_to_index);
    }

    blocks
}

fn create_graph_from_codeblocks<'a>(blocks: &'a Vec<CodeBlock<'a>>) -> code::CodeGraph<'a> {
    let mut graph = code::CodeGraph::new();
    let mut node_map: HashMap<usize, petgraph::prelude::NodeIndex> = HashMap::new();
    for (block_nr, block) in blocks.iter().enumerate() {
        let code_block = code::CodeNode{ script: block.script, ops: vec![ code::Operation::Execute(block.code.clone()) ]};
        let n = graph.add_node(code_block);
        node_map.insert(block_nr, n);
    }

    for (block_nr, block) in blocks.iter().enumerate() {
        if let Some(index_offset) = &block.branch_index_always {
            if let OffsetIndex::Index(index) = index_offset {
                graph.add_edge(node_map[&block_nr], node_map[index], code::CodeEdge{ branch: code::Branch::Always });
            }
        }
        if let Some(index_offset) = &block.branch_index_true {
            if let OffsetIndex::Index(index) = index_offset {
                graph.add_edge(node_map[&block_nr], node_map[index], code::CodeEdge{ branch: code::Branch::True });
            }
        }
        if let Some(index_offset) = &block.branch_index_false {
            if let OffsetIndex::Index(index) = index_offset {
                graph.add_edge(node_map[&block_nr], node_map[index], code::CodeEdge{ branch: code::Branch::False });
            }
        }
    }

    graph
}

fn is_single_execute<'a>(node: &'a code::CodeNode) -> Option<&'a code::CodeFragment> {
    if node.ops.len() == 1 {
        if let code::Operation::Execute(code) = node.ops.first().unwrap() {
            return Some(code)
        }
    }
    None
}

fn plot_graph(fname: &str, graph: &code::CodeGraph) -> Result<(), std::io::Error> {
    let mut out_file = File::create(fname).unwrap();
    writeln!(out_file, "digraph G {{")?;
    for node in graph.node_references() {
        let shape: &str;
        let mut label: String;
        let weight = node.weight();
        label = format!("{}", graph.to_index(node.id()));
        if let Some(code) = is_single_execute(weight) {
            shape = "oval";
            label += format!(" [{:x}..{:x}]", code.offset, code.offset + code.length).as_str();
        } else {
            shape = "box";
            for o in &weight.ops {
                label += format!(" {}", o.as_str()).as_str();
            }
        }
        writeln!(out_file, "  {} [ label=\"{}\" shape=\"{}\"]", graph.to_index(node.id()), label, shape)?;
    }
    for edge in graph.edge_references() {
        let e = edge.weight();
        let colour: &str;
        match e.branch {
            code::Branch::True => { colour = "green"; },
            code::Branch::False => { colour = "red"; },
            code::Branch::Always => { colour = "black"; },
        }
        writeln!(out_file, "  {} -> {} [ color={} ]", graph.to_index(edge.source()), graph.to_index(edge.target()), colour)?;
    }
    writeln!(out_file, "}}")?;
    Ok(())
}


#[derive(Debug)]
struct LoopNode {
    index: NodeIndex,
    is_continue: bool,
    is_break: bool,
    is_multi_entry: bool
}

impl LoopNode {
    fn new(index: NodeIndex) -> Self {
        LoopNode{ index, is_continue: false, is_break: false, is_multi_entry: false }
    }
}

#[derive(Debug)]
struct Loop {
    header: NodeIndex,
    body: Vec<LoopNode>
}

fn analyse_graph(graph: &code::CodeGraph) {
    // 1. Determine strongly connected components
    let m = kosaraju_scc(&graph);
    for (n, items) in m.iter().enumerate() {
        if items.len() < 2 { continue }
        println!("{}: {:?}", n, items);
        let subgraph = graph_lib::create_subgraph_with_node_indices(&graph, items);

        // 2. Identify loops by finding back edges
        let back_edges = graph_lib::find_back_edge_nodes(&subgraph);
        println!("back_edges {:?}", back_edges);

        // 3. Find all simple cycle paths and group them
        let cycles = graph_lib::find_simple_cycles(&graph);
        let mut loops: HashMap<NodeIndex, Loop> = HashMap::new();
        for c in cycles {
            let headers: Vec<NodeIndex> = c.iter().filter(|x| back_edges.contains(x)).map(|x| *x).collect();
            if headers.len() == 1 {
                let header = headers.first().unwrap();
                let body: Vec<LoopNode> = c.iter().filter(|x| *x != header).map(|x| LoopNode::new(*x)).collect();

                if let Some(l) = loops.get_mut(&header) {
                    for b in body {
                        if l.body.iter().any(|x| x.index == b.index) { continue; }
                        l.body.push(b);
                    }
                } else {
                    loops.insert(*header, Loop{ header: *header, body });
                }
            }
        }

        println!("loop entries");
        for (_, l) in loops {
            println!("  loop {:?}", l.header);
            for b in l.body {
                println!("    {:?}", b);
            }
        }
        println!();
    }
}

/*
fn format_ops(indent_level: i32, script: &script::ScriptBlock, labels: &LabelMap, ops: &Vec<code::Operation>) -> String {
    let mut indent = String::new();
    for _ in 0..indent_level {
        indent += "    ";
    }

    let mut result = String::new();
    for op in ops {
        match op {
            code::Operation::Execute(code) => {
                result += format!("{}// Code fragment {:x}..{:x}\n", indent, code.offset, code.offset + code.length).as_str();
                let disasm = disassemble::Disassembler::new(&script, code.offset - script.base);
                for ins in disasm {
                    if ins.offset + ins.bytes.len() > code.offset + code.length { break; }

                    if let Some(label) = labels.get(&ins.offset) {
                        result += format!("{}{}:\n", indent, label).as_str();
                    }

                    let opcode = &ins.opcode;
                    result += format!("{}{:04x} {}\n", indent, ins.offset, opcode.name).as_str();
                }
            },
            code::Operation::IfElse(code, true_branch, false_branch) => {
                let code = format_ops(indent_level + 1, script, labels, code);
                let true_ops = format_ops(indent_level + 1, script, labels, true_branch);
                let false_ops = format_ops(indent_level + 1, script, labels, false_branch);
                result += format!("{}if (\n", indent).as_str();
                result += format!("{}{}", indent, code).as_str();
                result += format!("{}) {{\n", indent).as_str();
                result += format!("{}{}", indent, true_ops).as_str();
                result += format!("{}}} else {{\n", indent).as_str();
                result += format!("{}{}", indent, false_ops).as_str();
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::If(code, true_branch) => {
                let code = format_ops(indent_level + 1, script, labels, code);
                let true_ops = format_ops(indent_level + 1, script, labels, true_branch);
                result += format!("{}if (\n", indent).as_str();
                result += format!("{}{}", indent, code).as_str();
                result += format!("{}) {{\n", indent).as_str();
                result += format!("{}{}", indent, true_ops).as_str();
                result += format!("{}}}\n", indent).as_str();
            },
        }
    }
    result
}
*/

#[derive(Debug,Clone)]
enum Operand {
    Global(usize),
    Local(usize),
    Temp(usize),
    Param(usize),
    Property(usize),
    Imm(usize),
    Acc,
    Prev,
    Sp,
    Tos,
    Rest,
    OpSelf,
    Pc,
    Tmp,
}


#[derive(Debug,Clone)]
enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    ShiftRight,
    ShiftLeft,
    ExclusiveOr,
    BitwiseAnd,
    BitwiseOr,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual,
    UnsignedGreaterThan,
    UnsignedGreaterOrEqual,
    UnsignedLess,
    UnsignedLessOrEqual,
}

#[derive(Debug,Clone)]
enum UnaryOp {
    Negate,
    LogicNot
}

#[derive(Debug,Clone)]
enum Expression {
    Operand(Operand),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
}

#[derive(Debug)]
enum IntermediateCode {
    Assign(Expression, Expression),
    BranchTrue(Expression),
    BranchFalse(Expression),
    BranchAlways(),
    Call(usize, usize),
    KCall(usize, usize),
    CallE(usize, usize, usize),
    Return(),
    Send(Expression, usize),
    Class(usize),
}

fn expr_acc() -> Expression { Expression::Operand(Operand::Acc) }
fn expr_prev() -> Expression { Expression::Operand(Operand::Prev) }
fn expr_sp() -> Expression { Expression::Operand(Operand::Sp) }
fn expr_rest() -> Expression { Expression::Operand(Operand::Rest) }
fn expr_imm(n: usize) -> Expression { Expression::Operand(Operand::Imm(n)) }
fn expr_self() -> Expression { Expression::Operand(Operand::OpSelf) }
fn expr_tos() -> Expression { Expression::Operand(Operand::Tos) }
fn expr_tmp() -> Expression { Expression::Operand(Operand::Tmp) }
fn expr_pc() -> Expression { Expression::Operand(Operand::Pc) }

fn new_box_expr(op: Operand) -> Box<Expression> {
    Box::new(Expression::Operand(op))
}

fn new_box_imm(n: usize) -> Box<Expression> { new_box_expr(Operand::Imm(n)) }
fn new_box_acc() -> Box<Expression> { new_box_expr(Operand::Acc) }
fn new_box_sp() -> Box<Expression> { new_box_expr(Operand::Sp) }
fn new_box_rest() -> Box<Expression> { new_box_expr(Operand::Rest) }
fn new_box_pc() -> Box<Expression> { new_box_expr(Operand::Pc) }
fn new_box_tos() -> Box<Expression> { new_box_expr(Operand::Tos) }

fn adjust_sp_before_call(frame_size: usize) -> Vec<IntermediateCode> {
    // sp -= frame_size + 2 + &rest_modifier, &rest_modifier = 0
    let amount = new_box_imm(frame_size + 2);
    vec![
        IntermediateCode::Assign(expr_sp(), Expression::Binary(BinaryOp::Subtract, new_box_sp(), Box::new(Expression::Binary(BinaryOp::Add, amount, new_box_rest())))),
        IntermediateCode::Assign(expr_rest(), expr_imm(0))
    ]
}

enum What {
    Add(usize),
    Subtract(usize)
}

fn apply_to_op(op: Operand, what: What) -> Vec<IntermediateCode> {
    match what {
        What::Add(n) => {
            vec![ IntermediateCode::Assign(Expression::Operand(op.clone()), Expression::Binary(BinaryOp::Add, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
        What::Subtract(n) => {
            vec![ IntermediateCode::Assign(Expression::Operand(op.clone()), Expression::Binary(BinaryOp::Subtract, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
    }
}

fn do_push(expr: Expression) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.push(IntermediateCode::Assign(expr_tos(), expr));
    result.append(&mut apply_to_op(Operand::Sp, What::Add(1)));
    result
}

fn pre_pop() -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut apply_to_op(Operand::Sp, What::Subtract(1)));
    result
}

fn binary_op_pop_acc(op: BinaryOp) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut pre_pop());
    result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(op, new_box_tos(), new_box_acc())));
    result
}

fn binary_op_acc_pop(op: BinaryOp) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut pre_pop());
    result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(op, new_box_acc(), new_box_tos())));
    result
}

fn instruction_to_intermediate_code(ins: &disassemble::Instruction) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.push(IntermediateCode::Assign(expr_pc(), expr_imm(ins.offset + ins.bytes.len())));
    match ins.bytes.first().unwrap() {
        0x00 | 0x01 => { // bnot
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(BinaryOp::ExclusiveOr, new_box_acc(), new_box_imm(0xffff))));
        },
        0x02 | 0x03 => { // add
            result.append(&mut binary_op_pop_acc(BinaryOp::Add));
        },
        0x04 | 0x05 => { // sub
            result.append(&mut binary_op_pop_acc(BinaryOp::Subtract));
        },
        0x06 | 0x07 => { // mul
            result.append(&mut binary_op_pop_acc(BinaryOp::Multiply));
        },
        0x08 | 0x09 => { // div
            result.append(&mut binary_op_pop_acc(BinaryOp::Divide));
        },
        0x0a | 0x0b => { // mod
            result.append(&mut binary_op_pop_acc(BinaryOp::Modulo));
        },
        0x0c | 0x0d => { // shr
            result.append(&mut binary_op_pop_acc(BinaryOp::ShiftRight));
        },
        0x0e | 0x0f => { // shl
            result.append(&mut binary_op_pop_acc(BinaryOp::ShiftLeft));
        },
        0x10 | 0x11 => { // xor
            result.append(&mut binary_op_pop_acc(BinaryOp::ExclusiveOr));
        },
        0x12 | 0x13 => { // and
            result.append(&mut binary_op_acc_pop(BinaryOp::BitwiseAnd));
        },
        0x14 | 0x15 => { // or
            result.append(&mut binary_op_acc_pop(BinaryOp::BitwiseOr));
        },
        0x16 | 0x17 => { // neg
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Unary(UnaryOp::Negate, new_box_acc())));
        },
        0x18 | 0x19 => { // not
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Unary(UnaryOp::LogicNot, new_box_acc())));
        },
        0x1a | 0x1b => { // eq?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::Equals));
        },
        0x1c | 0x1d => { // ne?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::NotEquals));
        },
        0x1e | 0x1f => { // gt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::GreaterThan));
        },
        0x20 | 0x21 => { // ge?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::GreaterOrEqual));
        },
        0x22 | 0x23 => { // lt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::LessThan));
        },
        0x24 | 0x25 => { // le?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::LessOrEqual));
        },
        0x26 | 0x27 => { // ugt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedGreaterThan));
        },
        0x28 | 0x29 => { // uge?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedGreaterOrEqual));
        },
        0x2a | 0x2b => { // ult?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedLess));
        },
        0x2c | 0x2d => { // ule?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedLessOrEqual));
        },
        0x2e | 0x2f => { // bt
            result.push(IntermediateCode::BranchTrue(expr_acc()));
        },
        0x30 | 0x31 => { // bnt
            result.push(IntermediateCode::BranchFalse(expr_acc()));
        },
        0x32 | 0x33 => { // jmp
            result.push(IntermediateCode::BranchAlways());
        },
        0x34 | 0x35 => { // ldi
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(Operand::Imm(ins.args[0]))));
        },
        0x36 | 0x37 => { // push
            result.append(&mut do_push(expr_acc()));
        },
        0x38 | 0x39 => { // pushi
            result.append(&mut do_push(expr_imm(ins.args[0])));
        },
        0x3a | 0x3b => { // toss
            result.append(&mut apply_to_op(Operand::Sp, What::Subtract(1)));
        },
        0x3c | 0x3d => { // dup
            result.append(&mut pre_pop());
            result.push(IntermediateCode::Assign(expr_tmp(), Expression::Operand(Operand::Tos)));
            result.append(&mut do_push(expr_tmp()));
            result.append(&mut do_push(expr_tmp()));
        },
        0x3e | 0x3f => { // link
            result.push(IntermediateCode::Assign(expr_sp(), Expression::Binary(BinaryOp::Add, new_box_sp(), new_box_imm(ins.args[0]))));
        },
        0x40 | 0x41 => { // call
            let addr = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::Call(addr, frame_size));
        },
        0x42 | 0x43 => { // kcall
            let addr = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::KCall(addr, frame_size));
        },
        0x44 | 0x45 => { // callb
            let script: usize = 0;
            let disp_index = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::CallE(script, disp_index, frame_size));
        },
        0x46 | 0x47 => { // calle
            let script = ins.args[0];
            let disp_index = ins.args[1];
            let frame_size = ins.args[2];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::CallE(script, disp_index, frame_size));
        },
        0x48 | 0x49 => { // ret
            result.push(IntermediateCode::Return());
        },
        0x4a | 0x4b => { // send
            let frame_size = ins.args[0];
            result.push(IntermediateCode::Send(expr_acc(), frame_size));
        },
        0x4c | 0x4d | 0x4e | 0x4f => { // ?
            panic!("invalid opcode (4c/4d/4e/4f)");
        },
        0x50 | 0x51 => { // class
            let func = ins.args[0];
            result.push(IntermediateCode::Class(func));
        },
        0x52 | 0x53 => { // ?
            panic!("invalid opcode (52/53)");
        },
        0x54 | 0x55 => { // self
            let frame_size = ins.args[0];
            result.push(IntermediateCode::Send(expr_self(), frame_size));
        },
        0x56 | 0x57 => { // super
            let class = ins.args[0];
            let frame_size = ins.args[1];
            result.push(IntermediateCode::Send(expr_imm(class), frame_size));
        },
        0x58 | 0x59 => { // &rest
            let _param_index = ins.args[0];
            todo!("&rest");
        },
        0x5a | 0x5b => { // lea
            let _arg = ins.args[0];
            todo!("lea");
        },
        0x5c | 0x5d => { // selfid
            result.push(IntermediateCode::Assign(expr_acc(), expr_self()));
        },
        0x5e | 0x5f => { // ?
            panic!("invalid opcode (5e/5f)");
        },
        0x60 | 0x61 => { // pprev
            result.append(&mut do_push(expr_prev()));
        },
        0x62 | 0x63 => { // ptoa
            let op = Operand::Property(ins.args[0]);
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x64 | 0x65 => { // atop
            let op = Operand::Property(ins.args[0]);
            result.push(IntermediateCode::Assign(Expression::Operand(op), Expression::Operand(Operand::Acc)));
        },
        0x66 | 0x67 => { // ptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x68 | 0x69 => { // stop
            let op = Operand::Property(ins.args[0]);
            result.append(&mut pre_pop());
            result.push(IntermediateCode::Assign(Expression::Operand(op), expr_tos()));
        },
        0x6a | 0x6b => { // iptoa
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Add(1)));
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x6c | 0x6d => { // dptoa
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x6e | 0x6f => { // iptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Add(1)));
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x70 | 0x71 => { // dptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x72 | 0x73 => { // lofsa
            let offset = ins.args[0];
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(BinaryOp::Add, new_box_pc(), new_box_imm(offset))));
        },
        0x74 | 0x75 => { // lofss
            let offset = ins.args[0];
            result.append(&mut do_push(Expression::Binary(BinaryOp::Add, new_box_pc(), new_box_imm(offset))));
        },
        0x76 | 0x77 => { // push0
            result.append(&mut do_push(expr_imm(0)));
        },
        0x78 | 0x79 => { // push1
            result.append(&mut do_push(expr_imm(1)));
        },
        0x7a | 0x7b => { // push2
            result.append(&mut do_push(expr_imm(2)));
        },
        0x7c | 0x7d => { // pushself
            result.append(&mut do_push(expr_self()));
        },
        0x7e | 0x7f => { // ?
            panic!("invalid opcode (7e/7f)");
        },
        opcode @ 0x80..=0xff => {
            let typ = (opcode >> 1) & 3;
            let on_stack = (opcode & 0x8) != 0;
            let acc_modifier = (opcode & 0x10) != 0;
            let oper = (opcode >> 5) & 3;

            let op;
            match typ {
                0 => { op = Operand::Global(ins.args[0]); },
                1 => { op = Operand::Local(ins.args[0]); },
                2 => { op = Operand::Temp(ins.args[0]); },
                3 => { op = Operand::Param(ins.args[0]); },
                _ => { unreachable!() }
            }

            let expr;
            if acc_modifier {
                expr = Expression::Binary(
                         BinaryOp::Add,
                         Box::new(Expression::Operand(op)),
                         Box::new(Expression::Operand(Operand::Acc))
                       );
            } else {
                expr = Expression::Operand(op);
            }

            match oper {
                0 => { // load
                    let dest;
                    if on_stack {
                        result.append(&mut do_push(expr));
                    } else {
                        dest = Operand::Acc;
                        result.push(IntermediateCode::Assign(Expression::Operand(dest), expr));
                    }
                },
                1 => { // store
                    let source;
                    if on_stack {
                        result.append(&mut pre_pop());
                        source = Operand::Tos;
                    } else {
                        source = Operand::Acc;
                    }
                    result.push(IntermediateCode::Assign(expr, Expression::Operand(source)));
                },
                2 => { // inc+load
                    todo!();
                },
                3 => { // dec+load
                    todo!();
                },
                _ => { unreachable!() }
            }
        }
    }
    result
}

fn convert_codefrag_to_ic(script: &script::ScriptBlock, frag: &code::CodeFragment) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script, frag.offset - script.base);
    for ins in disasm {
        if ins.offset + ins.bytes.len() > frag.offset + frag.length { break; }

        let mut ic = instruction_to_intermediate_code(&ins);
        result.append(&mut ic);
    }
    result
}

fn convert_condition_to_ic(script: &script::ScriptBlock, frag: &code::CodeFragment) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script, frag.offset - script.base);
    for ins in disasm {
        if ins.offset + ins.bytes.len() > frag.offset + frag.length { break; }

        let mut ic = instruction_to_intermediate_code(&ins);
        result.append(&mut ic);
    }
    result
}

const STACK_SIZE: usize = 32;

#[derive(Debug)]
struct VMState {
    acc: Expression,
    rest: Expression,
    sp: Expression,
    prev: Expression,
    pc: Expression,
    tmp: Expression,
    stack: Vec<Expression>
}

impl VMState {
    fn new() -> Self {
        let zero = expr_imm(0);
        let mut stack: Vec<Expression> = Vec::new();
        for _ in 0..STACK_SIZE {
            stack.push(expr_imm(0).clone());
        }
        VMState{ acc: zero.clone(), pc: zero.clone(), rest: zero.clone(), prev: zero.clone(), sp: zero.clone(), tmp: zero.clone(), stack }
    }

}

fn get_imm_value(expr: &Expression) -> Option<usize> {
    if let Expression::Operand(op) = expr {
        if let Operand::Imm(n) = op {
            return Some(*n);
        }
    }
    None
}

fn format_expr(expr: &Expression) -> String {
    if let Some(v) = get_imm_value(expr) {
        return format!("{}", v).to_string();
    }
    format!("{:?}", expr).to_string()
}

impl fmt::Display for VMState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut stack: String = String::new();
        let stack_size: usize;
        if let Some(sp) = get_imm_value(&self.sp) {
            stack_size = sp;
        } else {
            stack_size = STACK_SIZE - 1;
        }
        for n in 0..=stack_size {
            stack += &format!(" {}", format_expr(&self.stack[n])).to_string();
        }
        write!(f, "acc: {} pc: {}: rest: {} prev: {}, sp: {} stack{}", format_expr(&self.acc), format_expr(&self.pc), format_expr(&self.rest), format_expr(&self.prev), format_expr(&self.sp), stack)
    }
}

#[derive(Debug)]
enum ResultOp {
    AssignProperty(usize, Expression),
}

enum BranchIf {
    Never,
    True(Expression),
    False(Expression),
}

struct VM {
    ops: Vec<ResultOp>,
    branch: BranchIf,
    state: VMState
}

#[derive(PartialEq,Eq,Hash)]
enum StateEnum {
    Sp,
    Acc,
    Tmp,
}

fn apply_binary_op(op: &BinaryOp, a: Option<usize>, b: Option<usize>) -> Option<usize> {
    if a.is_some() && b.is_some() {
        let a = a.unwrap();
        let b = b.unwrap();
        return Some(match op {
            BinaryOp::Add => { a + b },
            BinaryOp::Subtract => { a - b },
            BinaryOp::Multiply => { a * b },
            BinaryOp::Divide => { a / b },
            BinaryOp::Modulo => { a % b },
            BinaryOp::ShiftRight => { a >> b },
            BinaryOp::ShiftLeft => { a << b },
            BinaryOp::ExclusiveOr => { a ^ b },
            BinaryOp::BitwiseAnd => { a & b },
            BinaryOp::BitwiseOr => { a | b },
            BinaryOp::Equals => { if a == b { 1 } else { 0 } },
            BinaryOp::NotEquals => { if a != b { 1 } else { 0 } },
            BinaryOp::GreaterThan => { if a > b { 1 } else { 0 } },
            BinaryOp::GreaterOrEqual => { if a >= b { 1 } else { 0 } },
            BinaryOp::LessThan => { if a < b { 1 } else { 0 } },
            BinaryOp::LessOrEqual => { if a <= b { 1 } else { 0 } },
            BinaryOp::UnsignedGreaterThan => { if a > b { 1 } else { 0 } },
            BinaryOp::UnsignedGreaterOrEqual => { if a >= b { 1 } else { 0 } },
            BinaryOp::UnsignedLess => { if a < b { 1 } else { 0 } },
            BinaryOp::UnsignedLessOrEqual => { if a <= b { 1 } else { 0 } },
        });
    }
    None
}

fn expr_to_value(state: &VMState, expr: &Expression) -> Option<usize> {
    return match expr {
        Expression::Operand(Operand::Imm(n)) => { Some(*n) },
        Expression::Operand(Operand::Param(_)) => { None },
        Expression::Binary(op, a, b) => {
            let a = expr_to_value(state, a);
            let b = expr_to_value(state, b);
            apply_binary_op(op, a, b)
        },
        _ => { todo!("expr_to_value: {:?}", expr); }
    }
}

fn simplify_expr2(state: &mut VMState, state_seen: &mut HashSet<StateEnum>, expr: Expression) -> Expression {
    match expr {
        Expression::Operand(Operand::Acc) => {
            if !state_seen.contains(&StateEnum::Acc) {
                state_seen.insert(StateEnum::Acc);
                return simplify_expr2(state, state_seen, state.acc.clone());
            }
            return state.acc.clone();
        },
        Expression::Operand(Operand::Sp) => {
            if !state_seen.contains(&StateEnum::Sp) {
                state_seen.insert(StateEnum::Sp);
                return simplify_expr2(state, state_seen, state.sp.clone());
            }
            return state.acc.clone();
        },
        Expression::Operand(Operand::Tmp) => {
            if !state_seen.contains(&StateEnum::Tmp) {
                state_seen.insert(StateEnum::Tmp);
                return simplify_expr2(state, state_seen, state.tmp.clone());
            }
            return state.acc.clone();
        },
        Expression::Operand(Operand::Tos) => {
            if let Some(index) = expr_to_value(state, &state.sp) {
                let tos = state.stack[index].clone();
                //println!("simplify_expr2: reading tos index {} -> {:?}", index, tos);
                return simplify_expr2(state, state_seen, tos);
            }
            panic!("cannot resolve sp value for tos");
        },
        Expression::Operand(Operand::Imm(_)) => { return expr.clone(); },
        Expression::Operand(Operand::Param(_)) => { return expr.clone(); },
        Expression::Operand(Operand::Global(_)) => { return expr.clone(); },
        Expression::Operand(Operand::Local(_)) => { return expr.clone(); },
        Expression::Operand(Operand::Temp(_)) => { return expr.clone(); },
        Expression::Binary(op, a, b) => {
            let a = simplify_expr2(state, state_seen, *a);
            let b = simplify_expr2(state, state_seen, *b);
            return Expression::Binary(op, Box::new(a), Box::new(b));
        },
        Expression::Unary(op, a) => {
            let a = simplify_expr2(state, state_seen, *a);
            return Expression::Unary(op, Box::new(a));
        },
        _ => { todo!("simplify_expr2: {:?}", expr); }
    }
}

fn simplify_expr(state: &mut VMState, expr: &Expression) -> Expression {
    simplify_expr2(state, &mut HashSet::new(), expr.clone())
}

impl VM {
    fn new() -> Self {
        VM{ ops: Vec::new(), branch: BranchIf::Never, state: VMState::new() }
    }

    fn execute(&mut self, ic: &IntermediateCode) {
        match ic {
            IntermediateCode::Assign(dest, expr) => {
                match dest {
                    Expression::Operand(Operand::Acc) => {
                        self.state.acc = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.acc) {
                            self.state.acc = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Prev) => {
                        self.state.prev = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.prev) {
                            self.state.prev = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Sp) => {
                        self.state.sp = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.sp) {
                            self.state.sp = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Rest) => {
                        self.state.rest = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.rest) {
                            self.state.rest = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Tmp) => {
                        self.state.tmp = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.tmp) {
                            self.state.tmp = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Pc) => {
                        self.state.pc = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.pc) {
                            self.state.pc = Expression::Operand(Operand::Imm(v));
                        }
                    },
                    Expression::Operand(Operand::Tos) => {
                        if let Some(index) = expr_to_value(&self.state, &self.state.sp) {
                            self.state.stack[index] = simplify_expr(&mut self.state, expr);
                        } else {
                            panic!("could not simplify sp");
                        }
                    },
                    Expression::Operand(Operand::Property(n)) => {
                        self.ops.push(ResultOp::AssignProperty(*n, expr.clone()));
                    },
                    _ => { panic!("todo: Assign({:?}, {:?}", dest, expr); }
                }
            },
            IntermediateCode::BranchFalse(expr) => {
                let expr = simplify_expr(&mut self.state, expr);
                if let BranchIf::Never = self.branch {
                    self.branch = BranchIf::False(expr.clone());
                } else {
                    panic!();
                }
            }
            IntermediateCode::BranchTrue(expr) => {
                let expr = simplify_expr(&mut self.state, expr);
                if let BranchIf::Never = self.branch {
                    self.branch = BranchIf::True(expr.clone());
                } else {
                    panic!();
                }
            }
            op @ _ => { panic!("vm execute todo {:?}", op); }
        }
    }
}

fn convert_ops_to_ic(script: &script::ScriptBlock, ops: &Vec<code::Operation>) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, _, _) => {
                println!("todo IfElse({:?})", code);
                let mut x: Vec<IntermediateCode> = Vec::new();
                for op in code {
                    match op {
                        code::Operation::Execute(frag) => {
                            x.append(&mut convert_condition_to_ic(script, frag));
                        },
                        _ => { todo!(); }
                    }
                }
                if DEBUG_VM {
                    println!("about to execute:");
                    for y in &x {
                        println!("  {:?}", y);
                    }
                    println!();
                }

                let mut vm = VM::new();
                for ic in &x {
                    if DEBUG_VM { println!("vm: executing {:?}", ic); }
                    vm.execute(&ic);
                    if DEBUG_VM { println!(">> state is now {}", vm.state); }
                }
            },
            code::Operation::If(_, _) => {
                println!("todo If");
            },
            code::Operation::Execute(frag) => {
                let mut code = convert_codefrag_to_ic(script, frag);
                result.append(&mut code);
            },
        }
    }
    result
}

fn write_code(fname: &str, block: &script::ScriptBlock, _labels: &LabelMap, graph: &code::CodeGraph) -> Result<(), std::io::Error> {
    let mut out_file = File::create(fname).unwrap();
    writeln!(out_file, "// Block at {:x}", block.base)?;

    for n in graph.node_indices() {
        let node = &graph[n];
        if graph.edges_directed(n, Incoming).count() != 0 { continue; }

        if graph.edges_directed(n, Outgoing).count() != 0 {
            println!("TODO: node {:?} does not reduce to a single node", node.as_str());
            continue;
        }

        //println!("root {:?}", node.as_str());
        //let s = format_ops(0, block, labels, &node.ops);
        //writeln!(out_file, "// Node")?;
        //writeln!(out_file, "{}", s)?;

        let ic = convert_ops_to_ic(block, &node.ops);
        for i in &ic {
            println!("{:?}", i);
        }
    }

    writeln!(out_file, "\n")?;
    Ok(())
}

fn main() -> Result<(), ScriptError> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 3 {
        panic!("usage: {} out script_id [offset]", args[0]);
    }

    let extract_path = &args[1];
    let script_id: i16 = args[2].parse().unwrap();
    let script_data = std::fs::read(format!("{}/script.{:03}", extract_path, script_id))?;

    let script_base_offset: Option<usize>;
    if args.len() >= 4 {
        script_base_offset = usize::from_str_radix(&args[3], 16).ok();
    } else {
        script_base_offset = None;
    }

    let vocab_997_data = std::fs::read(format!("{}/vocab.997", extract_path))?;
    let _selector_vocab = vocab::Vocab997::new(&vocab_997_data)?;

    let vocab_000_data = std::fs::read(format!("{}/vocab.000", extract_path))?;
    let _main_vocab = vocab::Vocab000::new(&vocab_000_data)?;

    let script = script::Script::new(script_id, &script_data)?;

    let labels = find_code_labels(&script)?;

    for block in &script.blocks {
        //if block.base != 0x86e { continue; } // 300
        //if block.base != 0xc { continue; } // 300
        //if block.base != 0x1214 { continue; } // 300

        if let Some(base) = script_base_offset {
            if block.base != base { continue; }
        }
        match block.r#type {
            script::BlockType::Code => {
                let code_blocks = split_code_in_blocks(&block, &labels);
                let mut graph = create_graph_from_codeblocks(&code_blocks);
                let out_fname = format!("dot/{:x}.orig.dot", block.base);
                plot_graph(&out_fname, &graph)?;

                reduce::reduce_graph(&mut graph);
                analyse_graph(&graph);

                let out_fname = format!("dot/{:x}.dot", block.base);
                plot_graph(&out_fname, &graph)?;

                let out_fname = format!("tmp/{:x}.txt", block.base);
                write_code(&out_fname, &block, &labels, &graph)?;
            },
            _ => { }
        };
    }

    Ok(())
}
