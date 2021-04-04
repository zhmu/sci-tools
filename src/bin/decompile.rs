extern crate scitools;

use scitools::{disassemble, script, vocab, said, object_class, graph_lib, reduce, code, intermediate, execute};
use std::collections::{HashMap, HashSet};
use std::io::Write;
use std::env;
use std::fs::File;

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
        let ii = intermediate::convert_instruction(&ins);
        let ic = ii.ops.last().unwrap();
        match ic {
            intermediate::IntermediateCode::Call(addr, _) => {
                let label = format!("local_{:x}", addr);
                labels.insert(*addr, label);
            },
            intermediate::IntermediateCode::BranchAlways(addr) | intermediate::IntermediateCode::BranchTrue(addr, _, _) | intermediate::IntermediateCode::BranchFalse(addr, _,  _) => {
                let label = format!("local_{:x}", addr);
                labels.insert(*addr, label);
            },
            _ => { }
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

#[derive(Copy,Clone)]
enum OffsetIndex {
    None,
    Offset(usize),
    Index(usize),
}

struct CodeBlock<'a> {
    script: &'a script::ScriptBlock<'a>,
    code: code::CodeFragment,
    branch_index_always: OffsetIndex,
    branch_index_true: OffsetIndex,
    branch_index_false: OffsetIndex,
}

impl<'a> CodeBlock<'a> {
    fn new(script: &'a script::ScriptBlock, code: code::CodeFragment) -> CodeBlock<'a> {
        CodeBlock{ script, code, branch_index_always: OffsetIndex::None, branch_index_true: OffsetIndex::None, branch_index_false: OffsetIndex::None }
    }
}

fn fill_out_index(offset_index: &mut OffsetIndex, offset_to_index: &HashMap<usize, usize>) -> () {
    match offset_index {
        OffsetIndex::Offset(offset) => {
            match offset_to_index.get(offset) {
                Some(index) => { *offset_index = OffsetIndex::Index(*index) },
                None => { panic!("offset {:x} not found", offset) }
            }
        },
        OffsetIndex::None => { },
        _ => { panic!("not an offset?"); }
    }
}

fn is_unconditional_branch(ii: &intermediate::Instruction) -> Option<usize> {
    let ic = ii.ops.last().unwrap();
    return match ic {
        intermediate::IntermediateCode::BranchAlways(a) => { Some(*a) },
        _ => { None }
    }
}

fn is_conditional_branch(ii: &intermediate::Instruction) -> Option<(usize, usize)> {
    let ic = ii.ops.last().unwrap();
    return match ic {
        intermediate::IntermediateCode::BranchTrue(a, b, _) => { Some((*a, *b)) },
        intermediate::IntermediateCode::BranchFalse(a, b, _) => { Some((*a, *b)) },
        _ => { None }
    }
}

fn must_split(ii: &intermediate::Instruction) -> bool {
    let ic = ii.ops.last().unwrap();
    return match ic {
        intermediate::IntermediateCode::BranchAlways(_) => { true },
        intermediate::IntermediateCode::BranchTrue(_, _, _) => { true },
        intermediate::IntermediateCode::BranchFalse(_, _, _) => { true },
        intermediate::IntermediateCode::Return() => { true },
        _ => { false }
    }
}

fn split_instructions_from_block(block: &mut CodeBlock, offset: usize) -> Vec<intermediate::Instruction> {
    for (n, ii) in block.code.instructions.iter().enumerate() {
        if ii.offset == offset {
            return block.code.instructions.drain(n..).collect();
        }
    }
    unreachable!();
}

fn split_code_in_blocks<'a>(script_block: &'a script::ScriptBlock, labels: &LabelMap) -> Vec<CodeBlock<'a>> {
    let mut blocks: Vec<CodeBlock> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script_block, 0);
    let mut current_block_offset: usize = script_block.base;
    let mut block_offsets: HashSet<usize> = HashSet::new();

    // Split on specific instruction
    let mut instructions: Vec<intermediate::Instruction> = Vec::new();
    for ins in disasm {
        instructions.push(intermediate::convert_instruction(&ins));
        let ii = instructions.last().unwrap().clone();

        if !must_split(&ii) { continue; }
        let next_offset = ii.offset + ii.length;
        let length = next_offset - current_block_offset;

        let mut block = CodeBlock::new(&script_block, code::CodeFragment{ offset: current_block_offset, length, instructions: instructions.drain(..).collect() });

        if let Some(next_offset) = is_unconditional_branch(&ii) {
            block.branch_index_always = OffsetIndex::Offset(next_offset);
        } else if let Some((true_offset, next_offset)) = is_conditional_branch(&ii) {
            block.branch_index_true = OffsetIndex::Offset(true_offset);
            block.branch_index_false = OffsetIndex::Offset(next_offset);
        }

        let block_offset = block.code.offset;
        blocks.push(block);
        block_offsets.insert(block_offset);
        current_block_offset = next_offset;
    }

    // Sometimes a lone 'bnot.w' (0) is added to the instructions; this is the
    // only unprocessed instructions block we will accept
    if !instructions.is_empty() {
        assert_eq!(instructions.len(), 1);
        let ii = instructions.first().unwrap();
        assert_eq!(script_block.data[ii.offset - script_block.base], 0x00);
    }

    // Ensure all block_offsets are split
    for (offset, _label) in labels {
        if block_offsets.contains(offset) { continue; }
        let offset = *offset;
        for (n, block) in &mut blocks.iter_mut().enumerate() {
            if offset >= block.code.offset && offset < block.code.offset + block.code.length {
                let instructions = split_instructions_from_block(block, offset);
                let mut new_block = CodeBlock::new(&script_block, code::CodeFragment{ offset, length: block.code.length - (offset - block.code.offset), instructions });
                block.code.length = offset - block.code.offset;
                // Move branches to new block
                new_block.branch_index_true = block.branch_index_true;
                new_block.branch_index_false = block.branch_index_false;
                new_block.branch_index_always = block.branch_index_always;
                // Ensure the previous block unconditionally branches to the new one
                block.branch_index_true = OffsetIndex::None;
                block.branch_index_false = OffsetIndex::None;
                block.branch_index_always = OffsetIndex::Offset(offset);

                blocks.insert(n + 1, new_block);
                break;
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
        if let OffsetIndex::Index(index) = block.branch_index_always {
            graph.add_edge(node_map[&block_nr], node_map[&index], code::CodeEdge{ branch: code::Branch::Always });
        }
        if let OffsetIndex::Index(index) = block.branch_index_true {
            graph.add_edge(node_map[&block_nr], node_map[&index], code::CodeEdge{ branch: code::Branch::True });
        }
        if let OffsetIndex::Index(index) = block.branch_index_false {
            graph.add_edge(node_map[&block_nr], node_map[&index], code::CodeEdge{ branch: code::Branch::False });
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

fn convert_ops_to_ic(ops: &Vec<code::Operation>) -> Vec<intermediate::Instruction> {
    let mut result: Vec<intermediate::Instruction> = Vec::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, _, _) => {
                println!("todo IfElse({:?})", code);
                let mut x: Vec<intermediate::Instruction> = Vec::new();
                for op in code {
                    match op {
                        code::Operation::Execute(frag) => {
                            x.append(&mut frag.instructions.clone());
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

                let mut vm = execute::VM::new();
                for ic in &x {
                    if DEBUG_VM { println!("vm: executing {:?}", ic); }
                    for op in &ic.ops {
                        vm.execute(&op);
                    }
                    if DEBUG_VM { println!(">> state is now {}", vm.state); }
                }
            },
            code::Operation::If(_, _) => {
                println!("todo If");
            },
            code::Operation::Execute(frag) => {
                let mut code = frag.instructions.clone();
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

        let ic = convert_ops_to_ic(&node.ops);
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
