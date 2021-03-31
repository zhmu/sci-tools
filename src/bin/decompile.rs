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

fn convert_condition_to_ic(script: &script::ScriptBlock, frag: &code::CodeFragment) -> Vec<intermediate::IntermediateCode> {
    let mut result: Vec<intermediate::IntermediateCode> = Vec::new();
    let disasm = disassemble::Disassembler::new(&script, frag.offset - script.base);
    for ins in disasm {
        if ins.offset + ins.bytes.len() > frag.offset + frag.length { break; }

        let mut ic = intermediate::instruction_to_intermediate_code(&ins);
        result.append(&mut ic);
    }
    result
}

fn convert_ops_to_ic(script: &script::ScriptBlock, ops: &Vec<code::Operation>) -> Vec<intermediate::IntermediateCode> {
    let mut result: Vec<intermediate::IntermediateCode> = Vec::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, _, _) => {
                println!("todo IfElse({:?})", code);
                let mut x: Vec<intermediate::IntermediateCode> = Vec::new();
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

                let mut vm = execute::VM::new();
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
                let mut code = intermediate::convert_codefrag_to_ic(script, frag);
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
