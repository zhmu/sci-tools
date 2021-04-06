extern crate scitools;

use scitools::{script, vocab, said, object_class, graph_lib, reduce, code, execute, split, label};
use std::collections::HashMap;
use std::io::Write;
use std::env;
use std::fs::File;

use petgraph::graph::NodeIndex;
use petgraph::algo::kosaraju_scc;
use petgraph::{Incoming, Outgoing};

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

fn convert_code(state: &mut execute::VMState, ops: &Vec<code::Operation>, level: i32) -> String {
    let mut indent = String::new();
    for _ in 0..level { indent += "    "; }

    let mut result = String::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, true_code, false_code) => {
                let code = convert_code(state, &code, level + 1);
                let mut true_state = state.clone();
                let mut false_state = state.clone();
                let true_code = convert_code(&mut true_state, &true_code, level + 1);
                let false_code = convert_code(&mut false_state, &false_code, level + 1);
                result += format!("{}if (\n", indent).as_str();
                result += &code;
                result += format!("{}) then {{\n", indent).as_str();
                result += &true_code;
                result += format!("{}}} else {{\n", indent).as_str();
                result += &false_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::If(code, true_code) => {
                let code = convert_code(state, &code, level + 1);
                let mut true_state = state.clone();
                let true_code = convert_code(&mut true_state, &true_code, level + 1);
                result += format!("{}if (\n", indent).as_str();
                result += &code;
                result += format!("{}) then {{\n", indent).as_str();
                result += &true_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::Execute(frag) => {
                result += format!("{}// {:x} .. {:x}\n", indent, frag.get_start_offset(), frag.get_end_offset()).as_str();

                let mut vm = execute::VM::new(&state);
                for ins in &frag.instructions {
                    for op in &ins.ops {
                        if DEBUG_VM {
                            println!(">> execute {:04x} {:?} -- current sp {:?}", ins.offset, op, vm.state.sp);
                        }
                        vm.execute(&op);
                    }
                }
                *state = vm.state;

                for rop in &vm.ops {
                    result += format!("{}{:?}\n", indent, rop).as_str();
                }
                match vm.branch {
                    execute::BranchIf::True(expr) => {
                        result += format!("{}TRUE CONDITION {:?}\n", indent, expr).as_str();
                    },
                    execute::BranchIf::False(expr) => {
                        result += format!("{}FALSE CONDITION {:?}\n", indent, expr).as_str();
                    },
                    execute::BranchIf::Never => { }
                }
            },
        }
    }
    result
}

fn format_ops(ops: &Vec<code::Operation>, level: i32) -> String {
    let mut indent = String::new();
    for _ in 0..level { indent += "    "; }

    let mut result = String::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, true_code, false_code) => {
                let code = format_ops(&code, level + 1);
                let true_code = format_ops(&true_code, level + 1);
                let false_code = format_ops(&false_code, level + 1);
                result += format!("{}if (\n", indent).as_str();
                result += &code;
                result += format!("{}) then {{\n", indent).as_str();
                result += &true_code;
                result += format!("{}}} else {{\n", indent).as_str();
                result += &false_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::If(code, true_code) => {
                let code = format_ops(&code, level + 1);
                let true_code = format_ops(&true_code, level + 1);
                result += format!("{}if (\n", indent).as_str();
                result += &code;
                result += format!("{}) then {{\n", indent).as_str();
                result += &true_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::Execute(frag) => {
                result += format!("{}// {:x} .. {:x}\n", indent, frag.get_start_offset(), frag.get_end_offset()).as_str();
                for ii in &frag.instructions {
                    result += format!("{}{:?}\n", indent, ii.ops).as_str();
                }
            },
        }
    }
    result
}

fn write_code(fname: &str, block: &script::ScriptBlock, _labels: &label::LabelMap, graph: &code::CodeGraph) -> Result<(), std::io::Error> {
    let mut out_file = File::create(fname).unwrap();
    writeln!(out_file, "// Block at {:x}", block.base)?;

    for n in graph.node_indices() {
        let node = &graph[n];
        if graph.edges_directed(n, Incoming).count() != 0 { continue; }

        if graph.edges_directed(n, Outgoing).count() != 0 {
            println!("TODO: node {:?} does not reduce to a single node", node.as_str());
            continue;
        }

        println!("intermediate code:");
        println!("{}", format_ops(&node.ops, 0));
        println!();
        println!("converted code:");
        let mut state = execute::VMState::new();
        println!("{}", convert_code(&mut state, &node.ops, 0));
        println!();
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

    let labels = label::find_code_labels(&script);

    for block in &script.blocks {
        if let Some(base) = script_base_offset {
            if block.base != base { continue; }
        }
        match block.r#type {
            script::BlockType::Code => {
                let code_blocks = split::split_code_in_blocks(&block, &labels);
                let mut graph = code::create_graph_from_codeblocks(&code_blocks);
                let out_fname = format!("dot/{:x}.orig.dot", block.base);
                code::plot_graph(&out_fname, &graph)?;

                reduce::reduce_graph(&mut graph);
                analyse_graph(&graph);

                let out_fname = format!("dot/{:x}.dot", block.base);
                code::plot_graph(&out_fname, &graph)?;

                let out_fname = format!("tmp/{:x}.txt", block.base);
                write_code(&out_fname, &block, &labels, &graph)?;
            },
            _ => { }
        };
    }

    Ok(())
}
