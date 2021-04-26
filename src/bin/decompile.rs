extern crate scitools;

use scitools::{script, vocab, said, object_class, graph_lib, reduce, code, execute, split, label, flow, intermediate};
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

fn split_if_code<'a>(ops: &'a Vec<code::Operation>) -> (&'a [intermediate::Instruction], &'a [intermediate::Instruction]) {
    assert_eq!(1, ops.len());

    if let code::Operation::Execute(frag) = ops.first().unwrap() {
        let n = frag.instructions.len();
        return (&frag.instructions[0..n-1], &frag.instructions[n-1..]);
    } else {
        unreachable!();
    }
}

fn convert_instructions(state: &mut execute::VMState, indent: &str, instructions: &[intermediate::Instruction]) -> String {
    let mut vm = execute::VM::new(&state);
    for ins in instructions {
        for op in &ins.ops {
            if DEBUG_VM {
                println!(">> execute {:04x} {:?} -- current sp {:?}", ins.offset, op, vm.state.sp);
            }
            vm.execute(&op);
        }
    }
    *state = vm.state;
    let mut result: String = String::new();
    for rop in &vm.ops {
        result += format!("{}{}\n", indent, format_rop(rop)).as_str();
    }
    match vm.branch {
        execute::BranchIf::True(_) | execute::BranchIf::False(_) => { unreachable!() },
        execute::BranchIf::Never => { }
    }
    result
}

fn format_operand(op: &intermediate::Operand) -> String {
    match op {
        intermediate::Operand::Global(expr) => {
            let expr = format_expression(expr);
            format!("global({})", expr)
        },
        intermediate::Operand::Local(expr) => {
            let expr = format_expression(expr);
            format!("local({})", expr)
        },
        intermediate::Operand::Temp(expr) => {
            let expr = format_expression(expr);
            format!("temp({})", expr)
        },
        intermediate::Operand::Param(expr) => {
            let expr = format_expression(expr);
            format!("param({})", expr)
        },
        intermediate::Operand::Property(expr) => {
            let expr = format_expression(expr);
            format!("property({})", expr)
        },
        intermediate::Operand::Imm(val) => {
            format!("{}", val)
        },
        intermediate::Operand::HelperVariable(num) => {
            format!("v{}", num)
        },
        intermediate::Operand::Acc => { "acc".to_string() },
        intermediate::Operand::Prev => { "prev".to_string() },
        intermediate::Operand::Sp => { "sp".to_string() },
        intermediate::Operand::Tos => { "tos".to_string() },
        intermediate::Operand::Rest => { "rest".to_string() },
        intermediate::Operand::OpSelf => { "self".to_string() },
        intermediate::Operand::Tmp => { "tmp".to_string() }
    }
}

fn format_expression(expr: &intermediate::Expression) -> String {
    match expr {
        intermediate::Expression::Undefined => { "undefined".to_string() },
        intermediate::Expression::Operand(op) => { format_operand(op) },
        intermediate::Expression::Binary(op, expr1, expr2) => {
            let expr1 = format_expression(expr1);
            let expr2 = format_expression(expr2);
            let op = match op {
                intermediate::BinaryOp::Add => { "+" },
                intermediate::BinaryOp::Subtract => { "-" },
                intermediate::BinaryOp::Multiply => { "*" },
                intermediate::BinaryOp::Divide => { "/" },
                intermediate::BinaryOp::Modulo => { "%" },
                intermediate::BinaryOp::ShiftRight => { ">>" },
                intermediate::BinaryOp::ShiftLeft => { "<<" },
                intermediate::BinaryOp::ExclusiveOr => { "^" },
                intermediate::BinaryOp::BitwiseAnd => { "&" },
                intermediate::BinaryOp::BitwiseOr => { "|"},
                intermediate::BinaryOp::Equals => { "==" },
                intermediate::BinaryOp::NotEquals => { "!=" },
                intermediate::BinaryOp::GreaterThan => { ">" },
                intermediate::BinaryOp::GreaterOrEqual => { ">=" },
                intermediate::BinaryOp::LessThan => { "<" },
                intermediate::BinaryOp::LessOrEqual => { "<=" },
                intermediate::BinaryOp::UnsignedGreaterThan => { "u>"},
                intermediate::BinaryOp::UnsignedGreaterOrEqual => { "u>=" },
                intermediate::BinaryOp::UnsignedLess => { "u<" },
                intermediate::BinaryOp::UnsignedLessOrEqual => { "u<=" },
            };
            format!("{} {} {}", expr1, op, expr2)
        },
        intermediate::Expression::Unary(op, expr) => {
            let expr = format_expression(expr);
            let op = match op {
                intermediate::UnaryOp::Negate => { "-" },
                intermediate::UnaryOp::LogicNot => { "!" },
            };
            format!("{} {}", op, expr)
        },
        intermediate::Expression::Address(expr) => {
            let expr = format_expression(expr);
            format!("&({})", expr)
        },
        intermediate::Expression::Class(val) => {
            format!("class({})", val)
        },
        intermediate::Expression::KCall(val, frame_size) => {
            format!("kcall({}, {})", val, frame_size)
        }
    }
}

fn format_expression_vec(v: &Vec<intermediate::Expression>) -> String {
    let mut result: String = String::new();
    for p in v {
        if !result.is_empty() {
            result += ", ";
        }
        result += &format_expression(p);
    }
    result
}

fn format_rop(op: &execute::ResultOp) -> String {
    match op {
        execute::ResultOp::AssignProperty(dest, expr) => {
            let dest = format_expression(dest);
            let expr = format_expression(expr);
            format!("property({}) = {}", dest, expr)
        },
        execute::ResultOp::AssignGlobal(dest, expr) => {
            let dest = format_expression(dest);
            let expr = format_expression(expr);
            format!("global({}) = {}", dest, expr)
        },
        execute::ResultOp::AssignTemp(dest, expr) => {
            let dest = format_expression(dest);
            let expr = format_expression(expr);
            format!("temp({}) = {}", dest, expr)
        },
        execute::ResultOp::AssignLocal(dest, expr) => {
            let dest = format_expression(dest);
            let expr = format_expression(expr);
            format!("local({}) = {}", dest, expr)
        },
        execute::ResultOp::AssignParam(dest, expr) => {
            let dest = format_expression(dest);
            let expr = format_expression(expr);
            format!("param({}) = {}", dest, expr)
        },
        execute::ResultOp::AssignHelperVar(n, expr) => {
            let expr = format_expression(expr);
            format!("v{} = {}", n, expr)
        },
        execute::ResultOp::CallE(script_num, disp_index, params) => {
            let params = format_expression_vec(params);
            format!("callE({}, {}, {})", script_num, disp_index, params)
        },
        execute::ResultOp::Call(offset, params) => {
            let params = format_expression_vec(params);
            format!("call({}, {})", offset, params)
        },
        execute::ResultOp::KCall(num, params) => {
            let params = format_expression_vec(params);
            format!("callK({}, {})", num, params)
        },
        execute::ResultOp::Send(dest, selector, params) => {
            let dest = format_expression(dest);
            let selector = format_expression(selector);
            let params = format_expression_vec(params);
            format!("send({}, {}, {})", dest, selector, params)
        },
        execute::ResultOp::Incomplete(msg) => {
            format!("incomplete!({})", msg)
        },
        execute::ResultOp::Return() => { "return".to_string() },
    }
}

fn convert_conditional(state: &mut execute::VMState, indent: &str, instructions: &[intermediate::Instruction]) -> String {
    let mut vm = execute::VM::new(&state);
    for ins in instructions {
        for op in &ins.ops {
            if DEBUG_VM {
                println!(">> execute {:04x} {:?} -- current sp {:?}", ins.offset, op, vm.state.sp);
            }
            vm.execute(&op);
        }
    }
    *state = vm.state;
    let mut result: String = String::new();
    for rop in &vm.ops {
        result += format!("{}{}\n", indent, format_rop(rop)).as_str();
    }
    match vm.branch {
        execute::BranchIf::True(expr) => {
            result += format!("{}TRUE CONDITION {:?}\n", indent, format_expression(&expr)).as_str();
        },
        execute::BranchIf::False(expr) => {
            result += format!("{}FALSE CONDITION {:?}\n", indent, format_expression(&expr)).as_str();
        },
        execute::BranchIf::Never => { unreachable!() }
    }
    result
}

fn convert_code(state: &mut execute::VMState, ops: &Vec<code::Operation>, level: i32) -> String {
    let mut indent = String::new();
    for _ in 0..level { indent += "    "; }

    let mut result = String::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, true_code, false_code) => {
                assert_eq!(1, code.len());
                let (code, if_condition) = split_if_code(&code);

                let code = convert_instructions(state, &indent, code);
                let if_code = convert_conditional(state, format!("{}    ", indent).as_str(), if_condition);
                let if_code = if_code.trim();

                let mut true_state = state.clone();
                let mut false_state = state.clone();
                let true_code = convert_code(&mut true_state, &true_code, level + 1);
                let false_code = convert_code(&mut false_state, &false_code, level + 1);
                result += &code;
                result += format!("{}if ({}) {{\n", indent, if_code).as_str();
                result += &true_code;
                result += format!("{}}} else {{\n", indent).as_str();
                result += &false_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::If(code, true_code) => {
                assert_eq!(1, code.len());
                let (code, if_condition) = split_if_code(&code);
                let code = convert_instructions(state, &indent, code);
                let if_code = convert_conditional(state, format!("{}    ", indent).as_str(), if_condition);
                let if_code = if_code.trim();

                let mut true_state = state.clone();
                let true_code = convert_code(&mut true_state, &true_code, level + 1);
                result += &code;
                result += format!("{}if ({}) {{\n", indent, if_code).as_str();
                result += &true_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::Execute(frag) => {
                result += format!("{}// {:x} .. {:x}\n", indent, frag.get_start_offset(), frag.get_end_offset()).as_str();
                result += &convert_instructions(state, &indent, &frag.instructions);
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

                flow::analyse_inout(&mut graph);

                let out_fname = format!("dot/{:x}.orig.dot", block.base);
                code::plot_graph(&out_fname, &graph, |_| { "".to_string() })?;

                reduce::reduce_graph(&mut graph);
                analyse_graph(&graph);

                let out_fname = format!("dot/{:x}.dot", block.base);
                code::plot_graph(&out_fname, &graph, |_| { "".to_string() })?;

                let out_fname = format!("tmp/{:x}.txt", block.base);
                write_code(&out_fname, &block, &labels, &graph)?;
            },
            _ => { }
        };
    }

    Ok(())
}
