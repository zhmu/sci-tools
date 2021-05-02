extern crate scitools;

use scitools::{script, vocab, said, object_class, graph_lib, reduce, code, execute, split, label, flow, intermediate, print, class_defs};
use std::collections::HashMap;
use std::io::Write;
use std::env;
use std::fs::File;
use std::convert::TryInto;

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

fn convert_instructions(state: &mut execute::VMState, formatter: &print::Formatter, class_definitions: &class_defs::ClassDefinitions, indent: &str, instructions: &[intermediate::Instruction]) -> String {
    let mut vm = execute::VM::new(&state, class_definitions);
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
        result += format!("{}{}\n", indent, formatter.format_rop(rop)).as_str();
    }
    match vm.branch {
        execute::BranchIf::True(_) | execute::BranchIf::False(_) => { unreachable!() },
        execute::BranchIf::Never => { }
    }
    result
}

fn just_prepend_logic_not(expr: &intermediate::Expression) -> intermediate::Expression {
    intermediate::Expression::Unary(intermediate::UnaryOp::LogicNot, Box::new(expr.clone()))
}

fn invert_boolean_expression(expr: &intermediate::Expression) -> intermediate::Expression {
    match expr {
        intermediate::Expression::Binary(op, expr1, expr2) => {
            let op = match op {
                intermediate::BinaryOp::Equals => { intermediate::BinaryOp::NotEquals },
                intermediate::BinaryOp::NotEquals => { intermediate::BinaryOp::Equals },
                intermediate::BinaryOp::GreaterThan => { intermediate::BinaryOp::LessOrEqual},
                intermediate::BinaryOp::GreaterOrEqual => { intermediate::BinaryOp::LessThan },
                intermediate::BinaryOp::LessThan => { intermediate::BinaryOp::GreaterOrEqual },
                intermediate::BinaryOp::LessOrEqual => { intermediate::BinaryOp::GreaterThan },
                intermediate::BinaryOp::UnsignedGreaterThan => { intermediate::BinaryOp::UnsignedLessOrEqual },
                intermediate::BinaryOp::UnsignedGreaterOrEqual => { intermediate::BinaryOp::UnsignedLess },
                intermediate::BinaryOp::UnsignedLess => { intermediate::BinaryOp::UnsignedGreaterOrEqual },
                intermediate::BinaryOp::UnsignedLessOrEqual => { intermediate::BinaryOp::UnsignedGreaterThan },
                _ => {
                    return just_prepend_logic_not(expr);
                }
            };
            intermediate::Expression::Binary(op, expr1.clone(), expr2.clone())
        },
        _ => { just_prepend_logic_not(expr) }
    }
}

fn convert_conditional(state: &mut execute::VMState, formatter: &print::Formatter, class_definitions: &class_defs::ClassDefinitions, indent: &str, instructions: &[intermediate::Instruction]) -> String {
    let mut vm = execute::VM::new(&state, class_definitions);
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
        result += format!("{}{}\n", indent, formatter.format_rop(rop)).as_str();
    }
    match vm.branch {
        execute::BranchIf::True(expr) => {
            result += format!("{}{:?}\n", indent, formatter.format_expression(&expr)).as_str();
        },
        execute::BranchIf::False(expr) => {
            let expr = invert_boolean_expression(&expr);
            result += format!("{}{:?}\n", indent, formatter.format_expression(&expr)).as_str();
        },
        execute::BranchIf::Never => { unreachable!() }
    }
    result
}

fn convert_code(state: &mut execute::VMState, formatter: &print::Formatter, class_definitions: &class_defs::ClassDefinitions, ops: &Vec<code::Operation>, level: i32) -> String {
    let mut indent = String::new();
    for _ in 0..level { indent += "    "; }

    let mut result = String::new();
    for op in ops {
        match op {
            code::Operation::IfElse(code, true_code, false_code) => {
                assert_eq!(1, code.len());
                let (code, if_condition) = split_if_code(&code);

                let code = convert_instructions(state, formatter, class_definitions, &indent, code);
                let if_code = convert_conditional(state, formatter, class_definitions, format!("{}    ", indent).as_str(), if_condition);
                let if_code = if_code.trim();

                let mut true_state = state.clone();
                let mut false_state = state.clone();
                let true_code = convert_code(&mut true_state, formatter, class_definitions, &true_code, level + 1);
                let false_code = convert_code(&mut false_state, formatter, class_definitions, &false_code, level + 1);
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
                let code = convert_instructions(state, formatter, class_definitions, &indent, code);
                let if_code = convert_conditional(state, formatter, class_definitions, format!("{}    ", indent).as_str(), if_condition);
                let if_code = if_code.trim();

                let mut true_state = state.clone();
                let true_code = convert_code(&mut true_state, formatter, class_definitions, &true_code, level + 1);
                result += &code;
                result += format!("{}if ({}) {{\n", indent, if_code).as_str();
                result += &true_code;
                result += format!("{}}}\n", indent).as_str();
            },
            code::Operation::Execute(frag) => {
                result += format!("{}// {:x} .. {:x}\n", indent, frag.get_start_offset(), frag.get_end_offset()).as_str();
                result += &convert_instructions(state, formatter, class_definitions, &indent, &frag.instructions);
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

fn find_offset(op: &code::Operation) -> u16 {
    match op {
        code::Operation::Execute(frag) => { frag.get_start_offset() },
        code::Operation::IfElse(code, _, _) => { find_offset(code.first().unwrap()) }
        code::Operation::If(code, _) => { find_offset(code.first().unwrap()) }
    }
}

fn write_code(int_file: &mut std::fs::File, out_file: &mut std::fs::File, formatter: &print::Formatter, class_definitions: &class_defs::ClassDefinitions, graph: &code::CodeGraph) -> Result<(), std::io::Error> {
    for n in graph.node_indices() {
        let node = &graph[n];
        if graph.edges_directed(n, Incoming).count() != 0 { continue; }

        let base = find_offset(&node.ops.first().unwrap());
        let label = formatter.get_label(base);

        writeln!(int_file, "// Block at {:x}", base)?;
        writeln!(int_file, "{} {{", label)?;
        writeln!(out_file, "// Block at {:x}", base)?;
        writeln!(out_file, "{} {{", label)?;

        if graph.edges_directed(n, Outgoing).count() == 0 {
            writeln!(int_file, "{}", format_ops(&node.ops, 1))?;
            let mut state = execute::VMState::new();
            writeln!(out_file, "{}", convert_code(&mut state, &formatter, class_definitions, &node.ops, 1))?;
        } else {
            let msg = format!("    TODO(node {:?} does not reduce to a single node)", node.as_str());
            writeln!(int_file, "{}", msg)?;
            writeln!(out_file, "{}", msg)?;
        }

        writeln!(int_file, "}}\n")?;
        writeln!(out_file, "}}\n")?;

    }
    Ok(())
}

fn add_object_class_labels(o: &object_class::ObjectClass, sel_vocab: &vocab::Vocab997, labels: &mut label::LabelMap) {
    for f in &o.functions {
        let sel_name = sel_vocab.get_selector_name(f.selector.into());
        let name = format!("{}::{}", o.name, sel_name);
        labels.insert(f.offset, name);
    }
}

fn write_object_class(out_file: &mut std::fs::File, sel_vocab: &vocab::Vocab997, class_definitions: &class_defs::ClassDefinitions, o: &object_class::ObjectClass) -> Result<(), ScriptError> {
    let is_class;
    let oc_type;
    match o.r#type {
        object_class::ObjectClassType::Class => { oc_type = "class"; is_class = true; },
        object_class::ObjectClassType::Object => { oc_type = "object"; is_class = false; },
    }
    let species = o.get_species();
    let species_class = class_definitions.find_class(species).unwrap();

    let inherits_from: String;
    if species != 0 {
        inherits_from = format!(" : {}", species_class.name);
    } else {
         inherits_from = "".to_string();
    }

    writeln!(out_file, "{} {}{} {{", oc_type, o.name, inherits_from)?;
    let property_vec = species_class.get_class_properties(sel_vocab);
    for (n, p) in o.properties.iter().enumerate() {
        if is_class {
            writeln!(out_file, "    property, selector: {} selector_id: {}", p.selector, p.selector_id.unwrap())?;
        } else {
            if n < property_vec.len() {
                writeln!(out_file, "    property({}) {} = {}", n, property_vec[n].0, p.selector)?;
            } else {
                writeln!(out_file, "    property({}) ??? OUT OF RANGE {}", n, p.selector)?;
            }
        }
    }

    for f in &o.functions {
        let sel_name = sel_vocab.get_selector_name(f.selector.into());
        writeln!(out_file, "    function, selector: {} ({}) offset: {:x}", sel_name, f.selector, f.offset)?;
    }

    writeln!(out_file, "}}\n")?;

    Ok(())
}

fn add_said_labels(s: &said::Said, labels: &mut label::LabelMap) {
    for s in &s.items {
        let name = format!("said_{:x}", s.offset);
        labels.insert(s.offset.try_into().unwrap(), name);
    }
}

fn write_said(out_file: &mut std::fs::File, s: &said::Said) -> Result<(), std::io::Error> {
    writeln!(out_file, "said {{")?;
    for s in &s.items {
        writeln!(out_file, "    said_{:x} {}", s.offset, s.said)?;
    }
    writeln!(out_file, "}}\n")?;
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
    let selector_vocab = vocab::Vocab997::new(&vocab_997_data)?;

    let vocab_996_data = std::fs::read(format!("{}/vocab.996", extract_path))?;
    let class_vocab = vocab::Vocab996::new(&vocab_996_data)?;

    let class_definitions = class_defs::ClassDefinitions::new(extract_path.to_string(), &class_vocab);

    let vocab_000_data = std::fs::read(format!("{}/vocab.000", extract_path))?;
    let main_vocab = vocab::Vocab000::new(&vocab_000_data)?;

    let script = script::Script::new(script_id, &script_data)?;

    let mut labels = label::find_code_labels(&script);

    let out_path = "tmp";
    let mut int_file = File::create(format!("{}/{}.intermediate.txt", out_path, script_id))?;
    let mut out_file = File::create(format!("{}/{}.txt", out_path, script_id))?;

    let mut object_classes: Vec<object_class::ObjectClass> = Vec::new();
    let mut saids: Vec<said::Said> = Vec::new();
    for block in &script.blocks {
        match block.r#type {
            script::BlockType::Object => {
                let object_class = object_class::ObjectClass::new(&script, &block, false)?;
                add_object_class_labels(&object_class, &selector_vocab, &mut labels);
                object_classes.push(object_class);
            },
            script::BlockType::Class => {
                let object_class = object_class::ObjectClass::new(&script, &block, true)?;
                add_object_class_labels(&object_class, &selector_vocab, &mut labels);
                object_classes.push(object_class);
            },
            script::BlockType::Said => {
                let said = said::Said::new(&block, &main_vocab)?;
                add_said_labels(&said, &mut labels);
                saids.push(said);
            },
            _ => { }
        }
    }

    let formatter = print::Formatter::new(&labels, &selector_vocab, &class_definitions);
    for block in &script.blocks {
        if let Some(base) = script_base_offset {
            if block.base != base { continue; }
        }
        match block.r#type {
            script::BlockType::Code => {
                let code_blocks = split::split_code_in_blocks(&block, &labels);
                let mut graph = code::create_graph_from_codeblocks(&code_blocks);

                flow::analyse_inout(&mut graph, &class_definitions);

                let out_fname = format!("dot/{:x}.orig.dot", block.base);
                code::plot_graph(&out_fname, &graph, |_| { "".to_string() })?;

                reduce::reduce_graph(&mut graph);
                analyse_graph(&graph);

                let out_fname = format!("dot/{:x}.dot", block.base);
                code::plot_graph(&out_fname, &graph, |_| { "".to_string() })?;

                write_code(&mut int_file, &mut out_file, &formatter, &class_definitions, &graph)?;
            },
            _ => { }
        };
    }

    for o in &object_classes { write_object_class(&mut out_file, &selector_vocab, &class_definitions, &o)?; }
    for s in &saids { write_said(&mut out_file, &s)?; }

    Ok(())
}
