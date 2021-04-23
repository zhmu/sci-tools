use petgraph::graph::{Graph, NodeIndex};
use petgraph::visit::{NodeRef, EdgeRef, IntoNodeReferences};
use petgraph::visit::NodeIndexable;

use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use crate::{script, intermediate};

#[derive(Clone,Debug)]
pub struct CodeFragment {
    pub instructions: Vec<intermediate::Instruction>,
}

impl CodeFragment {
    pub fn get_start_offset(&self) -> u16 {
        let ii = self.instructions.first().unwrap();
        ii.offset
    }

    pub fn get_end_offset(&self) -> u16 {
        let ii = self.instructions.last().unwrap();
        (ii.offset as usize + ii.length) as u16
    }

    pub fn as_str(&self) -> String {
        format!("{:x}..{:x}", self.get_start_offset(), self.get_end_offset())
    }
}


#[derive(Copy,Clone,Debug,PartialEq)]
pub enum Branch {
    Always,
    True,
    False
}

#[derive(Copy,Clone,Debug)]
pub struct CodeEdge {
    pub branch: Branch
}

#[derive(Clone,Debug)]
pub enum Operation {
    // Code
    Execute(CodeFragment),
    // Code, TrueBranch, FalseBranch
    IfElse(Vec<Operation>, Vec<Operation>, Vec<Operation>),
    // Code, TrueBranch
    If(Vec<Operation>, Vec<Operation>),
}

pub fn as_str(ops: &Vec<Operation>) -> String {
    let mut r = String::new();
    for op in ops {
        r += &op.as_str();
        r += "\\n";
    }
    r
}

impl Operation {
    pub fn as_str(&self) -> String {
        return match self {
            Operation::Execute(fragment) => { format!("execute {}", fragment.as_str()) },
            Operation::If(op1, op2) => { format!("if({}) {{ {} }}", as_str(op1), as_str(op2)) },
            Operation::IfElse(op1, op2, op3) => { format!("if({}) {{ {} }} else {{ {} }}", as_str(op1), as_str(op2), as_str(op3) ) },
        }
    }
}

#[derive(Clone)]
pub struct CodeNode<'a> {
    pub script: &'a script::ScriptBlock<'a>,
    pub ops: Vec<Operation>
}


impl<'a> CodeNode<'a> {
    pub fn as_str(&self) -> String {
        as_str(&self.ops)
    }
}

pub type CodeGraph<'a> = Graph<CodeNode<'a>, CodeEdge>;

#[derive(Copy,Clone)]
pub enum OffsetIndex {
    None,
    Offset(u16),
    Index(usize),
}

pub struct CodeBlock<'a> {
    pub script: &'a script::ScriptBlock<'a>,
    pub code: CodeFragment,
    pub branch_index_always: OffsetIndex,
    pub branch_index_true: OffsetIndex,
    pub branch_index_false: OffsetIndex,
}

impl<'a> CodeBlock<'a> {
    pub fn new(script: &'a script::ScriptBlock, code: CodeFragment) -> CodeBlock<'a> {
        CodeBlock{ script, code, branch_index_always: OffsetIndex::None, branch_index_true: OffsetIndex::None, branch_index_false: OffsetIndex::None }
    }
}

pub fn create_graph_from_codeblocks<'a>(blocks: &'a Vec<CodeBlock<'a>>) -> CodeGraph<'a> {
    let mut graph = CodeGraph::new();
    let mut node_map: HashMap<usize, petgraph::prelude::NodeIndex> = HashMap::new();
    for (block_nr, block) in blocks.iter().enumerate() {
        let code_block = CodeNode{ script: block.script, ops: vec![ Operation::Execute(block.code.clone()) ]};
        let n = graph.add_node(code_block);
        node_map.insert(block_nr, n);
    }

    for (block_nr, block) in blocks.iter().enumerate() {
        if let OffsetIndex::Index(index) = block.branch_index_always {
            graph.add_edge(node_map[&block_nr], node_map[&index], CodeEdge{ branch: Branch::Always });
        }
        if let OffsetIndex::Index(index) = block.branch_index_true {
            graph.add_edge(node_map[&block_nr], node_map[&index], CodeEdge{ branch: Branch::True });
        }
        if let OffsetIndex::Index(index) = block.branch_index_false {
            graph.add_edge(node_map[&block_nr], node_map[&index], CodeEdge{ branch: Branch::False });
        }
    }

    graph
}

fn is_single_execute<'a>(node: &'a CodeNode) -> Option<&'a CodeFragment> {
    if node.ops.len() == 1 {
        if let Operation::Execute(code) = node.ops.first().unwrap() {
            return Some(code)
        }
    }
    None
}

pub fn plot_graph<F>(fname: &str, graph: &CodeGraph, format_label: F) -> Result<(), std::io::Error>
    where F: Fn(NodeIndex) -> String
{
    let mut out_file = File::create(fname).unwrap();
    writeln!(out_file, "digraph G {{")?;
    for node in graph.node_references() {
        let shape: &str;
        let mut label: String;
        let weight = node.weight();
        label = format!("{}", graph.to_index(node.id()));
        if let Some(code) = is_single_execute(weight) {
            shape = "oval";
            let start_offset = code.get_start_offset();
            let end_offset = code.get_end_offset();
            label += format!(" [{:x}..{:x}]", start_offset, end_offset).as_str();
        } else {
            shape = "box";
            for o in &weight.ops {
                label += format!(" {}", o.as_str()).as_str();
            }
        }
        label += format_label(node.id()).as_str();
        writeln!(out_file, "  {} [ label=\"{}\" shape=\"{}\"]", graph.to_index(node.id()), label, shape)?;
    }
    for edge in graph.edge_references() {
        let e = edge.weight();
        let colour: &str;
        match e.branch {
            Branch::True => { colour = "green"; },
            Branch::False => { colour = "red"; },
            Branch::Always => { colour = "black"; },
        }
        writeln!(out_file, "  {} -> {} [ color={} ]", graph.to_index(edge.source()), graph.to_index(edge.target()), colour)?;
    }
    writeln!(out_file, "}}")?;
    Ok(())
}


