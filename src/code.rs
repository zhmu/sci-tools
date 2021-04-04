use petgraph::graph::Graph;

use crate::{script, intermediate};

#[derive(Clone,Debug)]
pub struct CodeFragment {
    pub offset: usize, // absolute (not relative to script base)
    pub length: usize,
    pub instructions: Vec<intermediate::Instruction>,
}

impl CodeFragment {
    pub fn as_str(&self) -> String {
        format!("{:x}..{:x}", self.offset, self.offset + self.length)
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
