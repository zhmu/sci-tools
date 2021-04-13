use crate::{intermediate, code, execute};

use petgraph::graph::NodeIndex;

use std::collections::HashSet;

#[derive(Debug,PartialEq,Eq,Hash,Copy,Clone)]
enum UsedRegister {
    Acc,
    Stack(usize)
}

fn find_regs_in_expr2(state: &execute::VMState, expr: &intermediate::Expression, regs: &mut HashSet<UsedRegister>) {
    match expr {
        intermediate::Expression::Operand(op) => {
            match op {
                intermediate::Operand::Acc => { regs.insert(UsedRegister::Acc); },
                intermediate::Operand::Tos => {
                    if let Some(sp) = execute::expr_to_value(&state, &state.sp) {
                        regs.insert(UsedRegister::Stack(sp));
                    } else {
                        panic!("cannot resolve sp value for tos");
                    }
                },
                _ => { }
            }
        },
        intermediate::Expression::Binary(_, left, right) => {
            find_regs_in_expr2(&state, left, regs);
            find_regs_in_expr2(&state, right, regs);
        },
        intermediate::Expression::Unary(_, expr) => {
            find_regs_in_expr2(&state, expr, regs);
        },
        intermediate::Expression::Address(_) => { },
    }
}

fn find_regs_in_expr(state: &execute::VMState, expr: &intermediate::Expression) -> HashSet<UsedRegister> {
    let mut result: HashSet<UsedRegister> = HashSet::new();
    find_regs_in_expr2(state, expr, &mut result);
    result
}

fn remove_unreachable_stack_regs(input: &mut HashSet<UsedRegister>, sp: usize) -> HashSet<UsedRegister> {
    let mut result: HashSet<UsedRegister> = HashSet::new();
    for reg in input.drain() {
        match reg {
            UsedRegister::Stack(n) => {
                if n < sp { result.insert(UsedRegister::Stack(n)); }
            },
            _ => { result.insert(reg); }
        }
    }
    result
}

fn process_expr_to_input_regs(state: &execute::VMState, expr: &intermediate::Expression, inputs: &mut HashSet<UsedRegister>, outputs: &HashSet<UsedRegister>) {
    for reg in find_regs_in_expr(&state, &expr) {
        if outputs.contains(&reg) { continue; }
        println!("expr-regs {:?} -> {:?}", expr, reg);
        inputs.insert(reg);
    }
}

fn analyse_instructions(frag: &code::CodeFragment) -> (HashSet<UsedRegister>, HashSet<UsedRegister>) {
    println!("analyse_instructions: {:x}..{:x}", frag.get_start_offset(), frag.get_end_offset());

    let mut state = execute::VMState::new();
    let mut inputs: HashSet<UsedRegister> = HashSet::new();
    let mut outputs: HashSet<UsedRegister> = HashSet::new();

    for ins in &frag.instructions {
        for op in &ins.ops {
            println!(">> op {:?}", op);
            match op {
                intermediate::IntermediateCode::Assign(op, expr) => {
                    process_expr_to_input_regs(&state, expr, &mut inputs, &outputs);
                    match op {
                        intermediate::Operand::Acc => {
                            state.acc = execute::simplify_expr(&mut state, expr);
                            if let Some(val) = execute::expr_to_value(&state, &state.acc) {
                                println!("acc <- {:?} [{:?}]", val, expr);
                            } else {
                                println!("cannot resolve acc value for assign {:?}", state.acc);
                            }
                            outputs.insert(UsedRegister::Acc);
                        },
                        intermediate::Operand::Sp => {
                            state.sp = execute::simplify_expr(&mut state, expr);
                            if let Some(val) = execute::expr_to_value(&state, &state.sp) {
                                println!("sp <- {:?}", val);
                            } else {
                                panic!("cannot resolve sp value for assign");
                            }
                        },
                        intermediate::Operand::Tos => {
                            if let Some(sp) = execute::expr_to_value(&state, &state.sp) {
                                println!("stack[{}] <- {:?}", sp, expr);
                                outputs.insert(UsedRegister::Stack(sp));
                            } else {
                                panic!("cannot resolve sp value for tos");
                            }
                        },
                        _ => { }
                    }
                },
                intermediate::IntermediateCode::BranchTrue{ taken_offset: _, next_offset: _, expr } |
                intermediate::IntermediateCode::BranchFalse{ taken_offset: _, next_offset: _, expr } => {
                    process_expr_to_input_regs(&state, expr, &mut inputs, &outputs);
                },
                intermediate::IntermediateCode::BranchAlways(_) => { },
                intermediate::IntermediateCode::Call(_, _) | intermediate::IntermediateCode::CallE(_, _, _) => {
                    // For now, let's assume that calls always change the accumulator
                    outputs.insert(UsedRegister::Acc);
                },
                _ => { println!("TODO {:?}", op); }
            }
        }
    }


    if let Some(sp) = execute::expr_to_value(&state, &state.sp) {
        println!("pre-filter {:?} sp {}", outputs, sp);
        outputs = remove_unreachable_stack_regs(&mut outputs, sp);
        println!("post-filter {:?}", outputs);
    } else {
        panic!("cannot resolve sp value");
    }

    println!("analysis done, inputs {:?} outputs {:?}", inputs, outputs);
    println!("");
    (inputs, outputs)
}

fn analyse_graph_inout_node(graph: &code::CodeGraph, n: NodeIndex) -> (HashSet<UsedRegister>, HashSet<UsedRegister>) {
    let node = &graph[n];

    assert_eq!(1, node.ops.len());
    let op = node.ops.first().unwrap();
    if let code::Operation::Execute(frag) = op {
        return analyse_instructions(&frag);
    } else {
        unreachable!();
    }
}

pub fn analyse_inout(graph: &mut code::CodeGraph) {
    for n in graph.node_indices() {
        println!("analyzing node {:?}", n);
        let (in_used, out_used) = analyse_graph_inout_node(graph, n);
        println!("node {:?}: in_used {:?} out_used {:?}", n, in_used, out_used);
    }
}
