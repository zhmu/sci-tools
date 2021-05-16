use crate::{intermediate, code, execute, sci, class_defs};

use petgraph::graph::NodeIndex;
use petgraph::visit::EdgeRef;
use petgraph::Incoming;

use std::collections::{HashSet, HashMap};

#[derive(Debug,PartialEq,Eq,Hash,Copy,Clone)]
enum UsedRegister {
    Acc
}

struct InOut {
        inputs: HashSet<UsedRegister>,
        outputs: HashSet<UsedRegister>,
}

const DEBUG_FLOW: bool = false;

fn find_regs_in_expr2(state: &execute::VMState, expr: &intermediate::Expression, regs: &mut HashSet<UsedRegister>) {
    match expr {
        intermediate::Expression::Operand(op) => {
            match op {
                intermediate::Operand::Acc => { regs.insert(UsedRegister::Acc); },
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
        intermediate::Expression::Class(_) => { },
        intermediate::Expression::Undefined => { },
        intermediate::Expression::Rest(..) => { },
    }
}

fn find_regs_in_expr(state: &execute::VMState, expr: &intermediate::Expression) -> HashSet<UsedRegister> {
    let mut result: HashSet<UsedRegister> = HashSet::new();
    find_regs_in_expr2(state, expr, &mut result);
    result
}

fn process_expr_to_input_regs(state: &execute::VMState, expr: &intermediate::Expression, inputs: &mut HashSet<UsedRegister>, outputs: &HashSet<UsedRegister>) {
    for reg in find_regs_in_expr(&state, &expr) {
        if outputs.contains(&reg) { continue; }
        inputs.insert(reg);
    }
}

fn analyse_instructions(frag: &code::CodeFragment, class_definitions: &class_defs::ClassDefinitions) -> InOut {
    if DEBUG_FLOW { println!("analyse_instructions: {:x}..{:x}", frag.get_start_offset(), frag.get_end_offset()); }

    let mut inputs: HashSet<UsedRegister> = HashSet::new();
    let mut outputs: HashSet<UsedRegister> = HashSet::new();

    let mut vm = execute::VM::new(&execute::VMState::new(), class_definitions);

    for ins in &frag.instructions {
        for op in &ins.ops {
            if DEBUG_FLOW { println!(">> op {:?} state {}", op, vm.state); }
            vm.execute(&op);
            if DEBUG_FLOW { println!(">> post op state {}", vm.state); }

            match op {
                intermediate::IntermediateCode::Assign(op, expr) => {
                    process_expr_to_input_regs(&vm.state, expr, &mut inputs, &outputs);
                    match op {
                        intermediate::Operand::Acc => {
                            outputs.insert(UsedRegister::Acc);
                        },
                        //intermediate::Operand::Sp => { },
/*
                        intermediate::Operand::Tos => {
                            if let Some(sp) = execute::expr_to_value(&vm.state, &vm.state.sp) {
                                outputs.insert(UsedRegister::Stack(sp));
                            } else {
                                panic!("cannot resolve sp value for tos");
                            }
                        },
*/
                        _ => { }
                    }
                },
                intermediate::IntermediateCode::Push(..) => { },
                intermediate::IntermediateCode::Branch{ taken_offset: _, next_offset: _, cond } => {
                    process_expr_to_input_regs(&vm.state, cond, &mut inputs, &outputs);
                },
                intermediate::IntermediateCode::BranchAlways(_) => { },
                intermediate::IntermediateCode::Call(_, _) | intermediate::IntermediateCode::CallE(_, _, _) => {
                    // For now, let's assume that calls always change the accumulator
                    outputs.insert(UsedRegister::Acc);
                },
                intermediate::IntermediateCode::KCall(nr, _) => {
                    if !sci::does_kcall_return_void(*nr) {
                        outputs.insert(UsedRegister::Acc);
                    }
                },
                intermediate::IntermediateCode::Return(..) => { },
                intermediate::IntermediateCode::Send(_, values) => {
                    let mut n: usize = 0;
                    while n + 1 < values.len() {
                        let selector = &values[n];
                        let num_values = &values[n + 1];
                        if let Some(num_values) = execute::expr_to_value(&vm.state, &num_values) {
                            let num_values = num_values as usize;
                            if let Some(selector) = execute::expr_to_value(&vm.state, &selector) {
                                if num_values == 0 && class_definitions.is_certainly_propery(selector) {
                                    outputs.insert(UsedRegister::Acc);
                                } else if class_definitions.is_certainly_func(selector) {
                                    outputs.insert(UsedRegister::Acc);
                                }
                            } else {
                                println!("couldn't resolve selector values in send call {:?}", selector);
                            }
                            n += 2 + num_values;
                        } else {
                            println!("couldn't resolve num values in send call {:?} - not analysing further", num_values);
                            break;
                        }
                    }
                }
            }
        }
    }

    InOut{ inputs, outputs }
}

fn analyse_graph_inout_node(class_definitions: &class_defs::ClassDefinitions, graph: &code::CodeGraph, n: NodeIndex) -> InOut {
    let node = &graph[n];

    assert_eq!(1, node.ops.len());
    let op = node.ops.first().unwrap();
    if let code::Operation::Execute(frag) = op {
        return analyse_instructions(&frag, class_definitions);
    } else {
        unreachable!();
    }
}

fn intersection(u: &mut HashSet<UsedRegister>, v: &HashSet<UsedRegister>)
{
    u.retain(|i| { v.contains(&i) });
}

fn append_assign_to_helper(node: &mut code::CodeNode, var_index: usize, op: &intermediate::Operand) {
    if let code::Operation::Execute(frag) = node.ops.last_mut().unwrap() {
        let ins = intermediate::IntermediateCode::Assign(
                    intermediate::Operand::HelperVariable(var_index),
                    intermediate::Expression::Operand(op.clone()));

        // If the last instruction is a branch, we must place our instruction in front of it -
        // otherwise, our instruction is the final value and must be in front
        let last_ops = &mut frag.instructions.last_mut().unwrap().ops;
        match last_ops.last().unwrap() {
            intermediate::IntermediateCode::Branch{ taken_offset: _,  next_offset: _, cond: _}
            | intermediate::IntermediateCode::BranchAlways(_) => {
                let last_op = last_ops.pop().unwrap();
                last_ops.push(ins);
                last_ops.push(last_op);
            },
            _ => {
                last_ops.push(ins);
            },
        }
    } else {
        unreachable!();
    }
}

fn prepend_assign_to_helper(node: &mut code::CodeNode, var_index: usize, op: intermediate::Operand) {
    if let code::Operation::Execute(frag) = node.ops.first_mut().unwrap() {
        let first_instruction = frag.instructions.first_mut().unwrap();
        let mut current_ops: Vec<_> = first_instruction.ops.drain(..).collect();
        first_instruction.ops = vec![
            intermediate::IntermediateCode::Assign(
                op,
                intermediate::Expression::Operand(
                    intermediate::Operand::HelperVariable(var_index)
                )
            )
        ];
        first_instruction.ops.append(&mut current_ops);
    } else {
        unreachable!();
    }
}

fn map_usedregister_to_op(reg: UsedRegister) -> intermediate::Operand {
    return match reg {
        UsedRegister::Acc => { intermediate::Operand::Acc },
    }
}

pub fn analyse_inout(graph: &mut code::CodeGraph, class_definitions: &class_defs::ClassDefinitions) {
    let mut result: HashMap<NodeIndex, InOut> = HashMap::new();

    for n in graph.node_indices() {
        if DEBUG_FLOW { println!("analyzing node {:?}", n); }
        let in_out = analyse_graph_inout_node(class_definitions, graph, n);
        if DEBUG_FLOW { println!("node {:?}: in_used {:?} out_used {:?}", n, in_out.inputs, in_out.outputs); }
        result.insert(n, in_out);
    }

    if DEBUG_FLOW {
        code::plot_graph("dot/foo.dot", graph, |n| {
            let in_out = &result[&n];
            format!("\\nin {:?} out {:?}", in_out.inputs, in_out.outputs)
        }).expect("could not write debug flow graph");
    }

    let mut var_index: usize = 1;
    for n in graph.node_indices() {
        let n_in_out = &result[&n];
        if n_in_out.inputs.is_empty() { continue; }
        let incoming_nodes: Vec<NodeIndex> = graph.edges_directed(n, Incoming).map(|e| e.source()).collect();
        if incoming_nodes.len() < 2 { continue; }

        let mut incoming_common_out: HashSet<UsedRegister> = HashSet::new();
        for e in &incoming_nodes {
            let source_in_out = &result[&e];
            if incoming_common_out.is_empty() {
                incoming_common_out = source_in_out.outputs.clone();
            } else {
                intersection(&mut incoming_common_out, &source_in_out.outputs);
            }
        }

        let mut registers_to_store = n_in_out.inputs.clone();
        intersection(&mut registers_to_store, &incoming_common_out);
        if DEBUG_FLOW {
            println!("flow analysis {:?} inputs {:?} common_outputs {:?} -> store {:?}", n, n_in_out.inputs, incoming_common_out, registers_to_store);
        }

        for op in registers_to_store {
            let op = map_usedregister_to_op(op);

            // Store the result in a helper variable for all input nodes
            for n in &incoming_nodes {
                append_assign_to_helper(&mut graph[*n], var_index, &op);
            }

            // Store the input in the helper variable
            prepend_assign_to_helper(&mut graph[n], var_index, op);
        }

        var_index += 1;
    }
}
