use crate::{intermediate};

use std::fmt;
use std::collections::HashSet;

const STACK_SIZE: intermediate::Value = 32;

#[derive(Debug,Clone)]
pub struct VMState {
    pub acc: intermediate::Expression,
    rest: intermediate::Expression,
    pub sp: intermediate::Expression,
    prev: intermediate::Expression,
    tmp: intermediate::Expression,
    stack: Vec<intermediate::Expression>
}

impl VMState {
    pub fn new() -> Self {
        let zero = intermediate::Expression::Operand(intermediate::Operand::Imm(0));
        let mut stack: Vec<intermediate::Expression> = Vec::new();
        for _ in 0..STACK_SIZE {
            stack.push(zero.clone());
        }
        VMState{ acc: zero.clone(), rest: zero.clone(), prev: zero.clone(), sp: zero.clone(), tmp: zero.clone(), stack }
    }
}

fn get_imm_value(expr: &intermediate::Expression) -> Option<intermediate::Register> {
    if let intermediate::Expression::Operand(op) = expr {
        if let intermediate::Operand::Imm(n) = op {
            return Some(*n);
        }
    }
    None
}

fn format_expr(expr: &intermediate::Expression) -> String {
    if let Some(v) = get_imm_value(expr) {
        return format!("{}", v).to_string();
    }
    format!("{:?}", expr).to_string()
}

impl fmt::Display for VMState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut stack: String = String::new();
        let stack_size: intermediate::Value;
        if let Some(sp) = get_imm_value(&self.sp) {
            stack_size = sp;
        } else {
            stack_size = STACK_SIZE - 1;
        }
        for n in 0..=stack_size {
            stack += &format!(" {}", format_expr(&self.stack[n as usize])).to_string();
        }
        write!(f, "acc: {} rest: {} prev: {}, sp: {} stack{}", format_expr(&self.acc), format_expr(&self.rest), format_expr(&self.prev), format_expr(&self.sp), stack)
    }
}

#[derive(Debug)]
pub enum ResultOp {
    AssignProperty(intermediate::Expression, intermediate::Expression),
    AssignGlobal(intermediate::Expression, intermediate::Expression),
    AssignTemp(intermediate::Expression, intermediate::Expression),
    CallE(intermediate::Value, intermediate::Value, Vec<intermediate::Expression>),
    Call(intermediate::Value, Vec<intermediate::Expression>),
    KCall(intermediate::Value, Vec<intermediate::Expression>),
    Return(),
}

pub enum BranchIf {
    Never,
    True(intermediate::Expression),
    False(intermediate::Expression),
}

pub struct VM {
    pub ops: Vec<ResultOp>,
    pub branch: BranchIf,
    pub state: VMState
}

#[derive(PartialEq,Eq,Hash)]
enum StateEnum {
    Sp,
    Acc,
    Tmp,
    Rest
}

fn apply_binary_op(op: &intermediate::BinaryOp, a: Option<intermediate::Value>, b: Option<intermediate::Value>) -> Option<intermediate::Value> {
    if a.is_some() && b.is_some() {
        let a = a.unwrap();
        let b = b.unwrap();
        return Some(match op {
            intermediate::BinaryOp::Add => { a + b },
            intermediate::BinaryOp::Subtract => { a - b },
            intermediate::BinaryOp::Multiply => { a * b },
            intermediate::BinaryOp::Divide => { a / b },
            intermediate::BinaryOp::Modulo => { a % b },
            intermediate::BinaryOp::ShiftRight => { a >> b },
            intermediate::BinaryOp::ShiftLeft => { a << b },
            intermediate::BinaryOp::ExclusiveOr => { a ^ b },
            intermediate::BinaryOp::BitwiseAnd => { a & b },
            intermediate::BinaryOp::BitwiseOr => { a | b },
            intermediate::BinaryOp::Equals => { if a == b { 1 } else { 0 } },
            intermediate::BinaryOp::NotEquals => { if a != b { 1 } else { 0 } },
            intermediate::BinaryOp::GreaterThan => { if a > b { 1 } else { 0 } },
            intermediate::BinaryOp::GreaterOrEqual => { if a >= b { 1 } else { 0 } },
            intermediate::BinaryOp::LessThan => { if a < b { 1 } else { 0 } },
            intermediate::BinaryOp::LessOrEqual => { if a <= b { 1 } else { 0 } },
            intermediate::BinaryOp::UnsignedGreaterThan => { if a > b { 1 } else { 0 } },
            intermediate::BinaryOp::UnsignedGreaterOrEqual => { if a >= b { 1 } else { 0 } },
            intermediate::BinaryOp::UnsignedLess => { if a < b { 1 } else { 0 } },
            intermediate::BinaryOp::UnsignedLessOrEqual => { if a <= b { 1 } else { 0 } },
        });
    }
    None
}

fn apply_unary_op(op: &intermediate::UnaryOp, a: Option<intermediate::Value>) -> Option<intermediate::Value> {
    if a.is_some() {
        let a = a.unwrap();
        return Some(match op {
            intermediate::UnaryOp::Negate => { todo!() },
            intermediate::UnaryOp::LogicNot => { if a == 0 { 1 } else { 0 }}
        })
    }
    None
}

pub fn expr_to_value(state: &VMState, expr: &intermediate::Expression) -> Option<intermediate::Value> {
    return match expr {
        intermediate::Expression::Operand(intermediate::Operand::Imm(n)) => { Some(*n) },
        intermediate::Expression::Operand(intermediate::Operand::Param(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::Global(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::Temp(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::Local(_)) => { None },
        intermediate::Expression::Binary(op, a, b) => {
            let a = expr_to_value(state, a);
            let b = expr_to_value(state, b);
            apply_binary_op(op, a, b)
        },
        intermediate::Expression::Unary(op, a) => {
            let a = expr_to_value(state, a);
            apply_unary_op(op, a)
        },
        _ => { todo!("expr_to_value: {:?}", expr); }
    }
}

fn simplify_expr2(state: &mut VMState, state_seen: &mut HashSet<StateEnum>, expr: intermediate::Expression) -> intermediate::Expression {
    match expr {
        intermediate::Expression::Operand(intermediate::Operand::Acc) => {
            if !state_seen.contains(&StateEnum::Acc) {
                state_seen.insert(StateEnum::Acc);
                return simplify_expr2(state, state_seen, state.acc.clone());
            }
            return state.acc.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Sp) => {
            if !state_seen.contains(&StateEnum::Sp) {
                state_seen.insert(StateEnum::Sp);
                return simplify_expr2(state, state_seen, state.sp.clone());
            }
            return state.acc.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Tmp) => {
            if !state_seen.contains(&StateEnum::Tmp) {
                state_seen.insert(StateEnum::Tmp);
                return simplify_expr2(state, state_seen, state.tmp.clone());
            }
            return state.tmp.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Rest) => {
            if !state_seen.contains(&StateEnum::Rest) {
                state_seen.insert(StateEnum::Rest);
                return simplify_expr2(state, state_seen, state.rest.clone());
            }
            return state.rest.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Tos) => {
            if let Some(index) = expr_to_value(state, &state.sp) {
                assert_eq!(index % 2, 0);
                let tos = state.stack[(index / 2) as usize].clone();
                //println!("simplify_expr2: reading tos index {} -> {:?}", index / 2, tos);
                return simplify_expr2(state, state_seen, tos);
            }
            panic!("cannot resolve sp value for tos");
        },
        intermediate::Expression::Operand(intermediate::Operand::Imm(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Param(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Global(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Local(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Temp(_)) => { return expr.clone(); },
        intermediate::Expression::Binary(op, a, b) => {
            let a = simplify_expr2(state, state_seen, *a);
            let b = simplify_expr2(state, state_seen, *b);
            return intermediate::Expression::Binary(op, Box::new(a), Box::new(b));
        },
        intermediate::Expression::Unary(op, a) => {
            let a = simplify_expr2(state, state_seen, *a);
            return intermediate::Expression::Unary(op, Box::new(a));
        },
        _ => { todo!("simplify_expr2: {:?}", expr); }
    }
}

pub fn simplify_expr(state: &mut VMState, expr: &intermediate::Expression) -> intermediate::Expression {
    simplify_expr2(state, &mut HashSet::new(), expr.clone())
}

fn gather_params(state: &mut VMState, sp: intermediate::Value, frame_size: intermediate::FrameSize) -> Vec<intermediate::Expression> {
    assert_eq!(sp % 2, 0);

    let mut params: Vec<intermediate::Expression> = Vec::new();
    for n in 0..=(frame_size / 2) {
        let expr = state.stack[(sp / 2 + n) as usize].clone();
        let expr = simplify_expr(state, &expr);
        params.push(expr);
    }
    params
}

impl VM {
    pub fn new(state: &VMState) -> Self {
        VM{ ops: Vec::new(), branch: BranchIf::Never, state: state.clone() }
    }

    pub fn execute(&mut self, ic: &intermediate::IntermediateCode) {
        match ic {
            intermediate::IntermediateCode::Assign(dest, expr) => {
                match dest {
                    intermediate::Operand::Acc => {
                        self.state.acc = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.acc) {
                            self.state.acc = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Prev => {
                        self.state.prev = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.prev) {
                            self.state.prev = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Sp => {
                        self.state.sp = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.sp) {
                            self.state.sp = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Rest => {
                        self.state.rest = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.rest) {
                            self.state.rest = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Tmp => {
                        self.state.tmp = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.tmp) {
                            self.state.tmp = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Tos => {
                        if let Some(index) = expr_to_value(&self.state, &self.state.sp) {
                            assert_eq!(index % 2, 0);
                            self.state.stack[(index / 2) as usize] = simplify_expr(&mut self.state, expr);
                        } else {
                            panic!("could not simplify sp");
                        }
                    },
                    intermediate::Operand::Property(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        self.ops.push(ResultOp::AssignProperty(n, result));
                    },
                    intermediate::Operand::Global(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        self.ops.push(ResultOp::AssignGlobal(n, result));
                    },
                    intermediate::Operand::Temp(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        self.ops.push(ResultOp::AssignTemp(n, result));
                    },
                    _ => { panic!("todo: Assign({:?}, {:?}", dest, expr); }
                }
            },
            intermediate::IntermediateCode::BranchFalse{ taken_offset: _, next_offset: _, expr} => {
                let expr = simplify_expr(&mut self.state, expr);
                if let BranchIf::Never = self.branch {
                    self.branch = BranchIf::False(expr.clone());
                } else {
                    panic!();
                }
            }
            intermediate::IntermediateCode::BranchTrue{ taken_offset: _, next_offset: _, expr} => {
                let expr = simplify_expr(&mut self.state, expr);
                if let BranchIf::Never = self.branch {
                    self.branch = BranchIf::True(expr.clone());
                } else {
                    panic!();
                }
            },
            intermediate::IntermediateCode::BranchAlways(_) => {
                println!("BranchAlways(): don't think we need to do anything here?");
            },
            intermediate::IntermediateCode::Return() => {
                self.ops.push(ResultOp::Return());
            },
            intermediate::IntermediateCode::CallE(script, disp_index, frame_size) => {
                if let Some(sp) = expr_to_value(&self.state, &self.state.sp) {
                    let params = gather_params(&mut self.state, sp, *frame_size);
                    self.ops.push(ResultOp::CallE(*script, *disp_index, params));
                } else {
                    panic!("could not simplify sp");
                }
            },
            intermediate::IntermediateCode::Call(addr, frame_size) => {
                if let Some(sp) = expr_to_value(&self.state, &self.state.sp) {
                    let params = gather_params(&mut self.state, sp, *frame_size);
                    self.ops.push(ResultOp::Call(*addr, params));
                } else {
                    panic!("could not simplify sp");
                }
            },
            intermediate::IntermediateCode::KCall(func, frame_size) => {
                if let Some(sp) = expr_to_value(&self.state, &self.state.sp) {
                    let params = gather_params(&mut self.state, sp, *frame_size);
                    self.ops.push(ResultOp::KCall(*func, params));
                } else {
                    panic!("could not simplify sp");
                }
            },
            op @ _ => { panic!("vm execute todo {:?}", op); }
        }
    }
}

