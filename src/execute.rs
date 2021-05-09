use crate::{intermediate, class_defs};

use std::fmt;
use std::collections::HashSet;

const STACK_SIZE: intermediate::Value = 2048;

#[derive(Debug,Clone)]
pub struct VMState {
    pub acc: intermediate::Expression,
    rest: intermediate::Expression,
    pub sp: intermediate::Expression,
    prev: intermediate::Expression,
    tmp: intermediate::Expression,
    pub stack: Vec<intermediate::Expression>
}

impl VMState {
    pub fn new() -> Self {
        let undef = intermediate::Expression::Undefined;
        let zero = intermediate::Expression::Operand(intermediate::Operand::Imm(0));
        let mut stack: Vec<intermediate::Expression> = Vec::new();
        for _ in 0..STACK_SIZE {
            stack.push(undef.clone());
        }
        VMState{ acc: undef.clone(), rest: zero.clone(), prev: undef.clone(), sp: zero.clone(), tmp: undef.clone(), stack }
    }

    fn get_sp(&self) -> intermediate::Register {
        if let Some(sp) = expr_to_value(&self, &self.sp) {
            return sp;
        } else {
            panic!("cannot resolve sp value");
        }
    }

    fn get_sp_index(&self) -> usize {
        let sp = self.get_sp() as usize;
        assert_eq!(sp % 2, 0);
        sp / 2
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

fn is_expr_undefined(expr: &intermediate::Expression) -> bool {
    match expr {
        intermediate::Expression::Undefined => { true },
        _ => { false }
    }
}

impl fmt::Display for VMState {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut stack: String = String::new();
        let stack_size: usize;
        if let Some(sp) = get_imm_value(&self.sp) {
            stack_size = sp as usize;
        } else {
            stack_size = STACK_SIZE as usize;
        }
        let mut n: usize = 0;
        while n <= stack_size / 2 && is_expr_undefined(&self.stack[n]) {
            n += 1;
        }
        if n > 0 {
            stack += &format!(" [Undef*{}]", n);
        }

        while n <= stack_size / 2 {
            stack += &format!(" {}", format_expr(&self.stack[n])).to_string();
            n += 1;
        }
        write!(f, "acc: {} rest: {} prev: {}, sp: {} stack{}", format_expr(&self.acc), format_expr(&self.rest), format_expr(&self.prev), format_expr(&self.sp), stack)
    }
}

#[derive(Debug)]
pub enum ResultOp {
    AssignProperty(intermediate::Expression, intermediate::Expression),
    AssignGlobal(intermediate::Expression, intermediate::Expression),
    AssignTemp(intermediate::Expression, intermediate::Expression),
    AssignLocal(intermediate::Expression, intermediate::Expression),
    AssignParam(intermediate::Expression, intermediate::Expression),
    AssignHelperVar(usize, intermediate::Expression),
    CallE(intermediate::Value, intermediate::Value, Vec<intermediate::Expression>),
    Call(intermediate::Value, Vec<intermediate::Expression>),
    KCall(intermediate::Value, Vec<intermediate::Expression>),
    Send(intermediate::Expression, intermediate::Expression, Vec<intermediate::Expression>),
    Incomplete(String),
    Return(),
}

pub enum BranchIf {
    Never,
    Condition(intermediate::Expression),
}

pub struct VM<'a> {
    pub ops: Vec<ResultOp>,
    pub branch: BranchIf,
    pub state: VMState,
    class_definitions: &'a class_defs::ClassDefinitions
}

#[derive(PartialEq,Eq,Hash)]
enum StateEnum {
    Sp,
    Acc,
    Tmp,
    Rest,
    Prev
}

fn apply_binary_op(op: &intermediate::BinaryOp, a: Option<intermediate::Value>, b: Option<intermediate::Value>) -> Option<intermediate::Value> {
    if a.is_some() && b.is_some() {
        let a = a.unwrap();
        let b = b.unwrap();
        return Some(match op {
            intermediate::BinaryOp::Add => { a.overflowing_add(b).0 },
            intermediate::BinaryOp::Subtract => { a.overflowing_sub(b).0 },
            intermediate::BinaryOp::Multiply => { a.overflowing_mul(b).0 },
            intermediate::BinaryOp::Divide => { a.overflowing_div(b).0 },
            intermediate::BinaryOp::Modulo => { a.overflowing_rem(b).0 },
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
            intermediate::UnaryOp::Negate => { (-(a as i16)) as u16 },
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
        intermediate::Expression::Operand(intermediate::Operand::Property(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::HelperVariable(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::CallResult) => { None },
        intermediate::Expression::Operand(intermediate::Operand::OpSelf) => { None },
        intermediate::Expression::Operand(intermediate::Operand::SelectorValue(_, _)) => { None },
        intermediate::Expression::Class(_) => { None },
        intermediate::Expression::Address(_) => { None },
        intermediate::Expression::Undefined => { None },
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
            let index = state.get_sp_index();
            let tos = state.stack[index].clone();
            //println!("simplify_expr2: reading tos index {} -> {:?}", index, tos);
            return simplify_expr2(state, state_seen, tos);
        },
        intermediate::Expression::Operand(intermediate::Operand::Prev) => {
            if !state_seen.contains(&StateEnum::Prev) {
                state_seen.insert(StateEnum::Prev);
                return simplify_expr2(state, state_seen, state.prev.clone());
            }
            return state.prev.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Imm(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Param(expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Param(Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::Global(expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Global(Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::Local(expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Local(Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::Temp(expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Temp(Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::Property(expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Property(Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::HelperVariable(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::OpSelf) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::CallResult) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::SelectorValue(expr, nr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::SelectorValue(Box::new(expr), nr));
        },
        intermediate::Expression::Binary(op, a, b) => {
            let a = simplify_expr2(state, state_seen, *a);
            let b = simplify_expr2(state, state_seen, *b);
            return intermediate::Expression::Binary(op, Box::new(a), Box::new(b));
        },
        intermediate::Expression::Unary(op, a) => {
            let a = simplify_expr2(state, state_seen, *a);
            return intermediate::Expression::Unary(op, Box::new(a));
        },
        intermediate::Expression::Address(a) => {
            let a = simplify_expr2(state, state_seen, *a);
            return intermediate::Expression::Address(Box::new(a));
        },
        intermediate::Expression::Class(_) => { return expr.clone(); },
        intermediate::Expression::Undefined => { return expr.clone(); },
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

impl<'a> VM<'a> {
    pub fn new(state: &VMState, class_definitions: &'a class_defs::ClassDefinitions) -> Self {
        VM{ ops: Vec::new(), branch: BranchIf::Never, state: state.clone(), class_definitions }
    }

    pub fn get_stack_values(&self, frame_size: intermediate::Register) -> Vec<intermediate::Expression> {
        let mut result: Vec<intermediate::Expression> = Vec::new();
        let sp = self.state.get_sp() / 2;
        for n in 0..(frame_size / 2) {
            result.push(self.state.stack[(sp + n) as usize].clone());
        }
        result
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
                        let index = self.state.get_sp_index();
                        self.state.stack[index] = simplify_expr(&mut self.state, expr);
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
                    intermediate::Operand::Local(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        self.ops.push(ResultOp::AssignLocal(n, result));
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
                    intermediate::Operand::HelperVariable(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        self.ops.push(ResultOp::AssignHelperVar(*n, result));
                    },
                    intermediate::Operand::Param(n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        self.ops.push(ResultOp::AssignParam(n, result));
                    },
                    _ => { panic!("todo: Assign({:?}, {:?}", dest, expr); }
                }
            },
            intermediate::IntermediateCode::Branch{ taken_offset: _, next_offset: _, cond} => {
                let cond = simplify_expr(&mut self.state, cond);
                if let BranchIf::Never = self.branch {
                    self.branch = BranchIf::Condition(cond.clone());
                } else {
                    panic!();
                }
            },
            intermediate::IntermediateCode::BranchAlways(_) => {
                //println!("BranchAlways(): don't think we need to do anything here?");
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
            intermediate::IntermediateCode::Send(expr, frame_size) => {
                let values = self.get_stack_values(*frame_size);
                let expr = simplify_expr(&mut self.state, &expr);

                let frame_size = (*frame_size as usize) / 2;
                let mut n: usize = 0;
                while n < frame_size {
                    let selector = &values[n];
                    n += 1;
                    if let Some(num_values) = expr_to_value(&self.state, &values[n]) {
                        let num_values = num_values as usize;
                        n += 1;
                        let mut args: Vec<intermediate::Expression> = Vec::new();
                        for m in 0..num_values {
                            let expr = simplify_expr(&mut self.state, &values[n + m]);
                            args.push(expr);
                        }

                        if let Some(selector_value) = expr_to_value(&self.state, &selector) {
                            if num_values == 0 && self.class_definitions.is_certainly_propery(selector_value) {
                                self.state.acc = intermediate::Expression::Operand(
                                    intermediate::Operand::SelectorValue(Box::new(expr.clone()), selector_value));
                            } else {
                                if self.class_definitions.is_certainly_func(selector_value) {
                                    self.state.acc = intermediate::Expression::Operand(intermediate::Operand::CallResult);
                                }
                                self.ops.push(ResultOp::Send(expr.clone(), selector.clone(), args));
                            }
                            n += num_values;
                        } else {
                            let msg = format!("send: unable to convert selector value {:?} to number", selector);
                            self.ops.push(ResultOp::Incomplete(msg));
                            self.ops.push(ResultOp::Send(expr.clone(), selector.clone(), args));
                            n += num_values;
                        }
                    } else {
                        let msg = format!("could not convert num_values {:?} to number", values[n]);
                        self.ops.push(ResultOp::Incomplete(msg));
                        break;
                    }
                }
            },
            intermediate::IntermediateCode::Rest(num_args) => {
                let msg = format!("&rest {} called", num_args);
                self.ops.push(ResultOp::Incomplete(msg));
            }
        }
    }
}

