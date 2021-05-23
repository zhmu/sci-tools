use crate::intermediate;

use std::fmt;
use std::collections::HashSet;
use std::collections::HashMap;

#[derive(Debug,Clone)]
pub struct VMState {
    pub acc: intermediate::Expression,
    prev: intermediate::Expression,
    tmp: intermediate::Expression,
    stk_values: HashMap<usize, intermediate::Expression>,
}

impl VMState {
    pub fn new() -> Self {
        let undef = intermediate::Expression::Undefined;
        VMState{ acc: undef.clone(), prev: undef.clone(), tmp: undef.clone(), stk_values: HashMap::new() }
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
        for (index, value) in &self.stk_values {
            stack += &format!(" s{}={}", index, format_expr(&value)).to_string();
        }
        write!(f, "acc: {} prev: {}, stack{}", format_expr(&self.acc), format_expr(&self.prev), stack)
    }
}

#[derive(Debug)]
pub enum ResultOp {
    Push(usize, intermediate::Expression),
    AssignProperty(intermediate::Expression, intermediate::Expression),
    AssignGlobal(intermediate::Expression, intermediate::Expression),
    AssignTemp(intermediate::Expression, intermediate::Expression),
    AssignLocal(intermediate::Expression, intermediate::Expression),
    AssignParam(intermediate::Expression, intermediate::Expression),
    AssignHelperVar(usize, intermediate::Expression),
    WriteSelectorValue(intermediate::Expression, intermediate::Value, intermediate::Expression),
    Send(intermediate::Expression, intermediate::Expression, Vec<intermediate::Expression>),
    Incomplete(String),
    Return(intermediate::Expression),
}

pub enum BranchIf {
    Never,
    Condition(intermediate::Expression),
}

pub struct VM {
    pub ops: Vec<ResultOp>,
    pub branch: BranchIf,
    pub state: VMState,
}

#[derive(PartialEq,Eq,Hash)]
enum StateEnum {
    Acc,
    Tmp,
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
        intermediate::Expression::Operand(intermediate::Operand::Stack(n)) => {
            if let Some(expr) = state.stk_values.get(n) {
                expr_to_value(state, expr)
            } else {
                println!("expr_to_value(): index {} out of range", n);
                None
            }
        },
        intermediate::Expression::Operand(intermediate::Operand::Variable(..)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::HelperVariable(_)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::CallResult) => { None },
        intermediate::Expression::Operand(intermediate::Operand::OpSelf) => { None },
        intermediate::Expression::Operand(intermediate::Operand::SelectorValue(_, _)) => { None },
        intermediate::Expression::Operand(intermediate::Operand::InvokeSelector(..)) => { None },
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
        intermediate::Expression::Call(..) => { None },
        intermediate::Expression::CallE(..) => { None },
        intermediate::Expression::KCall(..) => { None },
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
        intermediate::Expression::Operand(intermediate::Operand::Stack(..)) => {
            return expr.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Tmp) => {
            if !state_seen.contains(&StateEnum::Tmp) {
                state_seen.insert(StateEnum::Tmp);
                return simplify_expr2(state, state_seen, state.tmp.clone());
            }
            return state.tmp.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Prev) => {
            if !state_seen.contains(&StateEnum::Prev) {
                state_seen.insert(StateEnum::Prev);
                return simplify_expr2(state, state_seen, state.prev.clone());
            }
            return state.prev.clone();
        },
        intermediate::Expression::Operand(intermediate::Operand::Imm(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::Variable(par, expr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::Variable(par, Box::new(expr)));
        },
        intermediate::Expression::Operand(intermediate::Operand::HelperVariable(_)) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::OpSelf) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::CallResult) => { return expr.clone(); },
        intermediate::Expression::Operand(intermediate::Operand::SelectorValue(expr, nr)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            return intermediate::Expression::Operand(intermediate::Operand::SelectorValue(Box::new(expr), nr));
        },
        intermediate::Expression::Operand(intermediate::Operand::InvokeSelector(expr, nr, args)) => {
            let expr = simplify_expr2(state, state_seen, *expr);
            let mut new_args: Vec<intermediate::Expression> = Vec::new();
            for arg in args {
                let arg = simplify_expr2(state, state_seen, arg);
                new_args.push(arg);
            }
            return intermediate::Expression::Operand(intermediate::Operand::InvokeSelector(Box::new(expr), nr, new_args));
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
        intermediate::Expression::Rest(..) => { return expr.clone(); },
        intermediate::Expression::Call(..) => { return expr.clone(); },
        intermediate::Expression::CallE(..) => { return expr.clone(); },
        intermediate::Expression::KCall(..) => { return expr.clone(); },
    }
}

pub fn simplify_expr(state: &mut VMState, expr: &intermediate::Expression) -> intermediate::Expression {
    simplify_expr2(state, &mut HashSet::new(), expr.clone())
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
                    intermediate::Operand::Tmp => {
                        self.state.tmp = simplify_expr(&mut self.state, expr);
                        if let Some(v) = expr_to_value(&self.state, &self.state.tmp) {
                            self.state.tmp = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        }
                    },
                    intermediate::Operand::Variable(par, n) => {
                        let result;
                        let expr = simplify_expr(&mut self.state, &expr);
                        if let Some(v) = expr_to_value(&self.state, &expr) {
                            result = intermediate::Expression::Operand(intermediate::Operand::Imm(v));
                        } else {
                            result = expr.clone();
                        }
                        let n = simplify_expr(&mut self.state, n);
                        let op = match par {
                            intermediate::Parameter::Global => { ResultOp::AssignGlobal(n, result) },
                            intermediate::Parameter::Local => { ResultOp::AssignLocal(n, result) },
                            intermediate::Parameter::Temp => { ResultOp::AssignTemp(n, result) },
                            intermediate::Parameter::Parameter => { ResultOp::AssignParam(n, result) },
                            intermediate::Parameter::Property => { ResultOp::AssignProperty(n, result) }
                        };
                        self.ops.push(op);
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
                    _ => { panic!("todo: Assign({:?}, {:?}", dest, expr); }
                }
            },
            intermediate::IntermediateCode::Push(index, expr) => {
                let expr = simplify_expr(&mut self.state, expr);
                self.ops.push(ResultOp::Push(*index, expr.clone()));
                self.state.stk_values.insert(*index, expr);
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
            intermediate::IntermediateCode::Return(expr) => {
                let expr = simplify_expr(&mut self.state, expr);
                self.ops.push(ResultOp::Return(expr));
            },
            intermediate::IntermediateCode::Send(expr, values) => {
                // We should only get here once flow::analyse_send() could not
                // figure out which type of send it was, so no need to analyse
                // more here
                let selector = simplify_expr(&mut self.state, &values[0]);
                let mut params: Vec<intermediate::Expression> = Vec::with_capacity(values.len() - 1);
                for v in 1..values.len() {
                    let expr = simplify_expr(&mut self.state, &values[v]);
                    params.push(expr);
                }
                let expr = simplify_expr(&mut self.state, &expr);
                self.ops.push(ResultOp::Send(expr.clone(), selector, params));
            },
            intermediate::IntermediateCode::WriteSelector(expr, selector, value) => {
                self.ops.push(ResultOp::WriteSelectorValue(expr.clone(), *selector, value.clone()));
            },
        }
    }
}

