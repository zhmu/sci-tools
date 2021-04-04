use crate::{disassemble, script};

#[derive(Debug,Clone)]
pub enum Operand {
    Global(usize),
    Local(usize),
    Temp(usize),
    Param(usize),
    Property(usize),
    Imm(usize),
    Acc,
    Prev,
    Sp,
    Tos,
    Rest,
    OpSelf,
    Pc,
    Tmp,
}

#[derive(Debug,Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    ShiftRight,
    ShiftLeft,
    ExclusiveOr,
    BitwiseAnd,
    BitwiseOr,
    Equals,
    NotEquals,
    GreaterThan,
    GreaterOrEqual,
    LessThan,
    LessOrEqual,
    UnsignedGreaterThan,
    UnsignedGreaterOrEqual,
    UnsignedLess,
    UnsignedLessOrEqual,
}

#[derive(Debug,Clone)]
pub enum UnaryOp {
    Negate,
    LogicNot
}

#[derive(Debug,Clone)]
pub enum Expression {
    Operand(Operand),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Address(Box<Expression>),
}

#[derive(Debug,Clone)]
pub enum IntermediateCode {
    Assign(Expression, Expression),
    BranchTrue(usize, usize, Expression), // true-offset, false-offset, expr
    BranchFalse(usize, usize, Expression), // true-offset, false-offset, expr
    BranchAlways(usize),
    Call(usize, usize),
    KCall(usize, usize),
    CallE(usize, usize, usize),
    Return(),
    Send(Expression, usize),
    Class(usize),
}

#[derive(Debug,Clone)]
pub struct Instruction {
    pub offset: usize,
    pub length: usize,
    pub ops: Vec<IntermediateCode>,
}

fn expr_acc() -> Expression { Expression::Operand(Operand::Acc) }
fn expr_prev() -> Expression { Expression::Operand(Operand::Prev) }
fn expr_sp() -> Expression { Expression::Operand(Operand::Sp) }
fn expr_rest() -> Expression { Expression::Operand(Operand::Rest) }
fn expr_imm(n: usize) -> Expression { Expression::Operand(Operand::Imm(n)) }
fn expr_self() -> Expression { Expression::Operand(Operand::OpSelf) }
fn expr_tos() -> Expression { Expression::Operand(Operand::Tos) }
fn expr_tmp() -> Expression { Expression::Operand(Operand::Tmp) }
fn expr_pc() -> Expression { Expression::Operand(Operand::Pc) }

fn new_box_expr(op: Operand) -> Box<Expression> {
    Box::new(Expression::Operand(op))
}

fn new_box_imm(n: usize) -> Box<Expression> { new_box_expr(Operand::Imm(n)) }
fn new_box_acc() -> Box<Expression> { new_box_expr(Operand::Acc) }
fn new_box_sp() -> Box<Expression> { new_box_expr(Operand::Sp) }
fn new_box_rest() -> Box<Expression> { new_box_expr(Operand::Rest) }
fn new_box_pc() -> Box<Expression> { new_box_expr(Operand::Pc) }
fn new_box_tos() -> Box<Expression> { new_box_expr(Operand::Tos) }

fn adjust_sp_before_call(frame_size: usize) -> Vec<IntermediateCode> {
    // sp -= frame_size + 2 + &rest_modifier, &rest_modifier = 0
    let amount = new_box_imm(frame_size + 2);
    vec![
        IntermediateCode::Assign(expr_sp(), Expression::Binary(BinaryOp::Subtract, new_box_sp(), Box::new(Expression::Binary(BinaryOp::Add, amount, new_box_rest())))),
        IntermediateCode::Assign(expr_rest(), expr_imm(0))
    ]
}

enum What {
    Add(usize),
    Subtract(usize)
}

fn apply_to_op(op: Operand, what: What) -> Vec<IntermediateCode> {
    match what {
        What::Add(n) => {
            vec![ IntermediateCode::Assign(Expression::Operand(op.clone()), Expression::Binary(BinaryOp::Add, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
        What::Subtract(n) => {
            vec![ IntermediateCode::Assign(Expression::Operand(op.clone()), Expression::Binary(BinaryOp::Subtract, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
    }
}

fn do_push(expr: Expression) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.push(IntermediateCode::Assign(expr_tos(), expr));
    result.append(&mut apply_to_op(Operand::Sp, What::Add(1)));
    result
}

fn pre_pop() -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut apply_to_op(Operand::Sp, What::Subtract(1)));
    result
}

fn binary_op_pop_acc(op: BinaryOp) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut pre_pop());
    result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(op, new_box_tos(), new_box_acc())));
    result
}

fn binary_op_acc_pop(op: BinaryOp) -> Vec<IntermediateCode> {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.append(&mut pre_pop());
    result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(op, new_box_acc(), new_box_tos())));
    result
}

pub fn convert_instruction(ins: &disassemble::Instruction) -> Instruction {
    let mut result: Vec<IntermediateCode> = Vec::new();
    result.push(IntermediateCode::Assign(expr_pc(), expr_imm(ins.offset + ins.bytes.len())));
    match ins.bytes.first().unwrap() {
        0x00 | 0x01 => { // bnot
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(BinaryOp::ExclusiveOr, new_box_acc(), new_box_imm(0xffff))));
        },
        0x02 | 0x03 => { // add
            result.append(&mut binary_op_pop_acc(BinaryOp::Add));
        },
        0x04 | 0x05 => { // sub
            result.append(&mut binary_op_pop_acc(BinaryOp::Subtract));
        },
        0x06 | 0x07 => { // mul
            result.append(&mut binary_op_pop_acc(BinaryOp::Multiply));
        },
        0x08 | 0x09 => { // div
            result.append(&mut binary_op_pop_acc(BinaryOp::Divide));
        },
        0x0a | 0x0b => { // mod
            result.append(&mut binary_op_pop_acc(BinaryOp::Modulo));
        },
        0x0c | 0x0d => { // shr
            result.append(&mut binary_op_pop_acc(BinaryOp::ShiftRight));
        },
        0x0e | 0x0f => { // shl
            result.append(&mut binary_op_pop_acc(BinaryOp::ShiftLeft));
        },
        0x10 | 0x11 => { // xor
            result.append(&mut binary_op_pop_acc(BinaryOp::ExclusiveOr));
        },
        0x12 | 0x13 => { // and
            result.append(&mut binary_op_acc_pop(BinaryOp::BitwiseAnd));
        },
        0x14 | 0x15 => { // or
            result.append(&mut binary_op_acc_pop(BinaryOp::BitwiseOr));
        },
        0x16 | 0x17 => { // neg
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Unary(UnaryOp::Negate, new_box_acc())));
        },
        0x18 | 0x19 => { // not
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Unary(UnaryOp::LogicNot, new_box_acc())));
        },
        0x1a | 0x1b => { // eq?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::Equals));
        },
        0x1c | 0x1d => { // ne?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::NotEquals));
        },
        0x1e | 0x1f => { // gt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::GreaterThan));
        },
        0x20 | 0x21 => { // ge?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::GreaterOrEqual));
        },
        0x22 | 0x23 => { // lt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::LessThan));
        },
        0x24 | 0x25 => { // le?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::LessOrEqual));
        },
        0x26 | 0x27 => { // ugt?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedGreaterThan));
        },
        0x28 | 0x29 => { // uge?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedGreaterOrEqual));
        },
        0x2a | 0x2b => { // ult?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedLess));
        },
        0x2c | 0x2d => { // ule?
            result.push(IntermediateCode::Assign(expr_prev(), expr_acc()));
            result.append(&mut binary_op_pop_acc(BinaryOp::UnsignedLessOrEqual));
        },
        0x2e | 0x2f => { // bt
            let true_offset = script::relpos0_to_absolute_offset(&ins);
            let next_offset = ins.offset + ins.bytes.len();
            result.push(IntermediateCode::BranchTrue(true_offset, next_offset, expr_acc()));
        },
        0x30 | 0x31 => { // bnt
            let true_offset = script::relpos0_to_absolute_offset(&ins);
            let next_offset = ins.offset + ins.bytes.len();
            result.push(IntermediateCode::BranchFalse(true_offset, next_offset, expr_acc()));
        },
        0x32 | 0x33 => { // jmp
            let next_offset = script::relpos0_to_absolute_offset(&ins);
            result.push(IntermediateCode::BranchAlways(next_offset));
        },
        0x34 | 0x35 => { // ldi
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(Operand::Imm(ins.args[0]))));
        },
        0x36 | 0x37 => { // push
            result.append(&mut do_push(expr_acc()));
        },
        0x38 | 0x39 => { // pushi
            result.append(&mut do_push(expr_imm(ins.args[0])));
        },
        0x3a | 0x3b => { // toss
            result.append(&mut apply_to_op(Operand::Sp, What::Subtract(1)));
        },
        0x3c | 0x3d => { // dup
            result.append(&mut pre_pop());
            result.push(IntermediateCode::Assign(expr_tmp(), Expression::Operand(Operand::Tos)));
            result.append(&mut do_push(expr_tmp()));
            result.append(&mut do_push(expr_tmp()));
        },
        0x3e | 0x3f => { // link
            result.push(IntermediateCode::Assign(expr_sp(), Expression::Binary(BinaryOp::Add, new_box_sp(), new_box_imm(ins.args[0]))));
        },
        0x40 | 0x41 => { // call
            let addr = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::Call(addr, frame_size));
        },
        0x42 | 0x43 => { // kcall
            let addr = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::KCall(addr, frame_size));
        },
        0x44 | 0x45 => { // callb
            let script: usize = 0;
            let disp_index = ins.args[0];
            let frame_size = ins.args[1];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::CallE(script, disp_index, frame_size));
        },
        0x46 | 0x47 => { // calle
            let script = ins.args[0];
            let disp_index = ins.args[1];
            let frame_size = ins.args[2];
            result.append(&mut adjust_sp_before_call(frame_size));

            result.push(IntermediateCode::CallE(script, disp_index, frame_size));
        },
        0x48 | 0x49 => { // ret
            result.push(IntermediateCode::Return());
        },
        0x4a | 0x4b => { // send
            let frame_size = ins.args[0];
            result.push(IntermediateCode::Send(expr_acc(), frame_size));
        },
        0x4c | 0x4d | 0x4e | 0x4f => { // ?
            panic!("invalid opcode (4c/4d/4e/4f)");
        },
        0x50 | 0x51 => { // class
            let func = ins.args[0];
            result.push(IntermediateCode::Class(func));
        },
        0x52 | 0x53 => { // ?
            panic!("invalid opcode (52/53)");
        },
        0x54 | 0x55 => { // self
            let frame_size = ins.args[0];
            result.push(IntermediateCode::Send(expr_self(), frame_size));
        },
        0x56 | 0x57 => { // super
            let class = ins.args[0];
            let frame_size = ins.args[1];
            result.push(IntermediateCode::Send(expr_imm(class), frame_size));
        },
        0x58 | 0x59 => { // &rest
            let _param_index = ins.args[0];
            todo!("&rest");
        },
        0x5a | 0x5b => { // lea
            let vt = ins.args[0];
            let vi = ins.args[1];
            let vtype = (vt >> 1) & 3;

            let op;
            match vtype {
                0 => { op = Operand::Global(vi); },
                1 => { op = Operand::Local(vi); },
                2 => { op = Operand::Temp(vi); },
                3 => { op = Operand::Param(vi); },
                _ => { unreachable!() }
            }

            let expr;
            if (vt & 0x10) != 0 {
                // Add accumulator
                expr = Expression::Binary(BinaryOp::Add, new_box_expr(op), new_box_acc());
            } else {
                expr = Expression::Operand(op);
            }

            // TODO this needs verification to ensure it is correct
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Address(Box::new(expr))));
        },
        0x5c | 0x5d => { // selfid
            result.push(IntermediateCode::Assign(expr_acc(), expr_self()));
        },
        0x5e | 0x5f => { // ?
            panic!("invalid opcode (5e/5f)");
        },
        0x60 | 0x61 => { // pprev
            result.append(&mut do_push(expr_prev()));
        },
        0x62 | 0x63 => { // ptoa
            let op = Operand::Property(ins.args[0]);
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x64 | 0x65 => { // atop
            let op = Operand::Property(ins.args[0]);
            result.push(IntermediateCode::Assign(Expression::Operand(op), Expression::Operand(Operand::Acc)));
        },
        0x66 | 0x67 => { // ptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x68 | 0x69 => { // stop
            let op = Operand::Property(ins.args[0]);
            result.append(&mut pre_pop());
            result.push(IntermediateCode::Assign(Expression::Operand(op), expr_tos()));
        },
        0x6a | 0x6b => { // iptoa
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Add(1)));
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x6c | 0x6d => { // dptoa
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Operand(op)));
        },
        0x6e | 0x6f => { // iptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Add(1)));
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x70 | 0x71 => { // dptos
            let op = Operand::Property(ins.args[0]);
            result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
            result.append(&mut do_push(Expression::Operand(op)));
        },
        0x72 | 0x73 => { // lofsa
            let offset = ins.args[0];
            result.push(IntermediateCode::Assign(expr_acc(), Expression::Binary(BinaryOp::Add, new_box_pc(), new_box_imm(offset))));
        },
        0x74 | 0x75 => { // lofss
            let offset = ins.args[0];
            result.append(&mut do_push(Expression::Binary(BinaryOp::Add, new_box_pc(), new_box_imm(offset))));
        },
        0x76 | 0x77 => { // push0
            result.append(&mut do_push(expr_imm(0)));
        },
        0x78 | 0x79 => { // push1
            result.append(&mut do_push(expr_imm(1)));
        },
        0x7a | 0x7b => { // push2
            result.append(&mut do_push(expr_imm(2)));
        },
        0x7c | 0x7d => { // pushself
            result.append(&mut do_push(expr_self()));
        },
        0x7e | 0x7f => { // ?
            panic!("invalid opcode (7e/7f)");
        },
        opcode @ 0x80..=0xff => {
            let typ = (opcode >> 1) & 3;
            let on_stack = (opcode & 0x8) != 0;
            let acc_modifier = (opcode & 0x10) != 0;
            let mut oper = (opcode >> 5) & 3;

            let op;
            match typ {
                0 => { op = Operand::Global(ins.args[0]); },
                1 => { op = Operand::Local(ins.args[0]); },
                2 => { op = Operand::Temp(ins.args[0]); },
                3 => { op = Operand::Param(ins.args[0]); },
                _ => { unreachable!() }
            }

            let expr;
            if acc_modifier {
                expr = Expression::Binary(
                         BinaryOp::Add,
                         Box::new(Expression::Operand(op)),
                         Box::new(Expression::Operand(Operand::Acc))
                       );
            } else {
                expr = Expression::Operand(op);
            }

            if oper == 2 { // inc+load
                result.push(IntermediateCode::Assign(expr.clone(), Expression::Binary(BinaryOp::Add, Box::new(expr.clone()), new_box_imm(1))));
                oper = 0;
            } else if oper == 3 { // dec+load
                result.push(IntermediateCode::Assign(expr.clone(), Expression::Binary(BinaryOp::Subtract, Box::new(expr.clone()), new_box_imm(1))));
                oper = 0;
            }

            match oper {
                0 => { // load
                    let dest;
                    if on_stack {
                        result.append(&mut do_push(expr));
                    } else {
                        dest = Operand::Acc;
                        result.push(IntermediateCode::Assign(Expression::Operand(dest), expr));
                    }
                },
                1 => { // store
                    let source;
                    if on_stack {
                        result.append(&mut pre_pop());
                        source = Operand::Tos;
                    } else {
                        source = Operand::Acc;
                    }
                    result.push(IntermediateCode::Assign(expr, Expression::Operand(source)));
                },
                _ => { unreachable!() }
            }
        }
    }
    Instruction{ offset: ins.offset, length: ins.bytes.len(), ops: result }
}
