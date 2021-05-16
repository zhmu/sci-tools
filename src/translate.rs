use crate::intermediate::*;
use crate::{script, disassemble, sci};
use std::collections::VecDeque;
use std::convert::TryInto;

fn expr_acc() -> Expression { Expression::Operand(Operand::Acc) }
fn expr_prev() -> Expression { Expression::Operand(Operand::Prev) }
fn expr_imm(n: Register) -> Expression { Expression::Operand(Operand::Imm(n)) }
fn expr_self() -> Expression { Expression::Operand(Operand::OpSelf) }
fn expr_tmp() -> Expression { Expression::Operand(Operand::Tmp) }
fn expr_rest(index: FrameSize) -> Expression { Expression::Rest(index) }

fn new_box_expr(op: Operand) -> Box<Expression> {
    Box::new(Expression::Operand(op))
}

fn new_box_imm(n: Register) -> Box<Expression> { new_box_expr(Operand::Imm(n)) }
fn new_box_acc() -> Box<Expression> { new_box_expr(Operand::Acc) }
fn new_box_stack(n: usize) -> Box<Expression> { new_box_expr(Operand::Stack(n)) }

enum What {
    Add(Register),
    Subtract(Register)
}

fn apply_to_op(op: Operand, what: What) -> Vec<IntermediateCode> {
    match what {
        What::Add(n) => {
            vec![ IntermediateCode::Assign(op.clone(), Expression::Binary(BinaryOp::Add, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
        What::Subtract(n) => {
            vec![ IntermediateCode::Assign(op.clone(), Expression::Binary(BinaryOp::Subtract, Box::new(Expression::Operand(op.clone())), new_box_imm(n))) ]
        }
    }
}

fn vtype_to_variable(vtype: u8, arg: Expression ) -> Operand {
    let par = match vtype {
        0 => { Parameter::Global },
        1 => { Parameter::Local },
        2 => { Parameter::Temp },
        3 => { Parameter::Parameter },
        _ => { unreachable!() }
    };
    Operand::Variable(par, Box::new(arg))
}

fn make_property_op(nr: Register) -> Operand {
    Operand::Variable(Parameter::Property, new_box_imm(nr))
}

pub struct Translator
{
    stack_index: usize,
    stack_indexes: VecDeque<usize>,
    rest: FrameSize,
}

impl Translator
{
    pub fn new() -> Self {
        Translator{ stack_index: 0, rest: 0, stack_indexes: VecDeque::new() }
    }

    fn do_push(&mut self, expr: Expression) -> Vec<IntermediateCode> {
        let mut result: Vec<IntermediateCode> = Vec::new();
        result.push(IntermediateCode::Push(self.stack_index, expr.clone()));
        self.stack_indexes.push_back(self.stack_index);
        self.stack_index += 1;
        result
    }

    fn do_pop(&mut self) -> usize {
        if let Some(index) = self.stack_indexes.pop_back() {
            return index;
        }
        println!("do_pop() but stack is empty");
        usize::MAX
    }

    pub fn convert(&mut self, ins: &disassemble::Instruction) -> Instruction {
        let mut result: Vec<IntermediateCode> = Vec::new();
        match ins.bytes.first().unwrap() {
            0x00 | 0x01 => { // bnot
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Binary(BinaryOp::ExclusiveOr, new_box_acc(), new_box_imm(0xffff))));
            },
            0x02 | 0x03 => { // add
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Add));
            },
            0x04 | 0x05 => { // sub
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Subtract));
            },
            0x06 | 0x07 => { // mul
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Multiply));
            },
            0x08 | 0x09 => { // div
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Divide));
            },
            0x0a | 0x0b => { // mod
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Modulo));
            },
            0x0c | 0x0d => { // shr
                result.append(&mut self.binary_op_pop_acc(BinaryOp::ShiftRight));
            },
            0x0e | 0x0f => { // shl
                result.append(&mut self.binary_op_pop_acc(BinaryOp::ShiftLeft));
            },
            0x10 | 0x11 => { // xor
                result.append(&mut self.binary_op_pop_acc(BinaryOp::ExclusiveOr));
            },
            0x12 | 0x13 => { // and
                result.append(&mut self.binary_op_acc_pop(BinaryOp::BitwiseAnd));
            },
            0x14 | 0x15 => { // or
                result.append(&mut self.binary_op_acc_pop(BinaryOp::BitwiseOr));
            },
            0x16 | 0x17 => { // neg
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Unary(UnaryOp::Negate, new_box_acc())));
            },
            0x18 | 0x19 => { // not
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Unary(UnaryOp::LogicNot, new_box_acc())));
            },
            0x1a | 0x1b => { // eq?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::Equals));
            },
            0x1c | 0x1d => { // ne?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::NotEquals));
            },
            0x1e | 0x1f => { // gt?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::GreaterThan));
            },
            0x20 | 0x21 => { // ge?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::GreaterOrEqual));
            },
            0x22 | 0x23 => { // lt?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::LessThan));
            },
            0x24 | 0x25 => { // le?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::LessOrEqual));
            },
            0x26 | 0x27 => { // ugt?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::UnsignedGreaterThan));
            },
            0x28 | 0x29 => { // uge?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::UnsignedGreaterOrEqual));
            },
            0x2a | 0x2b => { // ult?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::UnsignedLess));
            },
            0x2c | 0x2d => { // ule?
                result.push(IntermediateCode::Assign(Operand::Prev, expr_acc()));
                result.append(&mut self.binary_op_pop_acc(BinaryOp::UnsignedLessOrEqual));
            },
            0x2e | 0x2f => { // bt
                let taken_offset = script::relpos0_to_absolute_offset(&ins);
                let next_offset: Offset = (ins.offset + ins.bytes.len()).try_into().unwrap();
                result.push(IntermediateCode::Branch{taken_offset, next_offset, cond: expr_acc()});
            },
            0x30 | 0x31 => { // bnt
                let taken_offset = script::relpos0_to_absolute_offset(&ins);
                let next_offset: Offset = (ins.offset + ins.bytes.len()).try_into().unwrap();
                let expr = Expression::Unary(UnaryOp::LogicNot, new_box_acc());
                result.push(IntermediateCode::Branch{taken_offset, next_offset, cond: expr});
            },
            0x32 | 0x33 => { // jmp
                let next_offset = script::relpos0_to_absolute_offset(&ins);
                result.push(IntermediateCode::BranchAlways(next_offset));
            },
            0x34 | 0x35 => { // ldi
                let imm: Register = ins.args[0];
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(Operand::Imm(imm))));
            },
            0x36 | 0x37 => { // push
                result.append(&mut self.do_push(expr_acc()));
            },
            0x38 | 0x39 => { // pushi
                let imm: Register = ins.args[0];
                result.append(&mut self.do_push(expr_imm(imm)));
            },
            0x3a | 0x3b => { // toss
                let pop = self.do_pop();
                result.push(IntermediateCode::Assign(Operand::Tmp, Expression::Operand(Operand::Stack(pop))));
            },
            0x3c | 0x3d => { // dup
                let pop = self.do_pop();
                result.push(IntermediateCode::Assign(Operand::Tmp, Expression::Operand(Operand::Stack(pop))));
                result.append(&mut self.do_push(expr_tmp()));
                result.append(&mut self.do_push(expr_tmp()));
            },
            0x3e | 0x3f => { // link
                let amount: Register = ins.args[0];
                for _ in 0..amount {
                    result.append(&mut self.do_push(Expression::Undefined));
                }
            },
            0x40 | 0x41 => { // call
                let addr = script::relpos0_to_absolute_offset(&ins);
                let frame_size: FrameSize = ins.args[1];

                let args = self.collect_arguments(frame_size + 2);
                result.push(IntermediateCode::Call(addr, args));
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(Operand::CallResult)));
            },
            0x42 | 0x43 => { // kcall
                let nr = ins.args[0];
                let frame_size: FrameSize = ins.args[1];

                let args = self.collect_arguments(frame_size + 2);
                result.push(IntermediateCode::KCall(nr, args));
                if !sci::does_kcall_return_void(nr) {
                    result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(Operand::CallResult)));
                }
            },
            0x44 | 0x45 => { // callb
                let script: ScriptID = 0;
                let disp_index = ins.args[0];
                let frame_size: FrameSize = ins.args[1];

                let args = self.collect_arguments(frame_size + 2);
                result.push(IntermediateCode::CallE(script, disp_index, args));
            },
            0x46 | 0x47 => { // calle
                let script: ScriptID = ins.args[0];
                let disp_index = ins.args[1];
                let frame_size: FrameSize = ins.args[2];

                let args = self.collect_arguments(frame_size + 2);
                result.push(IntermediateCode::CallE(script, disp_index, args));
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(Operand::CallResult)));
            },
            0x48 | 0x49 => { // ret
                result.push(IntermediateCode::Return(expr_acc()));
            },
            0x4a | 0x4b => { // send
                let frame_size: FrameSize = ins.args[0];
                result.append(&mut self.handle_send(expr_acc(), frame_size));
            },
            0x4c | 0x4d | 0x4e | 0x4f => { // ?
                panic!("invalid opcode (4c/4d/4e/4f)");
            },
            0x50 | 0x51 => { // class
                let func: Register = ins.args[0];
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Class(func)));
            },
            0x52 | 0x53 => { // ?
                panic!("invalid opcode (52/53)");
            },
            0x54 | 0x55 => { // self
                let frame_size: FrameSize = ins.args[0];
                result.append(&mut self.handle_send(expr_self(), frame_size));
            },
            0x56 | 0x57 => { // super
                let class: Register = ins.args[0];
                let frame_size: FrameSize = ins.args[1];
                result.append(&mut self.handle_send(expr_imm(class), frame_size));
            },
            0x58 | 0x59 => { // &rest
                let param_index: Register = ins.args[0];
                self.rest = 2;
                result.append(&mut self.do_push(expr_rest(param_index)));
            },
            0x5a | 0x5b => { // lea
                let vt: Register = ins.args[0];
                let vi: Register = ins.args[1];
                let vtype: u8 = ((vt >> 1) & 3).try_into().unwrap();

                let arg;
                if (vt & 0x10) != 0 {
                    // Add accumulator
                    arg = Expression::Binary(BinaryOp::Add, new_box_imm(vi), new_box_acc());
                } else {
                    arg = Expression::Operand(Operand::Imm(vi));
                }

                let op = vtype_to_variable(vtype, arg);

                // TODO this needs verification to ensure it is correct
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Address(new_box_expr(op))));
            },
            0x5c | 0x5d => { // selfid
                result.push(IntermediateCode::Assign(Operand::Acc, expr_self()));
            },
            0x5e | 0x5f => { // ?
                panic!("invalid opcode (5e/5f)");
            },
            0x60 | 0x61 => { // pprev
                result.append(&mut self.do_push(expr_prev()));
            },
            0x62 | 0x63 => { // ptoa
                let op = make_property_op(ins.args[0]);
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(op)));
            },
            0x64 | 0x65 => { // atop
                let op = make_property_op(ins.args[0]);
                result.push(IntermediateCode::Assign(op, Expression::Operand(Operand::Acc)));
            },
            0x66 | 0x67 => { // ptos
                let op = make_property_op(ins.args[0]);
                result.append(&mut self.do_push(Expression::Operand(op)));
            },
            0x68 | 0x69 => { // stop
                let op = make_property_op(ins.args[0]);
                let pop = self.do_pop();
                result.push(IntermediateCode::Assign(op, Expression::Operand(Operand::Stack(pop))));
            },
            0x6a | 0x6b => { // iptoa
                let op = make_property_op(ins.args[0]);
                result.append(&mut apply_to_op(op.clone(), What::Add(1)));
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(op)));
            },
            0x6c | 0x6d => { // dptoa
                let op = make_property_op(ins.args[0]);
                result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
                result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(op)));
            },
            0x6e | 0x6f => { // iptos
                let op = make_property_op(ins.args[0]);
                result.append(&mut apply_to_op(op.clone(), What::Add(1)));
                result.append(&mut self.do_push(Expression::Operand(op)));
            },
            0x70 | 0x71 => { // dptos
                let op = make_property_op(ins.args[0]);
                result.append(&mut apply_to_op(op.clone(), What::Subtract(1)));
                result.append(&mut self.do_push(Expression::Operand(op)));
            },
            0x72 | 0x73 => { // lofsa
                let offset: Register = ins.args[0];
                let addr: usize = ins.offset + ins.bytes.len() + offset as usize;
                result.push(IntermediateCode::Assign(Operand::Acc, expr_imm((addr & 0xffff) as Register)));
            },
            0x74 | 0x75 => { // lofss
                let offset: Register = ins.args[0];
                let addr: usize = ins.offset + ins.bytes.len() + offset as usize;
                result.append(&mut self.do_push(expr_imm((addr & 0xffff) as Register)));
            },
            0x76 | 0x77 => { // push0
                result.append(&mut self.do_push(expr_imm(0)));
            },
            0x78 | 0x79 => { // push1
                result.append(&mut self.do_push(expr_imm(1)));
            },
            0x7a | 0x7b => { // push2
                result.append(&mut self.do_push(expr_imm(2)));
            },
            0x7c | 0x7d => { // pushself
                result.append(&mut self.do_push(expr_self()));
            },
            0x7e | 0x7f => { // ?
                panic!("invalid opcode (7e/7f)");
            },
            opcode @ 0x80..=0xff => {
                let typ = (opcode >> 1) & 3;
                let on_stack = (opcode & 0x8) != 0;
                let acc_modifier = (opcode & 0x10) != 0;
                let mut oper = (opcode >> 5) & 3;

                let arg;
                let index: Register = ins.args[0];
                if acc_modifier {
                    arg = Expression::Binary(BinaryOp::Add, new_box_imm(index), new_box_acc());
                } else {
                    arg = Expression::Operand(Operand::Imm(index));
                }
                let op = vtype_to_variable(typ, arg);

                if oper == 2 { // inc+load
                    result.push(IntermediateCode::Assign(op.clone(), Expression::Binary(BinaryOp::Add, Box::new(Expression::Operand(op.clone())), new_box_imm(1))));
                    oper = 0;
                } else if oper == 3 { // dec+load
                    result.push(IntermediateCode::Assign(op.clone(), Expression::Binary(BinaryOp::Subtract, Box::new(Expression::Operand(op.clone())), new_box_imm(1))));
                    oper = 0;
                }
                match oper {
                    0 => { // load
                        if on_stack {
                            result.append(&mut self.do_push(Expression::Operand(op.clone())));
                        } else {
                            result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(op.clone())));
                        }
                    },
                    1 => { // store
                        // sa[gltp]i will pop for the actual value to store from the stack and store it in the
                        // accumulator
                        if !on_stack && acc_modifier {
                            let pop = self.do_pop();
                            result.push(IntermediateCode::Assign(op.clone(), Expression::Operand(Operand::Stack(pop))));
                            result.push(IntermediateCode::Assign(Operand::Acc, Expression::Operand(Operand::Stack(pop))));
                        } else {
                            let source;
                            if on_stack {
                                let pop = self.do_pop();
                                source = Operand::Stack(pop);
                            } else {
                                source = Operand::Acc;
                            }
                            result.push(IntermediateCode::Assign(op, Expression::Operand(source)));
                        }
                    },
                    _ => { unreachable!() }
                }
            }
        }
        Instruction{ offset: ins.offset as Offset, length: ins.bytes.len(), ops: result }
    }

    fn binary_op_pop_acc(&mut self, op: BinaryOp) -> Vec<IntermediateCode> {
        let mut result: Vec<IntermediateCode> = Vec::new();
        let pop = self.do_pop();
        result.push(IntermediateCode::Assign(Operand::Acc, Expression::Binary(op, new_box_stack(pop), new_box_acc())));
        result
    }

    fn binary_op_acc_pop(&mut self, op: BinaryOp) -> Vec<IntermediateCode> {
        let mut result: Vec<IntermediateCode> = Vec::new();
        let pop = self.do_pop();
        result.push(IntermediateCode::Assign(Operand::Acc, Expression::Binary(op, new_box_acc(), new_box_stack(pop))));
        result
    }

    fn collect_arguments(&mut self, frame_size: FrameSize) -> Vec<Expression> {
        let mut args: Vec<Expression> = Vec::new();
        for _ in 0..((frame_size + self.rest) / 2) {
            let pop = self.do_pop();
            args.push(Expression::Operand(Operand::Stack(pop)));
        }
        self.rest = 0;
        args.reverse();
        args
    }

    fn handle_send(&mut self, target: Expression, frame_size: FrameSize) -> Vec<IntermediateCode> {
        let mut result: Vec<IntermediateCode> = Vec::new();
        let args = self.collect_arguments(frame_size);
        result.push(IntermediateCode::Send(target, args));
        result
    }
}
