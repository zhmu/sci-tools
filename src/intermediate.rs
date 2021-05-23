pub type Value = u16;
pub type Register = Value;
pub type Offset = Value;
pub type ScriptID = Value;
pub type FrameSize = Value;

#[derive(Debug,Clone)]
pub enum Parameter {
    Global,
    Local,
    Temp,
    Parameter,
    Property
}


#[derive(Debug,Clone)]
pub enum Operand {
    Variable(Parameter, Box<Expression>),
    Imm(Register),
    HelperVariable(usize),
    SelectorValue(Box<Expression>, Register),
    InvokeSelector(Box<Expression>, Register, Vec<Expression>),
    Acc,
    Prev,
    OpSelf,
    Tmp,
    CallResult,
    Stack(usize),
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
    Undefined,
    Rest(FrameSize),
    Operand(Operand),
    Binary(BinaryOp, Box<Expression>, Box<Expression>),
    Unary(UnaryOp, Box<Expression>),
    Address(Box<Expression>),
    Class(Register),
    Call(Offset, Vec<Expression>),
    KCall(Register, Vec<Expression>),
    CallE(ScriptID, Register, Vec<Expression>),
}

#[derive(Debug,Clone)]
pub enum IntermediateCode {
    Assign(Operand, Expression),
    Push(usize, Expression),
    Branch{ taken_offset: Offset, next_offset: Offset, cond: Expression },
    BranchAlways(Offset),
    Return(Expression),
    Send(Expression, Vec<Expression>),
    WriteSelector(Expression, Register, Expression),
}

#[derive(Debug,Clone)]
pub struct Instruction {
    pub offset: Offset,
    pub length: usize,
    pub ops: Vec<IntermediateCode>,
}
