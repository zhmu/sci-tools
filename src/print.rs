use crate::{label, vocab, intermediate, execute, sci};

pub struct Formatter<'a> {
    labels: &'a label::LabelMap,
    sel_vocab: &'a vocab::Vocab997
}

impl<'a> Formatter<'a> {
    pub fn new(labels: &'a label::LabelMap, sel_vocab: &'a vocab::Vocab997) -> Self {
        Formatter{ labels, sel_vocab }
    }

    fn format_operand(&self, op: &intermediate::Operand) -> String {
        match op {
            intermediate::Operand::Global(expr) => {
                let expr = self.format_expression(expr);
                format!("global({})", expr)
            },
            intermediate::Operand::Local(expr) => {
                let expr = self.format_expression(expr);
                format!("local({})", expr)
            },
            intermediate::Operand::Temp(expr) => {
                let expr = self.format_expression(expr);
                format!("temp({})", expr)
            },
            intermediate::Operand::Param(expr) => {
                let expr = self.format_expression(expr);
                format!("param({})", expr)
            },
            intermediate::Operand::Property(expr) => {
                let expr = self.format_expression(expr);
                format!("property({})", expr)
            },
            intermediate::Operand::Imm(val) => {
                format!("{}", val)
            },
            intermediate::Operand::HelperVariable(num) => {
                format!("v{}", num)
            },
            intermediate::Operand::Acc => { "acc".to_string() },
            intermediate::Operand::Prev => { "prev".to_string() },
            intermediate::Operand::Sp => { "sp".to_string() },
            intermediate::Operand::Tos => { "tos".to_string() },
            intermediate::Operand::Rest => { "rest".to_string() },
            intermediate::Operand::OpSelf => { "self".to_string() },
            intermediate::Operand::Tmp => { "tmp".to_string() }
            intermediate::Operand::CallResult => { "callResult".to_string() }
        }
    }

    pub fn format_expression(&self, expr: &intermediate::Expression) -> String {
        match expr {
            intermediate::Expression::Undefined => { "undefined".to_string() },
            intermediate::Expression::Operand(op) => { self.format_operand(op) },
            intermediate::Expression::Binary(op, expr1, expr2) => {
                let expr1 = self.format_expression(expr1);
                let expr2 = self.format_expression(expr2);
                let op = match op {
                    intermediate::BinaryOp::Add => { "+" },
                    intermediate::BinaryOp::Subtract => { "-" },
                    intermediate::BinaryOp::Multiply => { "*" },
                    intermediate::BinaryOp::Divide => { "/" },
                    intermediate::BinaryOp::Modulo => { "%" },
                    intermediate::BinaryOp::ShiftRight => { ">>" },
                    intermediate::BinaryOp::ShiftLeft => { "<<" },
                    intermediate::BinaryOp::ExclusiveOr => { "^" },
                    intermediate::BinaryOp::BitwiseAnd => { "&" },
                    intermediate::BinaryOp::BitwiseOr => { "|"},
                    intermediate::BinaryOp::Equals => { "==" },
                    intermediate::BinaryOp::NotEquals => { "!=" },
                    intermediate::BinaryOp::GreaterThan => { ">" },
                    intermediate::BinaryOp::GreaterOrEqual => { ">=" },
                    intermediate::BinaryOp::LessThan => { "<" },
                    intermediate::BinaryOp::LessOrEqual => { "<=" },
                    intermediate::BinaryOp::UnsignedGreaterThan => { "u>"},
                    intermediate::BinaryOp::UnsignedGreaterOrEqual => { "u>=" },
                    intermediate::BinaryOp::UnsignedLess => { "u<" },
                    intermediate::BinaryOp::UnsignedLessOrEqual => { "u<=" },
                };
                format!("{} {} {}", expr1, op, expr2)
            },
            intermediate::Expression::Unary(op, expr) => {
                let expr = self.format_expression(expr);
                let op = match op {
                    intermediate::UnaryOp::Negate => { "-" },
                    intermediate::UnaryOp::LogicNot => { "!" },
                };
                format!("{} {}", op, expr)
            },
            intermediate::Expression::Address(expr) => {
                let expr = self.format_expression(expr);
                format!("&({})", expr)
            },
            intermediate::Expression::Class(val) => {
                format!("class({})", val)
            },
        }
    }

    fn format_expression_vec(&self, v: &Vec<intermediate::Expression>) -> String {
        let mut result: String = String::new();
        for p in v {
            if !result.is_empty() {
                result += ", ";
            }
            result += &self.format_expression(p);
        }
        result
    }

    fn format_selector(&self, selector: &intermediate::Expression) -> String {
        if let Some(value) = get_expression_value(selector) {
            return self.sel_vocab.get_selector_name(value.into()).to_string();
        }
        self.format_expression(selector)
    }

    fn format_kcall(&self, num: intermediate::Value, params: &Vec<intermediate::Expression>) -> String {
        let kcall = sci::lookup_kcall(num);
        match kcall {
            Some(kcall) => {
                let mut result: String;
                result = format!("K{}(", kcall.name); // prefix K here

                // First parameter is always the number of arguments
                let param = self.format_expression(&params[0]);
                result += format!("num_args={}", param).as_str();

                let mut n: usize = 1;
                while n <= kcall.arg.len() && n < params.len() {
                    let param = self.format_expression(&params[n]);
                    result += format!(", {}={}", kcall.arg[n - 1].name, param).as_str();
                    n += 1;
                }
                while n < params.len() {
                    let param = self.format_expression(&params[n]);
                    result += format!(", arg{}={}", n, param).as_str();
                    n += 1;
                }
                while n <= kcall.arg.len() {
                    result += format!(", {}=?", kcall.arg[n - 1].name).as_str();
                    n += 1;
                }

                result += ")";
                result
            },
            None => {
                let params = self.format_expression_vec(params);
                format!("callK({}, {})", num, params)
            }
        }
    }

    pub fn format_rop(&self, op: &execute::ResultOp) -> String {
        match op {
            execute::ResultOp::AssignProperty(dest, expr) => {
                let dest = self.format_expression(dest);
                let expr = self.format_expression(expr);
                format!("property({}) = {}", dest, expr)
            },
            execute::ResultOp::AssignGlobal(dest, expr) => {
                let dest = self.format_expression(dest);
                let expr = self.format_expression(expr);
                format!("global({}) = {}", dest, expr)
            },
            execute::ResultOp::AssignTemp(dest, expr) => {
                let dest = self.format_expression(dest);
                let expr = self.format_expression(expr);
                format!("temp({}) = {}", dest, expr)
            },
            execute::ResultOp::AssignLocal(dest, expr) => {
                let dest = self.format_expression(dest);
                let expr = self.format_expression(expr);
                format!("local({}) = {}", dest, expr)
            },
            execute::ResultOp::AssignParam(dest, expr) => {
                let dest = self.format_expression(dest);
                let expr = self.format_expression(expr);
                format!("param({}) = {}", dest, expr)
            },
            execute::ResultOp::AssignHelperVar(n, expr) => {
                let expr = self.format_expression(expr);
                format!("v{} = {}", n, expr)
            },
            execute::ResultOp::CallE(script_num, disp_index, params) => {
                let params = self.format_expression_vec(params);
                format!("callE({}, {}, {})", script_num, disp_index, params)
            },
            execute::ResultOp::Call(offset, params) => {
                let params = self.format_expression_vec(params);
                let offset = self.get_label(*offset);
                format!("{}({})", offset, params)
            },
            execute::ResultOp::KCall(num, params) => {
                self.format_kcall(*num, params)
            },
            execute::ResultOp::Send(dest, selector, params) => {
                let dest = self.format_expression(dest);
                let selector = self.format_selector(selector);
                let params = self.format_expression_vec(params);
                format!("send({}, {}, {})", dest, selector, params)
            },
            execute::ResultOp::Incomplete(msg) => {
                format!("incomplete!({})", msg)
            },
            execute::ResultOp::Return() => { "return".to_string() },
        }
    }

    pub fn get_label(&self, offset: u16) -> String {
        if let Some(label) = self.labels.get(&offset) {
            label.to_string()
        } else {
            format!("local_{:x}", offset)
        }
    }
}

fn get_expression_value(expr: &intermediate::Expression) -> Option<u16> {
    if let intermediate::Expression::Operand(op) = expr {
        if let intermediate::Operand::Imm(val) = op {
            return Some(*val);
        }
    }
    None
}