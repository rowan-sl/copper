use crate::codegen::Instruction;
use crate::lir::{Binding, Operation, SimpleValue, ValueExpr};

fn make_ident(raw_ident: &String, function: &String) -> String {
    format!("var_{function}_{raw_ident}")
}

fn make_tmp_ident(raw_ident: u64, function: &String) -> String {
    format!("tmp_{function}_{raw_ident}")
}

impl Binding {
    pub fn gen_ident(self, fn_name: &String) -> String {
        match self {
            Binding::Temporary(id) => make_tmp_ident(id, &fn_name),
            Binding::Variable(var) => make_ident(&var, &fn_name),
        }
    }
}

impl SimpleValue {
    pub fn gen_mlog_literal(self, fn_name: &String) -> String {
        match self {
            SimpleValue::Binding(binding) => binding.gen_ident(&fn_name),
            SimpleValue::BoolLiteral(b) => b.to_string(),
            SimpleValue::NumLiteral(n) => n.to_string(),
            SimpleValue::StrLiteral(s) => format!("\"{s}\""),
        }
    }
}

pub fn gen_instructions(fn_name: String, lir: Vec<Operation>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];

    for operation in lir {
        match operation {
            Operation::Bind(binding, expr) => match expr {
                ValueExpr::SimpleValue(val) => {
                    instructions.push(Instruction::Set {
                        ident: binding.gen_ident(&fn_name),
                        value: val.gen_mlog_literal(&fn_name),
                    });
                }
                other => error!("Unsuported ValueExpr: {other:#?}"),
            },
            Operation::Used(..) => {}
            other => error!("Unsuported operation: {other:#?}"),
        }
    }

    info!("Instructions for fn `{fn_name}`: {instructions:#?}");
    instructions
}
