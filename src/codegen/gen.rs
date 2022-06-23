use std::collections::HashMap;

use crate::codegen::Instruction;
use crate::lir::{Binding, Operation, SimpleValue, ValueExpr};

fn make_ident(
    raw_ident: &String,
    function: &String,
    constants: &HashMap<String, ValueExpr>,
) -> String {
    if constants.contains_key(raw_ident) {
        make_const_ident(raw_ident)
    } else {
        format!("var_{function}_{raw_ident}")
    }
}

fn make_const_ident(raw_ident: &String) -> String {
    format!("const_{raw_ident}")
}

fn make_tmp_ident(raw_ident: u64, function: &String) -> String {
    format!("tmp_{function}_{raw_ident}")
}

impl Binding {
    pub fn gen_ident(self, fn_name: &String, constants: &HashMap<String, ValueExpr>) -> String {
        match self {
            Binding::Temporary(id) => make_tmp_ident(id, &fn_name),
            Binding::Variable(var) => make_ident(&var, &fn_name, constants),
        }
    }
}

impl SimpleValue {
    pub fn gen_mlog_literal(
        self,
        fn_name: &String,
        constants: &HashMap<String, ValueExpr>,
    ) -> String {
        match self {
            SimpleValue::Binding(binding) => binding.gen_ident(&fn_name, constants),
            SimpleValue::BoolLiteral(b) => b.to_string(),
            SimpleValue::NumLiteral(n) => n.to_string(),
            SimpleValue::StrLiteral(s) => format!("\"{s}\""),
        }
    }

    pub fn gen_mlog_literal_no_binding(self) -> Option<String> {
        Some(match self {
            SimpleValue::Binding(..) => None?,
            SimpleValue::BoolLiteral(b) => b.to_string(),
            SimpleValue::NumLiteral(n) => n.to_string(),
            SimpleValue::StrLiteral(s) => format!("\"{s}\""),
        })
    }
}

pub fn gen_instructions(
    fn_name: String,
    lir: Vec<Operation>,
    constants: &HashMap<String, ValueExpr>,
    constant_idents: &HashMap<String, String>,
) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];

    for operation in lir {
        match operation {
            Operation::Bind(binding, expr) => match expr {
                ValueExpr::SimpleValue(val) => {
                    instructions.push(Instruction::Set {
                        ident: binding.gen_ident(&fn_name, constants),
                        value: val.gen_mlog_literal(&fn_name, constants),
                    });
                }
                ValueExpr::FunctionCall { name, mut args } => match name.as_str() {
                    "println" => {
                        for arg in args {
                            instructions.push(Instruction::Print {
                                stuff: arg.gen_mlog_literal(&fn_name, constants),
                            })
                        }
                        instructions.push(Instruction::Print {
                            stuff: String::from("\"\\n\""),
                        })
                    }
                    "print" => {
                        for arg in args {
                            instructions.push(Instruction::Print {
                                stuff: arg.gen_mlog_literal(&fn_name, constants),
                            })
                        }
                    }
                    "printflush" => {
                        assert_eq!(
                            args.len(),
                            1,
                            "invalid arguments supplied to printflush: bad arg count"
                        );
                        let ident = args.pop().unwrap();
                        let ident = match ident {
                            SimpleValue::Binding(Binding::Variable(name)) => {
                                if constant_idents.contains_key(&name) {
                                    constant_idents.get(&name).unwrap().clone()
                                } else {
                                    panic!("Invalid arguments supplied to printflush: constant identifier binding `{name}` does not exist")
                                }
                            }
                            _ => panic!("Invalid arguments supplied to printflush: bad value"),
                        };
                        instructions.push(Instruction::PrintFlush { to: ident })
                    }
                    other => {
                        todo!("Cannot call a non-builtin function. (attempting to call `{other}`)")
                    }
                },
                other => panic!("Unsuported ValueExpr: {other:#?}"),
            },
            Operation::Used(expr) => match expr {
                ValueExpr::SimpleValue(SimpleValue::Binding(Binding::Temporary(..))) => {}
                other => unreachable!(
                    "Operation::Used should only contain temporary bindings. found: {other:?}"
                ),
            },
            other => panic!("Unsuported operation: {other:#?}"),
        }
    }

    info!("Instructions for fn `{fn_name}`: {instructions:#?}");
    instructions
}

pub fn gen_constant_bindings(constants: HashMap<String, ValueExpr>) -> Vec<Instruction> {
    let mut instructions: Vec<Instruction> = vec![];
    for (ident, vexpr) in constants {
        if let ValueExpr::SimpleValue(value) = vexpr {
            instructions.push(Instruction::Set {
                ident: make_const_ident(&ident),
                value: value
                    .gen_mlog_literal_no_binding()
                    .expect("Constant definitions cannot reference other variables"),
            })
        } else {
            panic!("Only literal value constants are supported!");
        }
    }
    instructions
}
