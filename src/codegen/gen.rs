use std::collections::HashMap;

use uuid::Uuid;

use crate::analyzer::visitors::raw_functions::RawFn;
use crate::codegen::{Instruction, MlogAddr};
use crate::lir::{Binding, Operation, SimpleValue, ValueExpr, If};
use crate::parse2::types::functions::FunctionArguments;

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

#[derive(Debug, Clone, PartialEq)]
pub struct Metadata {
    tag: Option<Uuid>,
}

impl Default for Metadata {
    fn default() -> Self {
        Self { tag: None }
    }
}

pub fn gen_instructions(
    fn_name: String,
    lir: Vec<Operation>,
    constants: &HashMap<String, ValueExpr>,
    constant_idents: &HashMap<String, String>,
) -> Vec<(Metadata, Instruction)> {
    let mut instructions: Vec<(Metadata, Instruction)> = vec![];

    for operation in lir {
        match operation {
            Operation::Bind(binding, expr) | Operation::Update(binding, expr) => match expr {
                ValueExpr::SimpleValue(val) => {
                    instructions.push((
                        Metadata::default(),
                        Instruction::Set {
                            ident: binding.gen_ident(&fn_name, constants),
                            value: val.gen_mlog_literal(&fn_name, constants),
                        },
                    ));
                }
                ValueExpr::FunctionCall { name, mut args } => match name.as_str() {
                    "println" => {
                        for arg in args {
                            instructions.push((
                                Metadata::default(),
                                Instruction::Print {
                                    stuff: arg.gen_mlog_literal(&fn_name, constants),
                                },
                            ))
                        }
                        instructions.push((
                            Metadata::default(),
                            Instruction::Print {
                                stuff: String::from("\"\\n\""),
                            },
                        ))
                    }
                    "print" => {
                        for arg in args {
                            instructions.push((
                                Metadata::default(),
                                Instruction::Print {
                                    stuff: arg.gen_mlog_literal(&fn_name, constants),
                                },
                            ))
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
                        instructions
                            .push((Metadata::default(), Instruction::PrintFlush { to: ident }))
                    }
                    other => {
                        todo!("Cannot call a non-builtin function. (attempting to call `{other}`)")
                    }
                },
                ValueExpr::Op { left, op, right } => {
                    instructions.push((
                        Metadata::default(),
                        Instruction::Operation {
                            op: op.into(),
                            out: binding.gen_ident(&fn_name, constants),
                            a: left.gen_mlog_literal(&fn_name, constants),
                            b: right.gen_mlog_literal(&fn_name, constants),
                        },
                    ));
                }
                #[allow(unreachable_patterns)]
                other => panic!("Unsuported ValueExpr: {other:#?}"),
            },
            Operation::Used(expr) => match expr {
                ValueExpr::SimpleValue(SimpleValue::Binding(Binding::Temporary(..))) => {}
                other => unreachable!(
                    "Operation::Used should only contain temporary bindings. found: {other:?}"
                ),
            },
            Operation::While { condition, condition_code, code } => {
                let condition = match condition {
                    ValueExpr::SimpleValue(val) => val.gen_mlog_literal(&fn_name, constants),
                    _ => unreachable!("While statement condition must be a simple value"),
                };
                let loop_start = Uuid::new_v4();
                let loop_end = Uuid::new_v4();
                instructions.push((
                    Metadata {
                        tag: Some(loop_start),
                    },
                    Instruction::NoOp,
                ));
                instructions.extend(
                    gen_instructions(fn_name.clone(), condition_code, constants, constant_idents).into_iter(),
                );
                instructions.push((
                    Metadata::default(),
                    Instruction::Jump {
                        addr: crate::codegen::MlogAddr::Tag(loop_end),
                        condition: crate::codegen::JumpCondition::NotEqual,
                        args: Some((condition, String::from("true"))),
                    },
                ));
                instructions.extend(
                    gen_instructions(fn_name.clone(), code, constants, constant_idents).into_iter(),
                );
                instructions.push((
                    Metadata::default(),
                    Instruction::Jump {
                        addr: crate::codegen::MlogAddr::Tag(loop_start),
                        condition: crate::codegen::JumpCondition::Always,
                        args: None,
                    },
                ));
                instructions.push((
                    Metadata {
                        tag: Some(loop_end),
                    },
                    Instruction::NoOp,
                ));
            }
            Operation::IfChain { chain, base_case } => {
                let chain_end_tag = Uuid::new_v4();

                for link in chain {
                    let If { condition, condition_code, if_true } = link;
                    let after_tag = Uuid::new_v4();
                    let condition = match condition {
                        ValueExpr::SimpleValue(val) => val.gen_mlog_literal(&fn_name, constants),
                        _ => unreachable!("If statement condition must be a simple value"),
                    };
                    let condition_instrs = gen_instructions(fn_name.clone(), condition_code, constants, constant_idents);
                    instructions.extend(condition_instrs.into_iter());
                    instructions.push((
                        Metadata::default(),
                        Instruction::Jump {
                            addr: crate::codegen::MlogAddr::Tag(after_tag),
                            condition: crate::codegen::JumpCondition::NotEqual,
                            args: Some((condition, String::from("true"))),
                        },
                    ));
                    let if_true_instrs = gen_instructions(fn_name.clone(), if_true, constants, constant_idents);
                    instructions.extend(if_true_instrs.into_iter());
                    instructions.push((
                        Metadata::default(),
                        Instruction::Jump {
                            addr: crate::codegen::MlogAddr::Tag(chain_end_tag),
                            condition: crate::codegen::JumpCondition::Always,
                            args: None,
                        },
                    ));
                    instructions.push((
                        Metadata {
                            tag: Some(after_tag),
                        },
                        Instruction::NoOp,
                    ));
                }

                if let Some(base_case) = base_case {
                    instructions.extend(
                        gen_instructions(fn_name.clone(), base_case, constants, constant_idents).into_iter(),
                    );
                }

                instructions.push((
                    Metadata {
                        tag: Some(chain_end_tag),
                    },
                    Instruction::NoOp,
                ));
            }
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

/// offset: address of the first instruction in instrs
pub fn make_tags_absoulute(
    instrs: Vec<(Metadata, Instruction)>,
    offset: usize,
) -> Vec<Instruction> {
    let mut map: HashMap<Uuid, usize> = HashMap::new();
    let mut current_instr = 0usize + offset;
    for (meta, _) in &instrs {
        match meta.tag {
            Some(tag) => {
                map.insert(tag, current_instr);
            }
            None => {}
        }
        current_instr += 1;
    }
    instrs
        .into_iter()
        .map(|(_, b)| match b {
            Instruction::Jump {
                addr: MlogAddr::Tag(tag),
                condition,
                args,
            } => Instruction::Jump {
                addr: MlogAddr::Raw(
                    *map.get(&tag)
                        .expect("Tag {tag} was used but it was never defined!"),
                ),
                condition,
                args,
            },
            other => other,
        })
        .collect()
}

pub fn gen_instructions_for_function(
    fn_name: String,
    arguments: Vec<FunctionArguments>,
    code: Vec<Operation>,
    constants: &HashMap<String, ValueExpr>,
    constant_idents: &HashMap<String, String>,
    stack_memecell_name: String,
    stack_base_addr: usize,
    stack_size: usize,
) -> Vec<(Metadata, Instruction)> {
    todo!()
}
