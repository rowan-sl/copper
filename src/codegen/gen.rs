use std::collections::HashMap;

use uuid::Uuid;

use crate::analyzer::visitors::raw_functions::RawFn;
use crate::codegen::{Instruction, MlogAddr};
use crate::lir::{Binding, If, Operation, SimpleValue, ValueExpr};

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

fn make_const_ident(raw_ident: &str) -> String {
    format!("const_{raw_ident}")
}

fn make_tmp_ident(raw_ident: u64, function: &str) -> String {
    format!("tmp_{function}_{raw_ident}")
}

/// Caller MUST guarentee that the produced ident will not overlaps
fn make_internal_tmp_ident(raw_ident: &str) -> String {
    format!("i_tmp_{raw_ident}")
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
    pub tag: Option<Uuid>,
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
    stack_memcell_name: String,
    // also addr of the stack pointer
    stack_base_addr: usize,
    stack_size: usize,
    functions: &HashMap<String, RawFn>,
    // this tagging of functions pre-codegen is needed because we need a tag to identify all functions before the code is ever generated,
    // so that call instructions can be generated for functions that are not yet defined (if that makes any sense)
    function_tags: &HashMap<String, Uuid>,
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
                        let call_instrs = gen_instructions_for_function_call(
                            fn_name.clone(),
                            other.to_string(),
                            args,
                            binding.gen_ident(&fn_name, constants),
                            stack_memcell_name.clone(),
                            stack_base_addr,
                            stack_size,
                            constants,
                            functions,
                            function_tags,
                        );
                        instructions.extend(call_instrs);
                        // todo!("Cannot call a non-builtin function. (attempting to call `{other}`)")
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
                // #[allow(unreachable_patterns)]
                // other => panic!("Unsuported ValueExpr: {other:#?}"),
            },
            Operation::Used(expr) => match expr {
                ValueExpr::SimpleValue(SimpleValue::Binding(Binding::Temporary(..))) => {}
                other => unreachable!(
                    "Operation::Used should only contain temporary bindings. found: {other:?}"
                ),
            },
            Operation::While {
                condition,
                condition_code,
                code,
            } => {
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
                    gen_instructions(fn_name.clone(), condition_code, constants, constant_idents, stack_memcell_name.clone(), stack_base_addr, stack_size, functions, function_tags)
                        .into_iter(),
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
                    gen_instructions(fn_name.clone(), code, constants, constant_idents, stack_memcell_name.clone(), stack_base_addr, stack_size, functions, function_tags).into_iter(),
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
                    let If {
                        condition,
                        condition_code,
                        if_true,
                    } = link;
                    let after_tag = Uuid::new_v4();
                    let condition = match condition {
                        ValueExpr::SimpleValue(val) => val.gen_mlog_literal(&fn_name, constants),
                        _ => unreachable!("If statement condition must be a simple value"),
                    };
                    let condition_instrs = gen_instructions(
                        fn_name.clone(),
                        condition_code,
                        constants,
                        constant_idents, stack_memcell_name.clone(), stack_base_addr, stack_size, functions, function_tags
                    );
                    instructions.extend(condition_instrs.into_iter());
                    instructions.push((
                        Metadata::default(),
                        Instruction::Jump {
                            addr: crate::codegen::MlogAddr::Tag(after_tag),
                            condition: crate::codegen::JumpCondition::NotEqual,
                            args: Some((condition, String::from("true"))),
                        },
                    ));
                    let if_true_instrs =
                        gen_instructions(fn_name.clone(), if_true, constants, constant_idents, stack_memcell_name.clone(), stack_base_addr, stack_size, functions, function_tags);
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
                        gen_instructions(fn_name.clone(), base_case, constants, constant_idents, stack_memcell_name.clone(), stack_base_addr, stack_size, functions, function_tags)
                            .into_iter(),
                    );
                }

                instructions.push((
                    Metadata {
                        tag: Some(chain_end_tag),
                    },
                    Instruction::NoOp,
                ));
            }
            Operation::Return(value) => {
                let value = match value {
                    Some(ValueExpr::SimpleValue(value)) => Some(value.gen_mlog_literal(&fn_name, constants)),
                    Some(_) => panic!("Value of return must be a simple value"),
                    None => None,
                };
                let insts = gen_instructions_for_return(value, stack_memcell_name.clone(), stack_base_addr);
                instructions.extend(insts);
            }
            // #[allow(unreachable_patterns)]
            // other => panic!("Unsuported operation: {other:#?}"),
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
    for (meta, instr) in &instrs {
        if let Instruction::Comment(..) = instr {
            continue;
        }
        match meta.tag {
            Some(tag) => {
                map.insert(tag, current_instr);
            }
            None => {}
        }
        current_instr += instr.worth();
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
            Instruction::SetToTagAddr { tag, ident } => Instruction::Set {
                ident,
                value: map
                    .get(&tag)
                    .expect("Tag {tag} was used but it was never defined!")
                    .to_string(),
            },
            other => other,
        })
        .collect()
}

pub fn gen_instructions_for_function(
    fn_name: String,
    // arguments: Vec<FunctionArguments>,
    code: Vec<Operation>,
    constants: &HashMap<String, ValueExpr>,
    constant_idents: &HashMap<String, String>,
    stack_memcell_name: String,
    // also addr of the stack pointer
    stack_base_addr: usize,
    stack_size: usize,
    functions: &HashMap<String, RawFn>,
    // this tagging of functions pre-codegen is needed because we need a tag to identify all functions before the code is ever generated,
    // so that call instructions can be generated for functions that are not yet defined (if that makes any sense)
    function_tags: &HashMap<String, Uuid>,
) -> Vec<(Metadata, Instruction)> {
    // Note: function parameters are already initialized by the time that a function is jumped to (with the same name)
    // so no intialization of the params must take place here

    let mut instructions: Vec<(Metadata, Instruction)> = vec![];
    instructions.push((
        Metadata {
            tag: Some(*function_tags.get(&fn_name).unwrap()),
        },
        Instruction::NoOp,
    ));

    instructions.extend(gen_instructions(
        fn_name.clone(),
        code,
        constants,
        constant_idents,
        stack_memcell_name,
        stack_base_addr,
        stack_size,
        functions,
        function_tags,
    ));

    // juuuust in case
    instructions.push((
        Metadata::default(),
        Instruction::Print {
            stuff: format!(
                "\"Error: function `{fn_name}` does not include a return statement\\n\""
            ),
        },
    ));
    instructions.push((Metadata::default(), Instruction::Trap));

    instructions
}

pub fn gen_instructions_for_function_call(
    // name of the function this is being called from
    calling_fn_name: String,
    // name of the function being called
    fn_name: String,
    called_with: Vec<SimpleValue>,
    out_ident: String,
    stack_memcell_name: String,
    // also addr of the stack pointer
    stack_base_addr: usize,
    stack_size: usize,
    constants: &HashMap<String, ValueExpr>,
    functions: &HashMap<String, RawFn>,
    // this tagging of functions pre-codegen is needed because we need a tag to identify all functions before the code is ever generated,
    // so that call instructions can be generated for functions that are not yet defined (if that makes any sense)
    function_tags: &HashMap<String, Uuid>,
) -> Vec<(Metadata, Instruction)> {
    let callee_def = functions.get(&fn_name).unwrap();
    assert_eq!(
        callee_def.args.len(),
        called_with.len(),
        "Error: Function called with the wrong number of arguments: expected {}, found {}",
        callee_def.args.len(),
        called_with.len()
    );

    let mut instructions: Vec<(Metadata, Instruction)> = vec![];
    let after_id = Uuid::new_v4();

    // initialize the functions arguments
    for (arg, binding) in called_with.into_iter().zip(callee_def.args.clone()) {
        instructions.push((
            Metadata::default(),
            Instruction::Set {
                ident: make_ident(&binding.ident, &fn_name, constants),
                value: arg.gen_mlog_literal(&calling_fn_name, constants),
            },
        ));
    }

    // read stack ptr
    let sp_ident = make_internal_tmp_ident("sp");
    instructions.push((
        Metadata::default(),
        Instruction::Read {
            out_var: sp_ident.clone(),
            bank_id: stack_memcell_name.clone(),
            addr: stack_base_addr.to_string(),
        },
    ));
    // stack overflow checks
    let after_check_tag = Uuid::new_v4();
    instructions.push((
        Metadata::default(),
        Instruction::Jump {
            addr: MlogAddr::Tag(after_check_tag),
            condition: crate::codegen::JumpCondition::LessThan,
            args: Some((sp_ident.clone(), (stack_base_addr + stack_size).to_string())),
        },
    ));
    // oh no! we did a oopsie
    instructions.push((
        Metadata::default(),
        Instruction::Print {
            stuff: format!("\"Error: function `{calling_fn_name}` overflowed its stack!\\n\""),
        },
    ));
    instructions.push((Metadata::default(), Instruction::Trap));

    // perform the jump (ish)
    instructions.push((
        Metadata {
            tag: Some(after_check_tag),
        },
        Instruction::Operation {
            op: super::Operation::Add,
            out: sp_ident.clone(),
            a: sp_ident.clone(),
            b: 1.to_string(),
        },
    ));
    let retaddr_ident = make_internal_tmp_ident("retaddr");
    instructions.push((
        Metadata::default(),
        Instruction::SetToTagAddr {
            tag: after_id,
            ident: retaddr_ident.clone(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Write {
            in_var: retaddr_ident,
            bank_id: stack_memcell_name.clone(),
            addr: sp_ident.clone(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Write {
            in_var: sp_ident,
            bank_id: stack_memcell_name.clone(),
            addr: stack_base_addr.to_string(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::SetToTagAddr {
            tag: *function_tags.get(&fn_name).unwrap(),
            ident: "@counter".to_string(),
        },
    ));
    instructions.push((
        Metadata {
            tag: Some(after_id),
        },
        Instruction::NoOp,
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Set {
            ident: out_ident,
            value: make_internal_tmp_ident("rvalue"),
        },
    ));

    instructions
}

pub fn gen_instructions_for_return(
    // variable that contains the return value
    return_value_ident: Option<String>,
    stack_memcell_name: String,
    stack_base_addr: usize,
) -> Vec<(Metadata, Instruction)> {
    let mut instructions: Vec<(Metadata, Instruction)> = vec![];

    // return value
    instructions.push((
        Metadata::default(),
        Instruction::Set {
            ident: make_internal_tmp_ident("rvalue"),
            value: return_value_ident.unwrap_or("null".to_string()),
        },
    ));

    // perform jump
    let sp_ident = make_internal_tmp_ident("sp");
    let retaddr_ident = make_internal_tmp_ident("retaddr");
    instructions.push((
        Metadata::default(),
        Instruction::Read {
            out_var: sp_ident.clone(),
            bank_id: stack_memcell_name.clone(),
            addr: stack_base_addr.to_string(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Read {
            out_var: retaddr_ident.clone(),
            bank_id: stack_memcell_name.clone(),
            addr: sp_ident.clone(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Operation {
            op: super::Operation::Sub,
            out: sp_ident.clone(),
            a: sp_ident.clone(),
            b: 1.to_string(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Write {
            in_var: sp_ident,
            bank_id: stack_memcell_name.clone(),
            addr: stack_base_addr.to_string(),
        },
    ));
    instructions.push((
        Metadata::default(),
        Instruction::Set {
            ident: "@counter".into(),
            value: retaddr_ident,
        },
    ));

    instructions
}
