use std::collections::HashMap;

use anyhow::Result;

use crate::{
    analyzer::visitors::{
        constants::{ConstantMap, CstIdentMap},
        raw_functions::RawFnMap,
    },
    lir::{self, Binding, If, ValueExpr},
    parse2::{types::functions::FunctionArguments, AstNode},
    util::take_n,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Local {
    value: ValueExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Temporary {
    value: ValueExpr,
}

#[derive(Debug)]
pub struct ScopeHelper {}

impl ScopeHelper {
    pub const fn new() -> Self {
        Self {}
    }
}

#[derive(thiserror::Error, Clone, Debug)]
pub enum ValueExprUseageError {
    #[error("Variable {0} was used, but it has not yet been defined!")]
    UseBeforeDefine(String),
    #[error("Temporary binding {0} was used before it was defined! (internal error)")]
    TemporaryUseBeforeDefine(u64),
    #[error("Use of undefined function {0}")]
    UndefinedFunction(String),
}

fn validate_valuexpr_useage(
    expr: &ValueExpr,
    current_fn_args: &Vec<FunctionArguments>,
    functions: &RawFnMap,
    consts: &ConstantMap,
    ident_bindings: &CstIdentMap,
    locals: &HashMap<String, Local>,
    temporaries: &HashMap<u64, Temporary>,
) -> Result<(), ValueExprUseageError> {
    let used_bindings = expr.uses();
    let used_functions = expr.uses_functions();

    for binding in used_bindings {
        match binding {
            Binding::Variable(name) => {
                let args_has = current_fn_args
                    .iter()
                    .fold(false, |acc, x| acc || x.ident == name);
                if !(consts.contains_key(&name)
                    || ident_bindings.contains_key(&name)
                    || locals.contains_key(&name)
                    || args_has)
                {
                    return Err(ValueExprUseageError::UseBeforeDefine(name));
                }
            }
            Binding::Temporary(id) => {
                if !temporaries.contains_key(&id) {
                    return Err(ValueExprUseageError::TemporaryUseBeforeDefine(id));
                }
            }
        }
    }

    for function in used_functions {
        if !functions.contains_key(&function) {
            return Err(ValueExprUseageError::UndefinedFunction(function));
        }
    }

    Ok(())
}

fn valuexpr_update_temporaries(expr: &ValueExpr, temporaries: &mut HashMap<u64, Temporary>) {
    let used_bindings = expr.uses();

    for binding in used_bindings {
        if let Binding::Temporary(id) = binding {
            if let None = temporaries.remove(&id) {
                unreachable!(
                    "please validate temporaries useage before calling update_temporaries"
                );
            }
        }
    }
}

pub fn walk_controll_flow(
    fn_name: String,
    code: Vec<AstNode>,
    arguments: &Vec<FunctionArguments>,
    functions: &RawFnMap,
    global_consts: &ConstantMap,
    global_ident_bindings: &CstIdentMap,
) {
    let mut locals: HashMap<String, Local> = HashMap::new();
    let mut temporaries: HashMap<u64, Temporary> = HashMap::new();
    // let mut helper = ScopeHelper::new();

    let mut simplified = AstNode::Block { inner: code }.simplify();
    debug_assert_eq!(simplified.len(), 1);
    let simplified = if let AstNode::Block { inner } = simplified.pop().unwrap() {
        inner
    } else {
        unreachable!();
    };
    info!("Simplified AST for function `{fn_name}`: {simplified:#?}");

    let global_lir: Vec<lir::Operation>;

    #[derive(Debug, Clone)]
    pub enum LateOperation {
        IfExpr {
            condition_code: Vec<lir::Operation>,
            condition: ValueExpr,
            if_true: Vec<lir::Operation>,
        },
        ElseIfExpr {
            condition_code: Vec<lir::Operation>,
            condition: ValueExpr,
            if_true: Vec<lir::Operation>,
        },
        ElseExpr {
            code: Vec<lir::Operation>,
        },
    }

    #[derive(Debug, Clone)]
    pub enum OnScopeFinish {
        /// exit the function with the current LIR scope
        Done,
        /// condition code parsing for conditionals
        FillConditionalCode,
        /// an if expression that requires a block (incomplete). next stage on the stack will contain the scope that this belongs to, so place it there
        IfExpr {
            condition: ValueExpr,
            condition_code: Option<Vec<lir::Operation>>,
        },
        ElseIfExpr {
            condition: ValueExpr,
            condition_code: Option<Vec<lir::Operation>>,
        },
        ElseExpr,
    }

    #[derive(Debug, Clone)]
    pub struct Frame {
        once_done: OnScopeFinish,
        code: Vec<AstNode>,
        current_lir: Vec<lir::Operation>,
    }

    let mut stack: Vec<Frame> = vec![Frame {
        once_done: OnScopeFinish::Done,
        code: simplified,
        current_lir: vec![],
    }];

    let mut late_opers: Vec<LateOperation> = vec![];

    'walk: loop {
        let Frame {
            once_done,
            mut code,
            current_lir: mut lir,
        } = match stack.pop() {
            Some(f) => f,
            None => unreachable!(),
        };

        for oper in late_opers.drain(..) {
            match oper {
                LateOperation::IfExpr {
                    condition_code,
                    condition,
                    if_true,
                } => match lir.last_mut() {
                    Some(lir::Operation::IfChain { chain, base_case }) => {
                        // if this fails something terrible is going on
                        debug_assert!(chain.is_empty());
                        debug_assert!(base_case.is_none());
                        chain.push(If {
                            condition_code,
                            condition,
                            if_true,
                        });
                    }
                    other => panic!("Late operation failure: expected if chain, found {other:#?}"),
                },
                LateOperation::ElseIfExpr {
                    condition_code,
                    condition,
                    if_true,
                } => match lir.last_mut() {
                    Some(lir::Operation::IfChain { chain, base_case }) => {
                        debug_assert!(!chain.is_empty()); // if this fails, something terrible has happened
                        assert!(
                            base_case.is_none(),
                            "`else if` cannot be preceeded by `else`"
                        );
                        chain.push(If {
                            condition_code,
                            condition,
                            if_true,
                        });
                    }
                    other => panic!("Late operation failure: expected if chain, found {other:#?}"),
                },
                LateOperation::ElseExpr { code } => match lir.last_mut() {
                    Some(lir::Operation::IfChain { chain, base_case }) => {
                        debug_assert!(!chain.is_empty()); // if this fails, something terrible has happened
                        assert!(base_case.is_none(), "Cannot have multiple else cases!");
                        *base_case = Some(code);
                    }
                    other => panic!("Late operation failure: expected if chain, found {other:#?}"),
                },
            }
        }

        while let Some(mut next) = take_n(&mut code, 1) {
            info!("{next:?}");
            assert_eq!(next.len(), 1);
            let next = next.pop().unwrap();
            match next {
                AstNode::BindTmp { local_id, value } => {
                    let v_expr = ValueExpr::from_ast_node(*value)
                        .expect("Value of temp binding is not a value expr: found {value:#?}");
                    if let Err(e) = validate_valuexpr_useage(
                        &v_expr,
                        &arguments,
                        &functions,
                        &global_consts,
                        &global_ident_bindings,
                        &locals,
                        &temporaries,
                    ) {
                        panic!("Error validating value expr: {e:#?}");
                    }
                    valuexpr_update_temporaries(&v_expr, &mut temporaries);
                    if let Some(..) = temporaries.insert(local_id, {
                        Temporary {
                            value: v_expr.clone(),
                        }
                    }) {
                        panic!("Local temporary binding defined twice: {local_id}");
                    }
                    lir.push(lir::Operation::Bind(Binding::Temporary(local_id), v_expr))
                }
                AstNode::Let {
                    ident,
                    typ: _, /* deal with this when typeck comes around */
                    value,
                } => {
                    let v_expr = ValueExpr::from_ast_node(*value)
                        .expect("Value of binding is not a value expr: found {value:#?}");
                    if let Err(e) = validate_valuexpr_useage(
                        &v_expr,
                        &arguments,
                        &functions,
                        &global_consts,
                        &global_ident_bindings,
                        &locals,
                        &temporaries,
                    ) {
                        panic!("Error validating value expr: {e:#?}");
                    }
                    valuexpr_update_temporaries(&v_expr, &mut temporaries);
                    if global_consts.contains_key(&ident) {
                        panic!("Local variable defined with same name as existing constant! name: {ident:?}");
                    }
                    if global_ident_bindings.contains_key(&ident) {
                        panic!("Local variable defined with same name as existing constant identifier binding! name: {ident:?}");
                    }
                    if let Some(..) = locals.insert(
                        ident.clone(),
                        Local {
                            value: v_expr.clone(),
                        },
                    ) {
                        // not actually an errror lol
                        // panic!("Local variable binding defined twice: {ident}");
                    }
                    lir.push(lir::Operation::Bind(Binding::Variable(ident), v_expr));
                }
                AstNode::Set { ident, value } => {
                    let v_expr = ValueExpr::from_ast_node(*value)
                        .expect("Value of binding is not a value expr: found {value:#?}");
                    if let Err(e) = validate_valuexpr_useage(
                        &v_expr,
                        &arguments,
                        &functions,
                        &global_consts,
                        &global_ident_bindings,
                        &locals,
                        &temporaries,
                    ) {
                        panic!("Error validating value expr: {e:#?}");
                    }
                    valuexpr_update_temporaries(&v_expr, &mut temporaries);
                    if locals
                        .insert(
                            ident.clone(),
                            Local {
                                value: v_expr.clone(),
                            },
                        )
                        .is_none()
                    {
                        panic!(
                            "Attempted to update a variable that does not exist! (name:`{ident}`)"
                        );
                    }
                    lir.push(lir::Operation::Update(Binding::Variable(ident), v_expr));
                }
                AstNode::If {
                    condition_code,
                    condition,
                    code: if_code,
                } => {
                    let condition_code = condition_code.expect("Error while parsing LIR: If expr does not include condition code! (included by simplification)");
                    let condition_vexpr = ValueExpr::from_ast_node(*condition)
                        .expect("Value of condition is not a value expr: found {value:#?}");
                    let if_code = if_code.unwrap();
                    lir.push(lir::Operation::IfChain {
                        chain: vec![],
                        base_case: None,
                    });
                    stack.push(Frame {
                        once_done,
                        code,
                        current_lir: lir,
                    });
                    stack.push(Frame {
                        once_done: OnScopeFinish::IfExpr {
                            condition_code: None,
                            condition: condition_vexpr,
                        },
                        code: if_code,
                        current_lir: vec![],
                    });
                    stack.push(Frame {
                        once_done: OnScopeFinish::FillConditionalCode,
                        code: condition_code,
                        current_lir: vec![],
                    });
                    continue 'walk;
                }
                AstNode::ElseIf {
                    condition_code,
                    condition,
                    code: if_code,
                } => {
                    let condition_code = condition_code.expect("Error while parsing LIR: If expr does not include condition code! (included by simplification)");
                    let condition_vexpr = ValueExpr::from_ast_node(*condition)
                        .expect("Value of condition is not a value expr: found {value:#?}");
                    let if_code = if_code.unwrap();
                    stack.push(Frame {
                        once_done,
                        code,
                        current_lir: lir,
                    });
                    stack.push(Frame {
                        once_done: OnScopeFinish::ElseIfExpr {
                            condition_code: None,
                            condition: condition_vexpr,
                        },
                        code: if_code,
                        current_lir: vec![],
                    });
                    stack.push(Frame {
                        once_done: OnScopeFinish::FillConditionalCode,
                        code: condition_code,
                        current_lir: vec![],
                    });
                    continue 'walk;
                }
                AstNode::Else { code: else_code } => {
                    let else_code = else_code.unwrap();
                    stack.push(Frame {
                        once_done,
                        code,
                        current_lir: lir,
                    });
                    stack.push(Frame {
                        once_done: OnScopeFinish::ElseExpr,
                        code: else_code,
                        current_lir: vec![],
                    });
                    continue 'walk;
                }
                _ => todo!(),
            }
        }

        match once_done {
            OnScopeFinish::Done => {
                global_lir = lir;
                break 'walk;
            }
            OnScopeFinish::IfExpr {
                condition_code,
                condition,
            } => late_opers.push(LateOperation::IfExpr {
                condition_code: condition_code.unwrap(),
                condition,
                if_true: lir,
            }),
            OnScopeFinish::ElseIfExpr {
                condition_code,
                condition,
            } => late_opers.push(LateOperation::ElseIfExpr {
                condition_code: condition_code.unwrap(),
                condition,
                if_true: lir,
            }),
            OnScopeFinish::ElseExpr => late_opers.push(LateOperation::ElseExpr { code: lir }),
            OnScopeFinish::FillConditionalCode => {
                match stack.last_mut() {
                    Some(Frame {
                        once_done:
                            OnScopeFinish::IfExpr {
                                condition,
                                condition_code,
                            }
                            | OnScopeFinish::ElseIfExpr {
                                condition,
                                condition_code,
                            },
                        code: _,
                        current_lir: _,
                    }) => {
                        debug_assert!(condition_code.is_none());
                        *condition_code = Some(lir);

                        // validation takes place here, so that it correctly modified bindings created within condition_code
                        if let Err(e) = validate_valuexpr_useage(
                            &condition,
                            &arguments,
                            &functions,
                            &global_consts,
                            &global_ident_bindings,
                            &locals,
                            &temporaries,
                        ) {
                            panic!("Error validating value expr: {e:#?}");
                        }
                        valuexpr_update_temporaries(&condition, &mut temporaries);
                    }
                    _ => panic!("FillConditionalCode not proceeded by a conditional!"),
                }
            }
        }
    }

    info!("LIR for function `{fn_name}`: {global_lir:#?}");
}
