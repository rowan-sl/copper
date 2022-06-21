use std::collections::HashMap;

use anyhow::Result;

use crate::{
    analyzer::visitors::{
        constants::{ConstantMap, CstIdentMap},
        raw_functions::RawFnMap,
    },
    lir::{self, Binding, ValueExpr},
    parse2::{AstBlock, AstNode},
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
                if !(consts.contains_key(&name)
                    || ident_bindings.contains_key(&name)
                    || locals.contains_key(&name))
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

pub fn walk_controll_flow(
    fn_name: String,
    code: AstBlock,
    functions: &RawFnMap,
    global_consts: &ConstantMap,
    global_ident_bindings: &CstIdentMap,
) {
    let mut locals: HashMap<String, Local> = HashMap::new();
    let mut temporaries: HashMap<u64, Temporary> = HashMap::new();
    // let mut helper = ScopeHelper::new();

    let mut lir: Vec<lir::Operation> = vec![];

    let mut simplified = AstNode::Block { inner: code }.simplify();
    debug_assert_eq!(simplified.len(), 1);
    let mut simplified = if let AstNode::Block { inner } = simplified.pop().unwrap() {
        inner
    } else {
        unreachable!();
    };
    info!("Simplified AST for function `{fn_name}`: {simplified:#?}");

    while let Some(mut next) = take_n(&mut simplified, 1) {
        info!("{next:?}");
        assert_eq!(next.len(), 1);
        let next = next.pop().unwrap();
        match next {
            AstNode::BindTmp { local_id, value } => {
                let v_expr = ValueExpr::from_ast_node(*value)
                    .expect("Value of temp binding is not a value expr: found {value:#?}");
                if let Err(e) = validate_valuexpr_useage(
                    &v_expr,
                    &functions,
                    &global_consts,
                    &global_ident_bindings,
                    &locals,
                    &temporaries,
                ) {
                    panic!("Error validating value expr: {e:#?}");
                }
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
                    &functions,
                    &global_consts,
                    &global_ident_bindings,
                    &locals,
                    &temporaries,
                ) {
                    panic!("Error validating value expr: {e:#?}");
                }
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
                    panic!("Local variable binding defined twice: {ident}");
                }
                lir.push(lir::Operation::Bind(Binding::Variable(ident), v_expr));
            }
            _ => todo!(),
        }
    }

    info!("LIR for function `{fn_name}`: {lir:#?}");
}
