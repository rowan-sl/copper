pub mod visitor;
pub mod visitors;

use anyhow::{bail, Result};

use crate::parsing::{
    parser::ast::{Ast, AstNode, Expr, self},
};

use visitor::VisitingIteratorExt;
use visitors::{
    constants::{ConstantMap, CstIdentMap, ConstantCollector, CstIdentCollector},
    raw_functions::{RawFnMap, RawFnCollector},
};

/// placeholder
pub fn check_ast(_ast: &Ast) -> Result<(), std::convert::Infallible> {
    Ok(())
}

#[derive(Debug, Clone)]
pub struct Program {
    pub global_const: ConstantMap,
    pub const_idents: CstIdentMap,
    pub raw_functions: RawFnMap,
    pub main_function: ast::Block,
}

pub fn interpret_ast(ast: Ast) -> Result<Program> {
    let mut const_collector = ConstantCollector::default();
    let mut const_ident_collector = CstIdentCollector::default();
    let mut raw_fn_collector = RawFnCollector::default();

    let extra_nodes = match ast.root {
        AstNode::Block(b) => b,
        _ => unreachable!(),
    }
        .subnodes
        .into_iter()
        .map(|n| match n {
            AstNode::Expr(e) => Ok(e),
            AstNode::Block(b) => bail!("Error: block in root scope: {b:#?}"),
        })
        .collect::<Result<Vec<Expr>>>()?
        .into_iter()
        .map(|e| Ok(e))
        .visitor(&mut const_collector)
        .visitor(&mut const_ident_collector)
        .visitor(&mut raw_fn_collector)
        .collect::<Result<Vec<Expr>>>()?;

    info!("Reamining nodes: {:#?}", extra_nodes);

    let prog = Program {
        global_const: const_collector.0,
        const_idents: const_ident_collector.0,
        main_function: raw_fn_collector.main_fn.unwrap(),
        raw_functions: raw_fn_collector.map,
    };

    Ok(prog)
}
