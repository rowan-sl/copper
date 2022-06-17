pub mod visitor;
pub mod visitors;
pub mod walk;

use anyhow::Result;

use crate::parse2::AstNode;

use visitor::VisitingIteratorExt;
use visitors::{
    constants::{ConstantCollector, ConstantMap, CstIdentCollector, CstIdentMap},
    raw_functions::{RawFnCollector, RawFnMap},
};

#[derive(Debug, Clone)]
pub struct Program {
    pub global_const: ConstantMap,
    pub const_idents: CstIdentMap,
    pub raw_functions: RawFnMap,
}

pub fn interpret_ast(ast: AstNode) -> Result<Program> {
    let mut const_collector = ConstantCollector::default();
    let mut const_ident_collector = CstIdentCollector::default();
    let mut raw_fn_collector = RawFnCollector::default();

    let extra_nodes = match ast {
        AstNode::Block { inner } => inner,
        _ => unreachable!(),
    }
    .into_iter()
    .map(|e| Ok(e))
    .visitor(&mut const_collector)
    .visitor(&mut const_ident_collector)
    .visitor(&mut raw_fn_collector)
    .collect::<Result<Vec<AstNode>>>()?;

    if !extra_nodes.is_empty() {
        warn!("Nodes remain after walking AST: {:#?}", extra_nodes);
    }

    let prog = Program {
        global_const: const_collector.0,
        const_idents: const_ident_collector.0,
        raw_functions: raw_fn_collector.map,
    };

    Ok(prog)
}
