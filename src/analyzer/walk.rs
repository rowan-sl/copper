use std::collections::HashMap;

use anyhow::Result;

use super::visitors::{constants::{ConstantMap, CstIdentMap}, raw_functions::RawFnMap};
use crate::{parse2::{AstBlock, AstNode, MathOp}, util::TmpVarAllocator};

#[derive(Debug, Clone, PartialEq)]
pub struct Local { }

pub fn walk_controll_flow(
    code: AstBlock,
    functions: &RawFnMap,
    global_consts: &ConstantMap,
    global_ident_bindings: &CstIdentMap,
) {
    let mut locals: HashMap<String, Local> = HashMap::new();

    // let node = match code.remove(0) {
    //     AstNode::Let { ident: _, typ: _, value } => value,
    //     _ => panic!("e")
    // };

    // let mut alloc = TmpVarAllocator::new();
    // let return_binding = alloc.next();
    // let mut output = vec![];
    // node.traverse_simplify_group(return_binding, &mut alloc, &mut output);
    // info!("simplified: {:#?}", output);
    info!("Simplified main fn: {:#?}", AstNode::Block { inner: code }.simplify());
}
