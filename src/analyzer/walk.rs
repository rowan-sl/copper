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

    // let mut alloc = TmpVarAllocator::new();
    // let return_binding = alloc.next();
    // let mut output = vec![];
    // AstNode::Math {
    //     left: Box::new(AstNode::NumLiteral(
    //         1.0,
    //     )),
    //     oper: MathOp::Add,
    //     right: Box::new(AstNode::Math {
    //         left: Box::new(AstNode::NumLiteral(
    //             2.0,
    //         )),
    //         oper: MathOp::Div,
    //         right: Box::new(AstNode::NumLiteral(
    //             2.0,
    //         )),
    //     }),
    // }.traverse_simplify(return_binding, &mut alloc, &mut output);
    // info!("simplified: {:#?}", output);
}
