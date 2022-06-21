use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    analyzer::visitor::Visitor,
    parse2::{
        types::{ast_types::ASTType, functions::FunctionArguments},
        AstBlock, AstNode,
    },
};

#[derive(Debug, Clone, PartialEq)]
pub struct RawFn {
    pub args: Vec<FunctionArguments>,
    pub code: AstBlock,
    pub ret_typ: ASTType,
}

pub type RawFnMap = HashMap<String, RawFn>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RawFnCollector {
    pub map: RawFnMap,
}

impl Visitor for RawFnCollector {
    fn visit_node(&mut self, node: AstNode) -> Result<Option<AstNode>> {
        match node {
            AstNode::FunctionDef {
                name,
                args,
                ret_typ,
                code,
            } => {
                match self.map.insert(
                    name.clone(),
                    RawFn {
                        args,
                        code: code.unwrap(),
                        ret_typ,
                    },
                ) {
                    None => Ok(None),
                    Some(_) => {
                        bail!("Function {} defined twice!", name);
                    }
                }
            }
            other => Ok(Some(other)),
        }
    }
}
