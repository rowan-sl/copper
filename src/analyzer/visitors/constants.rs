use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    analyzer::visitor::Visitor,
    parse2::{types::ast_types::ASTType, AstNode},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    pub typ: ASTType,
    pub value: AstNode,
}

pub type ConstantMap = HashMap<String, Constant>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ConstantCollector(pub ConstantMap);

impl Visitor for ConstantCollector {
    fn visit_node(&mut self, node: AstNode) -> Result<Option<AstNode>> {
        match node {
            AstNode::Const { ident, typ, value } => {
                match self
                    .0
                    .insert(ident.clone(), Constant { typ, value: *value })
                {
                    None => Ok(None),
                    Some(_) => {
                        bail!("Constant {} defined twice!", ident);
                    }
                }
            }
            other => Ok(Some(other)),
        }
    }
}

pub type CstIdentMap = HashMap<String, String>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct CstIdentCollector(pub CstIdentMap);

impl Visitor for CstIdentCollector {
    fn visit_node(&mut self, node: AstNode) -> Result<Option<AstNode>> {
        match node {
            AstNode::ConstIdent { ident, value } => match self.0.insert(ident.clone(), value) {
                None => Ok(None),
                Some(_) => {
                    bail!("Constant ident {} defined twice!", ident);
                }
            },
            other => Ok(Some(other)),
        }
    }
}
