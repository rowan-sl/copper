use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    analyzer::visitor::Visitor,
    parsing::{
        lexer::Literal,
        parser::ast::Expr,
    }
};


#[derive(Debug, Clone, PartialEq)]
pub struct Constant {
    typ: String,
    value: Literal,
}

pub type ConstantMap = HashMap<String, Constant>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct ConstantCollector(pub ConstantMap);

impl Visitor for ConstantCollector {
    fn visit_expr(&mut self, expr: Expr) -> Result<Option<Expr>> {
        match expr {
            Expr::Constant { typ, name, value } => {
                match self.0.insert(name.clone(), Constant { typ, value }) {
                    None => Ok(None),
                    Some(_) => {
                        bail!("Constant {} defined twice!", name);
                    }
                }
            }
            other => Ok(Some(other))
        }
    }
}

pub type CstIdentMap = HashMap<String, String>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct CstIdentCollector(pub CstIdentMap);

impl Visitor for CstIdentCollector {
    fn visit_expr(&mut self, expr: Expr) -> Result<Option<Expr>> {
        match expr {
            Expr::ConstIdent { name, value } => {
                match self.0.insert(name.clone(), value) {
                    None => Ok(None),
                    Some(_) => {
                        bail!("Constant ident {} defined twice!", name);
                    }
                }
            }
            other => Ok(Some(other))
        }
    }
}
