use std::collections::HashMap;

use anyhow::{bail, Result};

use crate::{
    analyzer::visitor::Visitor,
    parsing::parser::ast::{Expr, Block, Argument},
};

#[derive(Debug, Clone, PartialEq)]
pub struct RawFn {
    args: Vec<Argument>,
    code: Block,
}

pub type RawFnMap = HashMap<String, RawFn>; // name, value

#[derive(Debug, Clone, PartialEq, Default)]
pub struct RawFnCollector {
    pub map: RawFnMap,
    pub main_fn: Option<Block>,
}

impl Visitor for RawFnCollector {
    fn visit_expr(&mut self, expr: Expr) -> Result<Option<Expr>> {
        match expr {
            Expr::MainFn { code } => {
                if self.main_fn.is_some() {
                    bail!("Multiple main functions??? what am I supposed to do with these!")
                }
                self.main_fn = Some(code);
                Ok(None)
            }
            Expr::FnDef { ident, args, code } => {
                match self.map.insert(ident.clone(), RawFn { args, code }) {
                    None => Ok(None),
                    Some(_) => {
                        bail!("Function {} defined twice!", ident);
                    }
                }
            }
            other => Ok(Some(other))
        }
    }
}
