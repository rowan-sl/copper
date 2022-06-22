//! low-level intermediate representation

use crate::parse2::{types::lexer_tokens::Op, AstNode};

#[derive(Debug, Clone, PartialEq)]
pub enum Binding {
    Temporary(u64),
    Variable(String),
}

impl Binding {
    pub fn from_ast_node(node: AstNode) -> Result<Self, AstNode> {
        match node {
            AstNode::Ident(name) => Ok(Binding::Variable(name)),
            AstNode::TmpBinding { local_id } => Ok(Binding::Temporary(local_id)),
            other => Err(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum SimpleValue {
    NumLiteral(f64),
    StrLiteral(String),
    BoolLiteral(bool),
    Binding(Binding),
}

impl SimpleValue {
    pub fn from_ast_node(node: AstNode) -> Result<Self, AstNode> {
        match node {
            AstNode::NumLiteral(n) => Ok(SimpleValue::NumLiteral(n)),
            AstNode::StrLiteral(s) => Ok(SimpleValue::StrLiteral(s)),
            AstNode::BoolLiteral(b) => Ok(SimpleValue::BoolLiteral(b)),
            node @ (AstNode::Ident(..) | AstNode::TmpBinding { .. }) => {
                Ok(SimpleValue::Binding(Binding::from_ast_node(node).unwrap()))
            }
            other => Err(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueExpr {
    SimpleValue(SimpleValue),
    Op {
        left: SimpleValue,
        op: Op,
        right: SimpleValue,
    },
    FunctionCall {
        name: String,
        args: Vec<SimpleValue>,
    },
}

impl ValueExpr {
    pub fn ast_node_is_value_expr(node: &AstNode) -> bool {
        match node {
            AstNode::NumLiteral(..)
            | AstNode::StrLiteral(..)
            | AstNode::BoolLiteral(..)
            | AstNode::FunctionCall { .. }
            | AstNode::TmpBinding { .. }
            | AstNode::Op { .. }
            | AstNode::Ident(..) => true,
            _ => false,
        }
    }

    pub fn from_ast_node(node: AstNode) -> Result<Self, AstNode> {
        match node {
            node @ (AstNode::NumLiteral(..)
            | AstNode::StrLiteral(..)
            | AstNode::BoolLiteral(..)
            | AstNode::Ident(..)
            | AstNode::TmpBinding { .. }) => Ok(ValueExpr::SimpleValue(
                SimpleValue::from_ast_node(node).unwrap(),
            )),
            AstNode::Op { left, oper, right } => Ok(ValueExpr::Op {
                left: SimpleValue::from_ast_node(*left)
                    .expect("Left value of an operation must be a simple value"),
                op: oper,
                right: SimpleValue::from_ast_node(*right)
                    .expect("Right value of an operation must be a simple value"),
            }),
            AstNode::FunctionCall { name, args } => Ok(ValueExpr::FunctionCall {
                name,
                args: args
                    .into_iter()
                    .map(|a| {
                        SimpleValue::from_ast_node(a.args)
                            .expect("Function arg must be a simple value")
                    })
                    .collect(),
            }),
            other => Err(other),
        }
    }

    /// external bindings that this makes use of.
    pub fn uses(&self) -> Vec<Binding> {
        match self {
            ValueExpr::SimpleValue(SimpleValue::Binding(b)) => vec![b.clone()],
            ValueExpr::SimpleValue(..) => vec![],
            ValueExpr::Op { left, op: _, right } => {
                let mut bindings = vec![];
                if let SimpleValue::Binding(b) = left {
                    bindings.push(b.clone());
                }
                if let SimpleValue::Binding(b) = right {
                    bindings.push(b.clone());
                }
                bindings
            }
            ValueExpr::FunctionCall { name: _, args } => {
                let mut bindings = vec![];
                for arg in args {
                    if let SimpleValue::Binding(b) = arg {
                        bindings.push(b.clone())
                    }
                }
                bindings
            }
        }
    }

    /// function bindings that this makes use of
    pub fn uses_functions(&self) -> Vec<String> {
        // returns Vec because there might be more than one thing returned in the future
        if let ValueExpr::FunctionCall { name, args: _ } = self {
            vec![name.clone()]
        } else {
            vec![]
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition_code: Vec<Operation>,
    pub condition: ValueExpr,
    pub if_true: Vec<Operation>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Bind(Binding, ValueExpr),
    Update(Binding, ValueExpr),
    // Unbind(Binding), might not be used?
    IfChain {
        chain: Vec<If>,
        base_case: Option<Vec<Operation>>,
    },
    Return(Option<ValueExpr>),
    While {
        condition: ValueExpr,
        code: Vec<Operation>,
    },
    /// "uses" an expr (mostly for temporary binding useage checks)
    Used(ValueExpr),
}
