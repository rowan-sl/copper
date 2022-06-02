use super::{scoped::ScopedTokens, ParseError};
use crate::parsing::lexer;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block(Block),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfChain {
    If { condition: Block, folowing: Block },
    ElseIf { condition: Block, folowing: Block },
    Else { folowing: Block },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Ident { name: String },
    BlockExpr(Block),
    Expr(Expr),
    Literal(lexer::Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Constant {
        typ: String,
        name: String,
        value: lexer::Literal,
    },
    ConstIdent {
        name: String,
        value: String,
    },
    Let {
        typ: String,
        typ_is_elided: bool,
        name: String,
        assignment: Block,
    },
    If {
        chain: IfChain,
    },
    Call {
        fn_ident: String,
        args: Vec<Argument>,
    },
    MethodCall {
        calee_ident: String,
        fn_ident: String,
        args: Vec<Argument>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub subnodes: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub root: AstNode,
}

pub fn build_ast(_tokens: ScopedTokens) -> Result<Ast, ParseError> {
    todo!()
}
