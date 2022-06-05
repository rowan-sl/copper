use super::scoped::ScopedTokens;
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
        name: String,
        assignment: Assignment,
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
pub enum Assignment {
    Literal(lexer::Literal),
    OtherVariable {
        name: String,
    },
    Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub subnodes: Vec<AstNode>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ast {
    pub root: AstNode,
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BuildError {
    #[error("an Expr cannot contain a Block!")] //this will change at some point
    BlockInExpr,
    #[error("an Expr cannot contain a Expr!")]
    ExprInExpr,
}

fn build_ast_block(ast: &mut AstNode, mut block_tokens: Vec<ScopedTokens>) -> Result<(), BuildError> {
    block_tokens.reverse(); // so that we can use .pop() more efficiently
    while let Some(st) = block_tokens.pop() {
        match st {
            ScopedTokens::Block { inner } => {
                let mut block_node = AstNode::Block(Block { subnodes: vec![] });
                build_ast_block(&mut block_node, inner)?;
                match ast {
                    AstNode::Block(ref mut block) => {
                        block.subnodes.push(block_node);
                    }
                    AstNode::Expr(ref mut _expr) => {
                        return Err(BuildError::BlockInExpr);
                    }
                }
            }
            ScopedTokens::Expr { tokens } => {
                use lexer::{Keyword, Operator, OtherGrammar, Token, Literal};
                // all hail slice patterns
                let expr: Option<Expr> = match &tokens[..] {
                    [
                        Token::Keyword(Keyword::Const),
                        Token::Ident(name),
                        Token::OtherGrammar(OtherGrammar::TypeHint),
                        Token::Type(typ),
                        Token::Operator(Operator::Assign),
                        Token::Literal(value)
                    ] => {
                        Some(Expr::Constant { typ: typ.clone(), name: name.clone(), value: value.clone() })
                    }
                    [
                        Token::Keyword(Keyword::ObjectIdent),
                        Token::Ident(name),
                        Token::Operator(Operator::Assign),
                        Token::Literal(Literal::String(value))
                    ] => {
                        Some(Expr::ConstIdent { name: name.clone(), value: value.clone() })
                    }
                    [
                        Token::Keyword(Keyword::Let),
                        Token::Ident(name),
                        Token::OtherGrammar(OtherGrammar::TypeHint),
                        Token::Type(typ),
                        Token::Operator(Operator::Assign),
                        assignment,
                    ] => {
                        match assignment.clone() {
                            Token::Literal(lit) => {
                                Some(Expr::Let { typ: typ.clone(), name: name.clone(), assignment: Assignment::Literal(lit) })
                            }
                            Token::Ident(ident) => {
                                Some(Expr::Let { typ: typ.clone(), name: name.clone(), assignment: Assignment::OtherVariable { name: ident } })
                            }
                            _other => {
                                // assigning to expressions, such as function calls
                                todo!();
                            }
                        }
                    }
                    _ => {
                        error!("no pattern for expression: {:#?}", tokens);
                        None
                    }
                };
                if let Some(expr) = expr {
                    match ast {
                        AstNode::Block(ref mut block) => {
                            block.subnodes.push(AstNode::Expr(expr));
                        }
                        AstNode::Expr(ref mut _expr) => {
                            return Err(BuildError::ExprInExpr);
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

pub fn build_ast(tokens: ScopedTokens) -> Result<Ast, BuildError> {
    let mut ast = Ast {
        root: AstNode::Block(Block { subnodes: vec![] }),
    };
    if let ScopedTokens::Block { inner } = tokens {
        build_ast_block(&mut ast.root, inner)?;
    } else {
        panic!("Token tree pased to build_ast was not a Block expr");
    }

    Ok(ast)
}
