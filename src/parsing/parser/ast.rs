use super::scoped::ScopedTokens;
use crate::parsing::{lexer, parser::scoped::GroupedTokens};

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block(Block),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IfChain {
    // can be assumed that the GroupedTokens here are always Groups
    If { condition: GroupedTokens, folowing: Block },
    ElseIf { condition: GroupedTokens, folowing: Block },
    Else { folowing: Block },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Argument {
    Ident { name: String },
    Group(GroupedTokens),
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
        /// can be assumed to always be a Group
        assignment: GroupedTokens,
    },
    If {
        chain: IfChain,
    },
    Call {
        fn_ident: Vec<String>,
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

#[derive(Debug, Clone, thiserror::Error)]
pub enum BuildError {
    #[error("an Expr cannot contain a Block!")] //this will change at some point
    BlockInExpr,
    #[error("an Expr cannot contain a Expr!")]
    ExprInExpr,
    #[error("An if statement must be folowed by a block!")]
    IfStatementNoBlock
}

fn build_ast_block(ast: &mut AstNode, mut block_tokens: Vec<ScopedTokens>) -> Result<(), BuildError> {
    fn parse_block(inner: Vec<ScopedTokens>) -> Result<Block, BuildError> {
        let mut block_node = AstNode::Block(Block { subnodes: vec![] });
        build_ast_block(&mut block_node, inner)?;
        if let AstNode::Block(block) = block_node {
            Ok(block)
        } else {
            unsafe { std::hint::unreachable_unchecked() }
        }
    }

    block_tokens.reverse(); // so that we can use .pop() more efficiently
    while let Some(st) = block_tokens.pop() {
        match st {
            ScopedTokens::Block { inner } => {
                let block_node = AstNode::Block(parse_block(inner)?);
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
                let tokens = if let GroupedTokens::Group(tokens) = tokens {
                    tokens
                } else {
                    unreachable!()
                };
                // all hail slice patterns
                #[allow(unused_imports)]
                use GroupedTokens::{Token as GTT, Group as GTG};
                let expr: Option<Expr> = match &tokens[..] {
                    [
                        GTT(Token::Keyword(Keyword::Const)),
                        GTT(Token::Ident(name)),
                        GTT(Token::OtherGrammar(OtherGrammar::TypeHint)),
                        GTT(Token::Type(typ)),
                        GTT(Token::Operator(Operator::Assign)),
                        GTT(Token::Literal(value))
                    ] => {
                        Some(Expr::Constant { typ: typ.clone(), name: name.clone(), value: value.clone() })
                    }
                    [
                        GTT(Token::Keyword(Keyword::ObjectIdent)),
                        GTT(Token::Ident(name)),
                        GTT(Token::Operator(Operator::Assign)),
                        GTT(Token::Literal(Literal::String(value)))
                    ] => {
                        Some(Expr::ConstIdent { name: name.clone(), value: value.clone() })
                    }
                    [
                        GTT(Token::Keyword(Keyword::Let)),
                        GTT(Token::Ident(name)),
                        GTT(Token::OtherGrammar(OtherGrammar::TypeHint)),
                        GTT(Token::Type(typ)),
                        GTT(Token::Operator(Operator::Assign)),
                        assignment @ ..,
                    ] => {
                        let group = if assignment.len() == 1 {
                            match assignment[0].clone() {
                                GroupedTokens::Group(group) => {
                                    GroupedTokens::Group(group)
                                }
                                GroupedTokens::Token(token) => {
                                    GroupedTokens::Group(vec![GroupedTokens::Token(token)])
                                }
                            }
                        } else {
                            GroupedTokens::Group(assignment.to_vec())
                        };
                        Some(Expr::Let { typ: typ.clone(), name: name.clone(), assignment: group })
                    }
                    [
                        GTT(Token::Keyword(Keyword::If)),
                        condition @ GTG(..),
                    ] => {
                        let block = match block_tokens.pop() {
                            Some(ScopedTokens::Block { inner }) => parse_block(inner)?,
                            Some(ScopedTokens::Expr { .. }) | None => return Err(BuildError::IfStatementNoBlock),
                        };
                        Some(Expr::If { chain: IfChain::If {
                            condition: condition.clone(),
                            folowing: block,
                        }})
                    }
                    [
                        GTT(Token::Path(path)),
                        args @ GTG(..),
                    ] if {
                        //TODO finish
                        let mut is_ok = true;
                        let mut needs_sep = false;
                        match args {
                            GroupedTokens::Group(tks) => {
                                for tk in tks {
                                    match tk {
                                        GroupedTokens::Token(Token::OtherGrammar(OtherGrammar::Seperator)) => {
                                            if needs_sep {
                                                needs_sep = false;
                                            } else {
                                                warn!("Unneded seperator in function args (this will become an error in the future!)");
                                                is_ok = false;
                                            }
                                        }
                                        _ => {
                                            needs_sep = true;
                                        }
                                    }
                                }
                            }
                            _ => unreachable!()
                        }
                        is_ok
                    } => {
                        let args = match args {
                            GTG(args) => args.clone().into_iter().filter(|x| {
                                match x {
                                    GroupedTokens::Token(Token::OtherGrammar(OtherGrammar::Seperator)) => false,
                                    _ => true,
                                }
                            }),
                            _ => unreachable!()
                        }
                            .map(|arg| {
                                match arg {
                                    GroupedTokens::Token(Token::Ident(ident)) => Argument::Ident { name: ident },
                                    GroupedTokens::Token(Token::Literal(lit)) => Argument::Literal(lit),
                                    other => Argument::Group(other)
                                }
                            })
                            .collect::<Vec<_>>();
                        Some(Expr::Call { fn_ident: path.clone(), args })
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
