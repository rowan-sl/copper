pub mod lex_rules;
pub mod scoped;

use self::scoped::Tokens;
pub use lex_rules::rules;

pub type BaseToken = crate::lexer2::BaseToken<Token>;
pub type BaseTokenVal = crate::lexer2::BaseTokenVal<Token>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Token {
    ParenOpen,
    ParenClose,
    ScopeOpen,
    ScopeClose,
    Fn,
    Let,
    Return,
    TypeHint,
    SmallArrow,
    Comma,
    OpAssign,
    NullT,// `null`
    NeverT,// `!`
    Math(MathOp),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathOp {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ASTType {
    Named(String),
    Never,
    Null
}

impl ASTType {
    pub fn try_from_token(tk: BaseTokenVal) -> Option<Self> {
        match tk {
            BaseTokenVal::Ident(name) => Some(Self::Named(name)),
            BaseTokenVal::Token(Token::NeverT) => Some(Self::Never),
            BaseTokenVal::Token(Token::NullT) => Some(Self::Null),
            _ => None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArguments {
    ident: String,
    typ: ASTType,
}

impl FunctionArguments {
    pub fn parse(tokens: &Vec<Tokens>) -> Option<Vec<FunctionArguments>> {
        enum State {
            NeedsIdent,
            NeedsTypeHint { ident: String },
            NeedsType { ident: String },
            NeedsComma { ident: String, typ: ASTType },
        }
        let mut state = State::NeedsIdent;
        let mut args = vec![];
        for token in tokens {
            match (token, state) {
                (
                    Tokens::Token(BaseToken {
                        val: BaseTokenVal::Ident(ident),
                        ..
                    }),
                    State::NeedsIdent,
                ) => {
                    state = State::NeedsTypeHint {
                        ident: ident.clone(),
                    };
                }
                (
                    Tokens::Token(BaseToken {
                        val: BaseTokenVal::Token(Token::TypeHint),
                        ..
                    }),
                    State::NeedsTypeHint { ident },
                ) => {
                    state = State::NeedsType { ident };
                }
                (
                    Tokens::Token(BaseToken {
                        val: typ,
                        ..
                    }),
                    State::NeedsType { ident },
                ) if ASTType::try_from_token(typ.clone()).is_some() => {
                    state = State::NeedsComma {
                        ident,
                        typ: ASTType::try_from_token(typ.clone()).unwrap(),
                    };
                }
                (
                    Tokens::Token(BaseToken {
                        val: BaseTokenVal::Token(Token::Comma),
                        ..
                    }),
                    State::NeedsComma { ident, typ },
                ) => {
                    args.push(FunctionArguments { ident, typ });
                    state = State::NeedsIdent;
                }
                _ => return None,
            }
        }
        Some(args)
    }
}

pub type ASTBlock = Vec<AstNode>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block {
        inner: ASTBlock,
    },
    FunctionDef {
        name: String,
        args: Vec<FunctionArguments>,
        ret_typ: ASTType,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<ASTBlock>,
    },
    Return {
        /// None is the same as typing `return;`, Some is `return <some thing here>`
        value: Option<Box<AstNode>>,
    },
    NumLiteral(f64),
    StrLiteral(String),
    Ident(String),
}

impl AstNode {
    pub fn parse(tk: Tokens) -> Result<AstNode, BuildAstError> {
        //TODO panics -> errors
        match tk {
            Tokens::Block(inner) => {
                let mut block = ASTBlock::new();

                let mut needing_block = None;

                for tokens in inner {
                    match tokens {
                        expr @ Tokens::Expr(..) => {
                            let node = AstNode::parse(expr)?;
                            if let AstNode::FunctionDef { .. } = &node {
                                needing_block = Some(node);
                            } else {
                                block.push(node);
                            }
                        }
                        blk @ Tokens::Block(..) => {
                            if let &mut Some(AstNode::FunctionDef { ref mut code, .. }) = &mut needing_block {
                                if let AstNode::Block { inner: parsed_blk_inner } = AstNode::parse(blk)? {
                                    *code = Some(parsed_blk_inner);
                                } else {
                                    unreachable!()
                                }
                            } else {
                                panic!("free floating block expr?");
                            }
                            if let Some(node) = needing_block {
                                block.push(node);
                                needing_block = None;
                            }
                        }
                        _ => {unreachable!("block cannot congain a group or token")}
                    }
                }
                Ok(AstNode::Block { inner: block })
            }
            Tokens::Group(mut inner) => {
                Tokens::simplify_inner(&mut inner);
                match inner.len() {
                    0 => panic!("empty group expr!"),
                    1 => {
                        match inner.pop().unwrap() {
                            Tokens::Block(..) => panic!("Blocks cannot exist within a group"),
                            gr @ Tokens::Group(..) => AstNode::parse(gr),
                            Tokens::Expr(..) => unreachable!("probably, please tell me if the happens"),
                            Tokens::Token(token) => {
                                match token.val {
                                    BaseTokenVal::NumLiteral(num) => Ok(AstNode::NumLiteral(num)),
                                    BaseTokenVal::StrLiteral(s) => Ok(AstNode::StrLiteral(s)),
                                    BaseTokenVal::Ident(id) => Ok(AstNode::Ident(id)),
                                    BaseTokenVal::Token(tk) => panic!("unparsed single token in group: {tk:?}"),
                                    BaseTokenVal::Semi => unreachable!()
                                }
                            }
                        }
                    },
                    _ => {
                        todo!("group parsing is unfinished")
                    }
                }
            }
            Tokens::Expr(inner) => match &inner[..] {
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Fn),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(name),
                    ..
                }), Tokens::Group(raw_args), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::SmallArrow),
                    ..
                }), Tokens::Token(BaseToken {
                    val: ret_type,
                    ..
                })] if { FunctionArguments::parse(raw_args).is_some() && ASTType::try_from_token(ret_type.clone()).is_some() } => {
                    Ok(AstNode::FunctionDef {
                        name: name.clone(),
                        args: FunctionArguments::parse(raw_args).unwrap(),
                        ret_typ: ASTType::try_from_token(ret_type.clone()).unwrap(),
                        code: None,
                    })
                }
                [
                    Tokens::Token(BaseToken {
                        val: BaseTokenVal::Token(Token::Return),
                        ..
                    })
                ] => {
                    Ok(AstNode::Return { value: None })
                }
                [
                    Tokens::Token(BaseToken {
                        val: BaseTokenVal::Token(Token::Return),
                        ..
                    }),
                    rest @ ..,
                ] => {
                    Ok(AstNode::Return { value: Some(Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?)) })
                }
                unmatched => {
                    todo!("unmatched expr:\n{unmatched:#?}")
                }
            },
            Tokens::Token(..) => {
                unreachable!("AstNode::parse may not be called with a value of Tokens::Token")
            }
        }
    }
}

pub fn build_ast(tk: Tokens) -> Result<AstNode, BuildAstError> {
    AstNode::parse(tk)
}

#[derive(Debug, Clone, thiserror::Error)]
pub enum BuildAstError {}

// /// self contains [at, len), returned contains [0, at)
// #[inline(always)]
// #[must_use]
// fn take_n<T>(input: &mut Vec<T>, n: usize) -> Option<Vec<T>> {
//     if n >= input.len() {
//         return None;
//     }
//     let mut new_input = input.split_off(n);
//     std::mem::swap(&mut new_input, input);
//     Some(new_input)
// }

// #[inline(always)]
// #[must_use]
// fn borrow_n<T>(input: &[T], n: usize) -> Option<&[T]> {
//     if n >= input.len() {
//         return None;
//     }
//     Some(&input[..n])
// }
