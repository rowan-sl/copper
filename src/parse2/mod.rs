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
    Const,
    ConstIdent,
    Loop,
    While,
    If,
    Else,
    Break,
    Continue,
    Return,
    TypeHint,
    SmallArrow,
    Comma,
    OpAssign,
    NullT,  // `null`
    NeverT, // `!`
    Math(MathOp),
    BoolLiteral(bool),
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
    Null,
}

impl ASTType {
    pub fn try_from_token(tk: BaseTokenVal) -> Option<Self> {
        match tk {
            BaseTokenVal::Ident(name) => Some(Self::Named(name)),
            BaseTokenVal::Token(Token::NeverT) => Some(Self::Never),
            BaseTokenVal::Token(Token::NullT) => Some(Self::Null),
            _ => None,
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
                (Tokens::Token(BaseToken { val: typ, .. }), State::NeedsType { ident })
                    if ASTType::try_from_token(typ.clone()).is_some() =>
                {
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

#[derive(Debug, Clone, PartialEq)]
pub struct CallArguments {
    args: AstNode,
}

impl CallArguments {
    pub fn parse(tokens: &Vec<Tokens>) -> Result<Vec<CallArguments>, BuildAstError> {
        // info!("parsing call args from {tokens:?}");
        let mut current_arg_tokens = vec![];
        let mut args = vec![];

        for token in tokens {
            match token {
                Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Comma),
                    loc,
                }) => {
                    if current_arg_tokens.is_empty() {
                        //TODO make this into an actuall error
                        warn!("unneded separator in function args at {loc:?}");
                    } else {
                        args.push(CallArguments {
                            args: AstNode::parse(Tokens::Group(current_arg_tokens))?,
                        });
                        current_arg_tokens = vec![];
                    }
                }
                other => {
                    current_arg_tokens.push(other.clone());
                }
            }
        }
        if !current_arg_tokens.is_empty() {
            args.push(CallArguments {
                args: AstNode::parse(Tokens::Group(current_arg_tokens))?,
            });
        }
        Ok(args)
    }
}

pub type AstBlock = Vec<AstNode>;
pub type AstGroup = Vec<AstNode>;

#[derive(Debug, Clone, PartialEq)]
pub enum AstNode {
    Block {
        inner: AstBlock,
    },
    Group {
        inner: AstGroup,
    },
    FunctionDef {
        name: String,
        args: Vec<FunctionArguments>,
        ret_typ: ASTType,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    FunctionCall {
        name: String,
        args: Vec<CallArguments>,
    },
    Return {
        /// None is the same as typing `return;`, Some is `return <some thing here>`
        value: Option<Box<AstNode>>,
    },
    Break,
    Continue,
    Let {
        ident: String,
        typ: ASTType,
        value: Box<AstNode>,
    },
    Const {
        ident: String,
        typ: ASTType,
        value: Box<AstNode>,
    },
    ConstIdent {
        ident: String,
        value: String,
    },
    Math {
        left: Box<AstNode>,
        oper: MathOp,
        right: Box<AstNode>,
    },
    Loop {
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    While {
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    If {
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    ElseIf {
        condition: Box<AstNode>,
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    Else {
        /// will always be Some by the time it escapes the build_ast function
        code: Option<AstBlock>,
    },
    NumLiteral(f64),
    StrLiteral(String),
    BoolLiteral(bool),
    Ident(String),
}

impl AstNode {
    fn needs_trailing_block(&self) -> bool {
        if let AstNode::FunctionDef { .. }
        | AstNode::Loop { .. }
        | AstNode::While { .. }
        | AstNode::If { .. }
        | AstNode::Else { .. }
        | AstNode::ElseIf { .. } = self
        {
            true
        } else {
            false
        }
    }

    fn accept_trailing_block(&mut self, block: AstBlock) {
        match self {
            AstNode::FunctionDef { code, .. } => {
                *code = Some(block);
            }
            AstNode::Loop { code } => {
                *code = Some(block);
            }
            AstNode::While { code, .. } => {
                *code = Some(block);
            }
            AstNode::If { code, .. } => {
                *code = Some(block);
            }
            AstNode::Else { code } => {
                *code = Some(block);
            }
            AstNode::ElseIf { code, .. } => {
                *code = Some(block);
            }
            _ => unreachable!(
                "called accept_trailing_block on a node that does not accept trailing blocks!"
            ),
        }
    }

    pub fn parse(tk: Tokens) -> Result<AstNode, BuildAstError> {
        //TODO panics -> errors
        match tk {
            Tokens::Block(inner) => {
                let mut block = AstBlock::new();

                let mut needing_block = None;

                for tokens in inner {
                    match tokens {
                        expr @ Tokens::Expr(..) => {
                            let node = AstNode::parse(expr)?;
                            if node.needs_trailing_block() {
                                needing_block = Some(node);
                            } else {
                                block.push(node);
                            }
                        }
                        blk @ Tokens::Block(..) => {
                            if let Some(mut node) = needing_block {
                                let inner = if let AstNode::Block {
                                    inner: parsed_blk_inner,
                                } = AstNode::parse(blk)?
                                {
                                    parsed_blk_inner
                                } else {
                                    unreachable!()
                                };
                                node.accept_trailing_block(inner);
                                block.push(node);
                                needing_block = None;
                            }
                        }
                        _ => {
                            unreachable!("block cannot congain a group or token")
                        }
                    }
                }
                Ok(AstNode::Block { inner: block })
            }
            Tokens::Group(mut inner) => {
                Tokens::simplify_inner(&mut inner);
                match inner.len() {
                    0 => panic!("empty group expr!"),
                    1 => match inner.pop().unwrap() {
                        Tokens::Block(..) => panic!("Blocks may not exist within a group"),
                        gr @ Tokens::Group(..) => AstNode::parse(gr),
                        Tokens::Expr(..) => unreachable!("probably, please tell me if the happens"),
                        Tokens::Token(token) => match token.val {
                            BaseTokenVal::NumLiteral(num) => Ok(AstNode::NumLiteral(num)),
                            BaseTokenVal::StrLiteral(s) => Ok(AstNode::StrLiteral(s)),
                            BaseTokenVal::Token(Token::BoolLiteral(b)) => {
                                Ok(AstNode::BoolLiteral(b))
                            }
                            BaseTokenVal::Ident(id) => Ok(AstNode::Ident(id)),
                            BaseTokenVal::Token(tk) => {
                                panic!("unparsed single token in group: {tk:?}")
                            }
                            BaseTokenVal::Semi => unreachable!(),
                        },
                    },
                    _ => match &inner[..] {
                        //* math things
                        [left, Tokens::Token(BaseToken {
                            val: BaseTokenVal::Token(Token::Math(op)),
                            ..
                        }), right] => {
                            let left = Box::new(AstNode::parse(Tokens::Group(vec![left.clone()]))?);
                            let right =
                                Box::new(AstNode::parse(Tokens::Group(vec![right.clone()]))?);
                            Ok(AstNode::Math {
                                left,
                                oper: op.clone(),
                                right,
                            })
                        }
                        //* function calls (duplicate exists in the Tokens::Expr branch, EDIT BOLTH IF ONE CHANGES)
                        [Tokens::Token(BaseToken {
                            val: BaseTokenVal::Ident(name),
                            ..
                        }), Tokens::Group(raw_args)] => {
                            let args = CallArguments::parse(raw_args)?;
                            Ok(AstNode::FunctionCall {
                                name: name.clone(),
                                args,
                            })
                        }
                        _ => todo!("unhandled group expression {inner:?}"),
                    },
                }
            }
            Tokens::Expr(inner) => match &inner[..] {
                //* function definitions
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Fn),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(name),
                    ..
                }), Tokens::Group(raw_args), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::SmallArrow),
                    ..
                }), Tokens::Token(BaseToken { val: ret_type, .. })]
                    if {
                        FunctionArguments::parse(raw_args).is_some()
                            && ASTType::try_from_token(ret_type.clone()).is_some()
                    } =>
                {
                    Ok(AstNode::FunctionDef {
                        name: name.clone(),
                        args: FunctionArguments::parse(raw_args).unwrap(),
                        ret_typ: ASTType::try_from_token(ret_type.clone()).unwrap(),
                        code: None,
                    })
                }
                //* function calls (duplicate exists in the Tokens::Group branch, EDIT BOLTH IF ONE CHANGES)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(name),
                    ..
                }), Tokens::Group(raw_args)] => {
                    let args = CallArguments::parse(raw_args)?;
                    Ok(AstNode::FunctionCall {
                        name: name.clone(),
                        args,
                    })
                }
                //* return statement (no value)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Return),
                    ..
                })] => Ok(AstNode::Return { value: None }),
                //* break statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Break),
                    ..
                })] => Ok(AstNode::Break),
                //* continue statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Continue),
                    ..
                })] => Ok(AstNode::Continue),
                //* return statement (with value)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Return),
                    ..
                }), rest @ ..] => Ok(AstNode::Return {
                    value: Some(Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?)),
                }),
                //* let statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Let),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::TypeHint),
                    ..
                }), Tokens::Token(BaseToken { val: raw_typ, .. }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), rest @ ..]
                    if ASTType::try_from_token(raw_typ.clone()).is_some() =>
                {
                    Ok(AstNode::Let {
                        ident: ident.clone(),
                        typ: ASTType::try_from_token(raw_typ.clone()).unwrap(),
                        value: Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?),
                    })
                }
                //* let statement (with no type hinting, for easier error correcting)
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Let),
                    loc,
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(..),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), ..] => {
                    panic!("Error @ {loc:?} Let statements must have types");
                }
                //* const statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Const),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::TypeHint),
                    ..
                }), Tokens::Token(BaseToken { val: raw_typ, .. }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), rest @ ..]
                    if ASTType::try_from_token(raw_typ.clone()).is_some() =>
                {
                    Ok(AstNode::Const {
                        ident: ident.clone(),
                        typ: ASTType::try_from_token(raw_typ.clone()).unwrap(),
                        value: Box::new(AstNode::parse(Tokens::Group(rest.to_vec()))?),
                    })
                }
                //* const ident statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Const),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::ConstIdent),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Ident(ident),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::OpAssign),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::StrLiteral(value),
                    ..
                })] => {
                    Ok(AstNode::ConstIdent {
                        ident: ident.clone(),
                        value: value.clone(),
                    })
                }
                //* loop statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Loop),
                    ..
                })] => Ok(AstNode::Loop { code: None }),
                //* while statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::While),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::While {
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* if statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::If),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::If {
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* else statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Else),
                    ..
                })] => Ok(AstNode::Else { code: None }),
                //* else if statement
                [Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::Else),
                    ..
                }), Tokens::Token(BaseToken {
                    val: BaseTokenVal::Token(Token::If),
                    ..
                }), condition_tks @ ..] => Ok(AstNode::ElseIf {
                    condition: Box::new(AstNode::parse(Tokens::Group(condition_tks.to_vec()))?),
                    code: None,
                }),
                //* catch all
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
