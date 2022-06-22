use crate::parse2::{
    types::{
        ast_types::ASTType,
        lexer_tokens::{BaseToken, BaseTokenVal, Token},
    },
    AstNode, BuildAstError, Tokens,
};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionArguments {
    pub ident: String,
    pub typ: ASTType,
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
        if let State::NeedsComma { ident, typ } = state {
            args.push(FunctionArguments { ident, typ });
        }
        Some(args)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallArguments {
    pub args: AstNode,
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
