use super::Token;

pub type BaseToken = crate::lexer2::BaseToken<Token>;
pub type BaseTokenVal = crate::lexer2::BaseTokenVal<Token>;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ScoperError {
    #[error("An Expr cannot contain a Block")]
    ExprContainingBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Tokens {
    Token(BaseToken),
    Group(Vec<Tokens>),
    Expr(Vec<Tokens>),
    Block(Vec<Tokens>),
}

impl Tokens {
    fn traverse_build_group(raw: &mut Vec<BaseToken>) -> Vec<Self> {
        // debug!("recursivly parsing token group {raw:#?}");
        let mut current = vec![];
        loop {
            let next = raw.pop(); // input array MUST be reversed
                                  // debug!("parsing: {next:?}");
            match next {
                Some(BaseToken {
                    val: BaseTokenVal::Token(Token::ParenOpen),
                    ..
                }) => {
                    // debug!("parsing inner scope");
                    let inner = Tokens::traverse_build_group(raw);
                    current.push(Tokens::Group(inner));
                    // debug!("continuing in outer scope");
                }
                Some(BaseToken {
                    val: BaseTokenVal::Token(Token::ParenClose),
                    ..
                })
                | None => {
                    // debug!("exiting inner scope");
                    break;
                }
                Some(other) => {
                    // debug!("token: {:?}", other);
                    current.push(Tokens::Token(other))
                }
            }
        }
        current
    }

    pub fn build_group(mut raw: Vec<BaseToken>) -> Vec<Self> {
        // debug!("Parsing from {raw:#?}");
        raw.reverse(); //so .pop() can be used to more efficiently remove items
        Tokens::traverse_build_group(&mut raw)
    }

    pub fn traverse_build(
        &mut self,
        token_stream: &mut impl Iterator<Item = BaseToken>,
    ) -> Result<(), ScoperError> {
        let mut current_line_tokens: Vec<BaseToken> = vec![];
        while let Some(token) = token_stream.next() {
            match token {
                BaseToken {
                    val: BaseTokenVal::Token(Token::ScopeOpen),
                    ..
                } => match self {
                    Self::Block(ref mut inner) => {
                        if !current_line_tokens.is_empty() {
                            inner.push(Tokens::Expr(Tokens::build_group(current_line_tokens)));
                            current_line_tokens = vec![];
                        }
                        inner.push(Tokens::Block(vec![]));
                        inner.last_mut().unwrap().traverse_build(token_stream)?;
                    }
                    Self::Expr { .. } => return Err(ScoperError::ExprContainingBlock),
                    _ => unreachable!(
                        "Cannot call traverse_build on a value of Tokens::Token or Tokens::Group"
                    ),
                },
                BaseToken {
                    val: BaseTokenVal::Token(Token::ScopeClose),
                    ..
                } => match self {
                    Self::Block(ref mut inner) => {
                        if !current_line_tokens.is_empty() {
                            inner.push(Tokens::Expr(Tokens::build_group(current_line_tokens)));
                        }
                        return Ok(());
                    }
                    Self::Expr { .. } => return Err(ScoperError::ExprContainingBlock),
                    _ => unreachable!(
                        "Cannot call traverse_build on a value of Tokens::Token or Tokens::Group"
                    ),
                },
                BaseToken {
                    val: BaseTokenVal::Semi,
                    ..
                } => match self {
                    Self::Block(ref mut inner) => {
                        inner.push(Tokens::Expr(Tokens::build_group(current_line_tokens)));
                        current_line_tokens = vec![];
                    }
                    Self::Expr { .. } => unreachable!(),
                    _ => unreachable!(
                        "Cannot call traverse_build on a value of Tokens::Token or Tokens::Group"
                    ),
                },
                other => {
                    current_line_tokens.push(other);
                }
            }
        }
        Ok(())
    }

    /// simplify inner tokens of a group
    pub fn simplify_inner(inner: &mut Vec<Tokens>) {
        loop {
            let mut rerun = false;
            if inner.len() == 1 {
                if let Tokens::Group(new_inner) = inner.last().unwrap() {
                    *inner = new_inner.clone();
                    rerun = true;
                }
            }
            if !rerun {
                break;
            }
        }
    }

    pub fn simplify(&mut self) {
        match self {
            Tokens::Group(group) => {
                Tokens::simplify_inner(group);
            }
            _ => {}
        }
    }
}

pub fn scope_out(tokens: Vec<BaseToken>) -> Result<Tokens, ScoperError> {
    // trace!("Scoping tokens");
    let mut root = Tokens::Block(vec![]);
    let mut tokens_iter = tokens.into_iter();
    root.traverse_build(&mut tokens_iter)?;
    Ok(root)
}
