use crate::parsing::lexer::{self, Token};

#[derive(Clone, Debug, thiserror::Error)]
pub enum ScoperError {
    #[error("An Expr cannot contain a Block")]
    ExprContainingBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub enum GroupedTokens {
    Token(lexer::Token),
    Group(Vec<GroupedTokens>),
}

impl GroupedTokens {
    fn traverse_parse(raw: &mut Vec<lexer::Token>) -> Self {
        // debug!("recursivly parsing token group {raw:#?}");
        let mut current = vec![];
        loop {
            let next = raw.pop();// input array MUST be reversed
            // debug!("parsing: {next:?}");
            match next {
                Some(Token::ParenBegin) => {
                    // debug!("parsing inner scope");
                    let inner = GroupedTokens::traverse_parse(raw);
                    current.push(inner);
                    // debug!("continuing in outer scope");
                }
                Some(Token::ParenEnd) | None => {
                    // debug!("exiting inner scope");
                    break
                }
                Some(other) => {
                    // debug!("token: {:?}", other);
                    current.push(GroupedTokens::Token(other))
                }
            }
        }
        GroupedTokens::Group(current)
    }

    pub fn parse(mut raw: Vec<lexer::Token>) -> Self {
        // debug!("Parsing from {raw:#?}");
        raw.reverse();//so .pop() can be used to more efficiently remove items
        GroupedTokens::traverse_parse(&mut raw)
    }
}

/// note: does not contiain tokens `Token::EOL`, Token::ScopeBegin`, or `Token::ScopeEnd` (replaced with this)
///
/// other note: a Expr cannot contain a Block
#[derive(Debug, Clone, PartialEq)]
pub enum ScopedTokens {
    // an expression, terminated by `;`
    Expr { tokens: GroupedTokens },
    // a block, contained in `{` and `}`
    Block { inner: Vec<ScopedTokens> },
}

impl ScopedTokens {
    pub fn traverse_build(
        &mut self,
        token_stream: &mut impl Iterator<Item = lexer::Token>,
    ) -> Result<(), ScoperError> {
        let mut current_line_tokens: Vec<lexer::Token> = vec![];
        while let Some(token) = token_stream.next() {
            match token {
                Token::ScopeBegin => match self {
                    Self::Block { ref mut inner } => {
                        if !current_line_tokens.is_empty() {
                            inner.push(ScopedTokens::Expr {
                                tokens: GroupedTokens::parse(current_line_tokens),
                            });
                            current_line_tokens = vec![];
                        }
                        inner.push(ScopedTokens::Block { inner: vec![] });
                        inner.last_mut().unwrap().traverse_build(token_stream)?;
                    }
                    Self::Expr { .. } => return Err(ScoperError::ExprContainingBlock),
                },
                Token::ScopeEnd => match self {
                    Self::Block { ref mut inner } => {
                        if !current_line_tokens.is_empty() {
                            inner.push(ScopedTokens::Expr {
                                tokens: GroupedTokens::parse(current_line_tokens),
                            });
                        }
                        return Ok(());
                    }
                    Self::Expr { .. } => return Err(ScoperError::ExprContainingBlock),
                },
                Token::EOL => match self {
                    Self::Block { ref mut inner } => {
                        inner.push(ScopedTokens::Expr {
                            tokens: GroupedTokens::parse(current_line_tokens),
                        });
                        current_line_tokens = vec![];
                    }
                    Self::Expr { .. } => unreachable!(),
                },
                other => {
                    current_line_tokens.push(other);
                }
            }
        }
        Ok(())
    }
}

pub fn scope_out(tokens: Vec<lexer::Token>) -> Result<ScopedTokens, ScoperError> {
    trace!("Scoping tokens");
    let mut root = ScopedTokens::Block { inner: vec![] };
    let mut tokens_iter = tokens.into_iter();
    root.traverse_build(&mut tokens_iter)?;
    Ok(root)
}
