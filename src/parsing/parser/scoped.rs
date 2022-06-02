use crate::parsing::lexer;

        #[derive(Clone ,Debug, thiserror::Error)]
        pub enum ScoperError {
            #[error("An Expr cannot contain a Block")]
            ExprContainingBlock,
        }

        /// note: does not contiain tokens `Token::EOL`, Token::ScopeBegin`, or `Token::ScopeEnd` (replaced with this)
        ///
        /// other note: a Expr cannot contain a Block
        #[derive(Debug, Clone, PartialEq)]
        pub enum ScopedTokens {
            // an expression, terminated by `;`
            Expr {
                tokens: Vec<lexer::Token>,
            },
            // a block, contained in `{` and `}`
            Block {
                inner: Vec<ScopedTokens>,
            },
        }

        impl ScopedTokens {
            pub fn traverse_build(&mut self, token_stream: &mut impl Iterator<Item = lexer::Token>) -> Result<(), ScoperError>  {
                let mut current_line_tokens: Vec<lexer::Token> = vec![];
                while let Some(token) = token_stream.next() {
                    use lexer::Token;
                    match token {
                        Token::ScopeBegin => match self {
                            Self::Block { ref mut inner } => {
                                if !current_line_tokens.is_empty() {
                                    inner.push(ScopedTokens::Expr { tokens: current_line_tokens });
                                    current_line_tokens = vec![];
                                }
                                inner.push(ScopedTokens::Block { inner: vec![] });
                                inner.last_mut().unwrap().traverse_build(token_stream)?;
                            }
                            Self::Expr { .. } => {
                                return Err(ScoperError::ExprContainingBlock)
                            }
                        },
                        Token::ScopeEnd => match self {
                            Self::Block { ref mut inner } => {
                                if !current_line_tokens.is_empty() {
                                    inner.push(ScopedTokens::Expr { tokens: current_line_tokens });
                                }
                                return Ok(())
                            }
                            Self::Expr { .. } => {
                                return Err(ScoperError::ExprContainingBlock)
                            }
                        },
                        Token::EOL => match self {
                            Self::Block { ref mut inner } => {
                                inner.push(ScopedTokens::Expr { tokens: current_line_tokens });
                                current_line_tokens = vec![];
                            }
                            Self::Expr { .. } => unreachable!()
                        }
                        other => {
                            current_line_tokens.push(other);
                        }
                    }
                }
                Ok(())
            }
        }

        pub fn scope_out(tokens: Vec<lexer::Token>) -> Result<ScopedTokens, ScoperError> {
            let mut root = ScopedTokens::Block { inner: vec![] };
            let mut tokens_iter = tokens.into_iter();
            root.traverse_build(&mut tokens_iter)?;
            Ok(root)
        }
