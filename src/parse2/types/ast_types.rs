pub use crate::parse2::types::lexer_tokens::{BaseToken, BaseTokenVal, Token};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
