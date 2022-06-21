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
    Op(Op),
    BoolLiteral(bool),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mul,
    Rem,
    Eq,
    Gtn,
    Ltn,
    GtnEq,
    LtnEq,
    /// logical and
    And,
    /// logical or
    Or,
    // Not,
}

pub type BaseToken = crate::lexer2::BaseToken<Token>;
pub type BaseTokenVal = crate::lexer2::BaseTokenVal<Token>;
