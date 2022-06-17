use regex::Regex;

use crate::{
    lexer2::Rule,
    parse2::{MathOp, Token},
};

macro_rules! rule {
    ($tk:expr, $val:expr, $len:expr) => {
        Rule {
            token: Some($tk),
            validator: Regex::new($val).unwrap(),
            length: $len,
        }
    };
}

pub fn rules() -> Vec<Rule<Token>> {
    [
        rule!(Token::SmallArrow, r"^(->)$", 2),
        rule!(Token::NullT, r"^(null)$", 4),
        rule!(Token::NeverT, r"^(!)$", 1),
        rule!(Token::ParenOpen, r"^\($", 1),
        rule!(Token::ParenClose, r"^\)$", 1),
        rule!(Token::ScopeOpen, r"^\{$", 1),
        rule!(Token::ScopeClose, r"^\}$", 1),
        rule!(Token::Fn, r"^fn$", 2),
        rule!(Token::Let, r"^let$", 3),
        rule!(Token::Const, r"^const$", 5),
        rule!(Token::ConstIdent, r"^ident$", 5),
        rule!(Token::OpAssign, r"^=$", 1),
        rule!(Token::Math(MathOp::Add), r"^\+$", 1),
        rule!(Token::Math(MathOp::Sub), r"^-$", 1),
        rule!(Token::Math(MathOp::Mul), r"^\*$", 1),
        rule!(Token::Math(MathOp::Div), r"^/$", 1),
        rule!(Token::Math(MathOp::Rem), r"^%$", 1),
        rule!(Token::TypeHint, r"^:$", 1),
        rule!(Token::Comma, r"^,$", 1),
        rule!(Token::Return, r"^(return)$", 6),
        rule!(Token::Break, r"^(break)$", 5),
        rule!(Token::Continue, r"^(continue)$", 8),
        rule!(Token::Loop, r"^(loop)$", 4),
        rule!(Token::While, r"^(while)$", 5),
        rule!(Token::If, r"^(if)$", 2),
        rule!(Token::Else, r"^(else)$", 4),
        rule!(Token::BoolLiteral(true), r"^(true)$", 4),
        rule!(Token::BoolLiteral(false), r"^(false)$", 5),
    ]
    .into()
}
