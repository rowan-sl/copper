pub mod ast;
pub mod scoped;

use crate::parsing::lexer;

#[derive(Clone, Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Lexing failed: {0:#?}")]
    LexerError(#[from] lexer::LexError),
    #[error("Scoping failed: {0:#?}")]
    ScoperError(#[from] scoped::ScoperError),
}

pub fn parse(raw: String) -> Result<ast::Ast, ParseError> {
    let tokens = lexer::tokenize(raw)?;
    let scoped_tokens = scoped::scope_out(tokens)?;
    debug!("scoped tokens: {scoped_tokens:#?}");
    let ast = ast::build_ast(scoped_tokens)?;
    debug!("ast: {ast:#?}");
    Ok(ast)
}
