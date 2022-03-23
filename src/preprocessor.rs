use crate::ast;

pub fn preprocess(tokens: Vec<ast::Token>, _lex_file: fn(&str) -> std::io::Result<Vec<ast::Token>>) -> std::io::Result<Vec<ast::Token>> {
    Ok(tokens)
}