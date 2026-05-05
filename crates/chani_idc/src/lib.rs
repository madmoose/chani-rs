pub mod ast;
pub mod database;
pub mod eval;
pub mod lexer;
pub mod parser;

pub use database::*;

use anyhow::Result;

/// Parse an IDC file (already decoded to UTF-8) and return the extracted database.
pub fn parse_idc(content: &str) -> Result<IdcDatabase> {
    let tokens = lexer::tokenize(content)?;
    let mut p = parser::Parser::new(tokens);
    let file = p.parse_file()?;
    let mut evaluator = eval::Evaluator::new(file);
    evaluator.run()?;
    Ok(evaluator.db)
}
