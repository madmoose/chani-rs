use std::fmt;

use crate::ast::*;
use crate::lexer::Token;

#[derive(Debug)]
pub struct ParseError {
    pub line: usize,
    pub msg: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "parse error at line {}: {}", self.line, self.msg)
    }
}

impl std::error::Error for ParseError {}

pub struct Parser {
    tokens: Vec<(Token, usize)>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<(Token, usize)>) -> Self {
        Parser { tokens, pos: 0 }
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos).map(|(t, _)| t)
    }

    fn peek_line(&self) -> usize {
        self.tokens.get(self.pos).map(|(_, l)| *l).unwrap_or(0)
    }

    fn advance(&mut self) -> Option<Token> {
        if self.pos < self.tokens.len() {
            let tok = self.tokens[self.pos].0.clone();
            self.pos += 1;
            Some(tok)
        } else {
            None
        }
    }

    fn expect_ident(&mut self) -> Result<Ident, ParseError> {
        let line = self.peek_line();
        match self.advance() {
            Some(Token::Ident(s)) => Ok(s),
            got => Err(ParseError {
                line,
                msg: format!("expected identifier, got {got:?}"),
            }),
        }
    }

    fn expect(&mut self, expected: Token) -> Result<(), ParseError> {
        let line = self.peek_line();
        match self.advance() {
            Some(tok) if tok == expected => Ok(()),
            got => Err(ParseError {
                line,
                msg: format!("expected {expected:?}, got {got:?}"),
            }),
        }
    }

    pub fn parse_file(&mut self) -> Result<File, ParseError> {
        let mut items = Vec::new();
        while self.peek().is_some() {
            items.push(self.parse_item()?);
        }
        Ok(File { items })
    }

    fn parse_item(&mut self) -> Result<Item, ParseError> {
        let line = self.peek_line();
        match self.peek() {
            Some(Token::Directive(_)) => match self.advance() {
                Some(Token::Directive(s)) => Ok(Item::Directive(parse_directive_str(&s, line)?)),
                _ => unreachable!(),
            },
            Some(Token::Ident(s)) if s == "static" => {
                self.advance();
                Ok(Item::FunctionDef(self.parse_function_def()?))
            }
            _ => Err(ParseError {
                line,
                msg: format!(
                    "expected '#' directive or 'static' function definition, got {:?}",
                    self.peek()
                ),
            }),
        }
    }

    fn parse_function_def(&mut self) -> Result<FunctionDef, ParseError> {
        let name = self.expect_ident()?;
        self.expect(Token::LParen)?;
        let params = self.parse_params()?;
        self.expect(Token::RParen)?;
        self.expect(Token::LBrace)?;
        let body = self.parse_block()?;
        self.expect(Token::RBrace)?;
        Ok(FunctionDef { name, params, body })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        // void  → empty list
        // ident → single untyped param
        // type ident (, type ident)* → typed params
        if matches!(self.peek(), Some(Token::RParen)) {
            return Ok(vec![]);
        }
        if matches!(self.peek(), Some(Token::Ident(s)) if s == "void") {
            self.advance();
            return Ok(vec![]);
        }
        let mut params = Vec::new();
        loop {
            let first = self.expect_ident()?;
            // If followed by another ident, `first` was the type
            let name = if matches!(self.peek(), Some(Token::Ident(_))) {
                self.expect_ident()?
            } else {
                first
            };
            params.push(Param { name });
            if !matches!(self.peek(), Some(Token::Comma)) {
                break;
            }
            self.advance(); // consume ','
        }
        Ok(params)
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !matches!(self.peek(), Some(Token::RBrace) | None) {
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let line = self.peek_line();
        match self.peek() {
            Some(Token::Semi) => {
                self.advance();
                Ok(Stmt::Empty)
            }
            Some(Token::Directive(_)) => match self.advance() {
                Some(Token::Directive(s)) => Ok(Stmt::Directive(parse_directive_str(&s, line)?)),
                _ => unreachable!(),
            },
            Some(Token::Ident(s)) if s == "auto" => {
                self.advance();
                self.parse_auto_decl()
            }
            Some(Token::Ident(s)) if s == "return" => {
                self.advance();
                if matches!(self.peek(), Some(Token::Semi)) {
                    self.advance();
                    Ok(Stmt::Return(None))
                } else {
                    let e = self.parse_expr()?;
                    self.expect(Token::Semi)?;
                    Ok(Stmt::Return(Some(e)))
                }
            }
            _ => {
                let e = self.parse_expr()?;
                self.expect(Token::Semi)?;
                Ok(Stmt::Expr(e))
            }
        }
    }

    fn parse_auto_decl(&mut self) -> Result<Stmt, ParseError> {
        let mut names = vec![self.expect_ident()?];
        loop {
            match self.peek() {
                Some(Token::Comma) => {
                    self.advance();
                    names.push(self.expect_ident()?);
                }
                Some(Token::Semi) => {
                    self.advance();
                    break;
                }
                _ => {
                    return Err(ParseError {
                        line: self.peek_line(),
                        msg: format!(
                            "expected ',' or ';' in auto declaration, got {:?}",
                            self.peek()
                        ),
                    })
                }
            }
        }
        Ok(Stmt::AutoDecl(names))
    }

    /// Parse an expression.
    ///
    /// Assignment `ident = expr` has the lowest precedence and is
    /// distinguished from other uses of an identifier by one token of
    /// lookahead: if the current token is an `Ident` and the next is `=`,
    /// it is an assignment.
    fn parse_expr(&mut self) -> Result<Expr, ParseError> {
        let is_assign = matches!(self.peek(), Some(Token::Ident(_)))
            && matches!(self.tokens.get(self.pos + 1), Some((Token::Eq, _)));

        if is_assign {
            let name = match self.advance() {
                Some(Token::Ident(s)) => s,
                _ => unreachable!(),
            };
            self.advance(); // consume '='
            let value = self.parse_expr()?; // right-associative
            return Ok(Expr::Assign(name, Box::new(value)));
        }
        self.parse_postfix()
    }

    /// Parse a postfix expression: a primary optionally followed by a call.
    fn parse_postfix(&mut self) -> Result<Expr, ParseError> {
        let base = self.parse_primary()?;
        // Only identifiers can be called in IDC
        if let Expr::Ident(ref name) = base
            && matches!(self.peek(), Some(Token::LParen)) {
                let name = name.clone();
                self.advance(); // consume '('
                let args = self.parse_arg_list()?;
                self.expect(Token::RParen)?;
                return Ok(Expr::Call(name, args));
            }
        Ok(base)
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let line = self.peek_line();
        match self.advance() {
            Some(Token::Ident(s)) => Ok(Expr::Ident(s)),
            Some(Token::Int(n)) => Ok(Expr::Int(n)),
            Some(Token::Str(s)) => Ok(Expr::Str(s)),
            Some(Token::Minus) => {
                let e = self.parse_primary()?;
                Ok(Expr::Neg(Box::new(e)))
            }
            Some(Token::LParen) => {
                let e = self.parse_expr()?;
                self.expect(Token::RParen)?;
                Ok(e)
            }
            got => Err(ParseError {
                line,
                msg: format!("expected expression, got {got:?}"),
            }),
        }
    }

    fn parse_arg_list(&mut self) -> Result<Vec<Expr>, ParseError> {
        if matches!(self.peek(), Some(Token::RParen)) {
            return Ok(vec![]);
        }
        let mut args = vec![self.parse_expr()?];
        while matches!(self.peek(), Some(Token::Comma)) {
            self.advance();
            args.push(self.parse_expr()?);
        }
        Ok(args)
    }
}

fn parse_directive_str(s: &str, _line: usize) -> Result<Directive, ParseError> {
    if let Some(rest) = s.strip_prefix("define") {
        let rest = rest.trim();
        // Split name from optional value at first whitespace
        if let Some(idx) = rest.find(|c: char| c.is_whitespace()) {
            let name = rest[..idx].to_string();
            let value = Some(rest[idx..].trim().to_string());
            Ok(Directive::Define { name, value })
        } else {
            Ok(Directive::Define {
                name: rest.to_string(),
                value: None,
            })
        }
    } else if let Some(rest) = s.strip_prefix("include") {
        Ok(Directive::Include {
            path: rest.trim().to_string(),
        })
    } else {
        // Unknown directive (e.g. #pragma): treat as no-op
        Ok(Directive::Define {
            name: String::new(),
            value: None,
        })
    }
}
