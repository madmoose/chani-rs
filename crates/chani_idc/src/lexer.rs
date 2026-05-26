use std::fmt;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Ident(String),
    Int(i64),
    Str(String),
    LParen,
    RParen,
    LBrace,
    RBrace,
    Semi,
    Comma,
    Eq,
    Minus,
    Directive(String),
}

#[derive(Debug)]
pub struct LexError {
    pub line: usize,
    pub msg: String,
}

impl fmt::Display for LexError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "lex error at line {}: {}", self.line, self.msg)
    }
}

impl std::error::Error for LexError {}

pub fn tokenize(src: &str) -> Result<Vec<(Token, usize)>, LexError> {
    let bytes = src.as_bytes();
    let mut pos = 0usize;
    let mut line = 1usize;
    let mut tokens = Vec::new();

    'outer: while pos < bytes.len() {
        let b = bytes[pos];

        if b == b'\n' {
            line += 1;
            pos += 1;
            continue;
        }

        if b == b' ' || b == b'\t' || b == b'\r' {
            pos += 1;
            continue;
        }

        // Line comment
        if b == b'/' && pos + 1 < bytes.len() && bytes[pos + 1] == b'/' {
            while pos < bytes.len() && bytes[pos] != b'\n' {
                pos += 1;
            }
            continue;
        }

        // Block comment
        if b == b'/' && pos + 1 < bytes.len() && bytes[pos + 1] == b'*' {
            pos += 2;
            while pos + 1 < bytes.len() {
                if bytes[pos] == b'\n' {
                    line += 1;
                }
                if bytes[pos] == b'*' && bytes[pos + 1] == b'/' {
                    pos += 2;
                    continue 'outer;
                }
                pos += 1;
            }
            continue;
        }

        let tok_line = line;

        // Preprocessor directive: read whole line after '#'
        if b == b'#' {
            pos += 1;
            while pos < bytes.len() && bytes[pos] == b' ' {
                pos += 1;
            }
            let start = pos;
            while pos < bytes.len() && bytes[pos] != b'\n' {
                pos += 1;
            }
            let raw = src[start..pos].trim().to_string();
            tokens.push((Token::Directive(raw), tok_line));
            continue;
        }

        // String literal
        if b == b'"' {
            pos += 1;
            let mut buf = String::new();
            loop {
                if pos >= bytes.len() {
                    return Err(LexError {
                        line: tok_line,
                        msg: "unterminated string literal".into(),
                    });
                }
                let c = bytes[pos];
                if c == b'"' {
                    pos += 1;
                    break;
                }
                if c == b'\\' {
                    pos += 1;
                    if pos >= bytes.len() {
                        return Err(LexError {
                            line: tok_line,
                            msg: "unterminated escape sequence".into(),
                        });
                    }
                    match bytes[pos] {
                        b'n' => buf.push('\n'),
                        b'r' => buf.push('\r'),
                        b't' => buf.push('\t'),
                        b'"' => buf.push('"'),
                        b'\\' => buf.push('\\'),
                        esc => {
                            buf.push('\\');
                            buf.push(esc as char);
                        }
                    }
                    pos += 1;
                    continue;
                }
                if c == b'\n' {
                    line += 1;
                }
                // Collect multi-byte UTF-8 sequences intact
                if c >= 0x80 {
                    let char_start = pos;
                    pos += 1;
                    while pos < bytes.len() && bytes[pos] >= 0x80 && bytes[pos] < 0xC0 {
                        pos += 1;
                    }
                    match std::str::from_utf8(&bytes[char_start..pos]) {
                        Ok(s) => buf.push_str(s),
                        Err(_) => buf.push('\u{FFFD}'),
                    }
                } else {
                    buf.push(c as char);
                    pos += 1;
                }
            }
            tokens.push((Token::Str(buf), tok_line));
            continue;
        }

        // Hex literal: 0x... or 0X...
        if b == b'0' && pos + 1 < bytes.len() && (bytes[pos + 1] == b'x' || bytes[pos + 1] == b'X')
        {
            pos += 2;
            let start = pos;
            while pos < bytes.len() && bytes[pos].is_ascii_hexdigit() {
                pos += 1;
            }
            if start == pos {
                return Err(LexError {
                    line: tok_line,
                    msg: "empty hex literal after 0x".into(),
                });
            }
            let hex = &src[start..pos];
            // Parse as u64 first to handle values > i64::MAX that IDA sometimes uses
            let val = u64::from_str_radix(hex, 16)
                .map(|v| v as i64)
                .map_err(|_| LexError {
                    line: tok_line,
                    msg: format!("invalid hex literal: 0x{hex}"),
                })?;
            tokens.push((Token::Int(val), tok_line));
            continue;
        }

        // Decimal integer
        if b.is_ascii_digit() {
            let start = pos;
            while pos < bytes.len() && bytes[pos].is_ascii_digit() {
                pos += 1;
            }
            let s = &src[start..pos];
            let val = s.parse::<i64>().map_err(|_| LexError {
                line: tok_line,
                msg: format!("invalid decimal literal: {s}"),
            })?;
            tokens.push((Token::Int(val), tok_line));
            continue;
        }

        // Identifier or keyword
        if b.is_ascii_alphabetic() || b == b'_' {
            let start = pos;
            while pos < bytes.len() && (bytes[pos].is_ascii_alphanumeric() || bytes[pos] == b'_') {
                pos += 1;
            }
            tokens.push((Token::Ident(src[start..pos].to_string()), tok_line));
            continue;
        }

        // Single-character tokens
        let tok = match b {
            b'(' => Token::LParen,
            b')' => Token::RParen,
            b'{' => Token::LBrace,
            b'}' => Token::RBrace,
            b';' => Token::Semi,
            b',' => Token::Comma,
            b'=' => Token::Eq,
            b'-' => Token::Minus,
            _ => {
                return Err(LexError {
                    line: tok_line,
                    msg: format!("unexpected character: 0x{b:02x}"),
                });
            }
        };
        tokens.push((tok, tok_line));
        pos += 1;
    }

    Ok(tokens)
}
