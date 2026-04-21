use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, line_ending, not_line_ending},
    combinator::{all_consuming, eof, map, opt, peek, value},
    multi::many0,
    sequence::{delimited, pair, preceded},
};

use crate::SmallString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    DictStart {
        name: SmallString,
        key: SmallString,
    },
    DictEnd {
        name: SmallString,
    },
    KeyValue {
        key: SmallString,
        value: SmallString,
    },
}

fn ws(i: &str) -> IResult<&str, &str> {
    take_while(|c| c == ' ' || c == '\t').parse(i)
}

fn comment(i: &str) -> IResult<&str, ()> {
    value((), (tag("//"), not_line_ending)).parse(i)
}

fn blank_line(i: &str) -> IResult<&str, ()> {
    // Must consume at least a newline (not just eof)
    value((), (ws, opt(comment), line_ending)).parse(i)
}

fn skip_blanks(i: &str) -> IResult<&str, ()> {
    value((), many0(blank_line)).parse(i)
}

fn dict_name(i: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c.is_ascii_lowercase()).parse(i)
}

fn dict_key(i: &str) -> IResult<&str, &str> {
    take_while(|c: char| c != ']').parse(i)
}

fn dict_header(i: &str) -> IResult<&str, (&str, &str)> {
    pair(dict_name, delimited(char('['), dict_key, char(']'))).parse(i)
}

fn key_name(i: &str) -> IResult<&str, &str> {
    take_while1(|c: char| c != '=' && c != '\n' && c != '\r' && c != ';').parse(i)
}

fn single_line_value(i: &str) -> IResult<&str, &str> {
    // Stop at ; or newline or // comment, but treat [...] as an opaque bracket group
    // so that e.g. `[u16; 289]` is consumed as a single token.
    let bytes = i.as_bytes();
    let mut end = 0;
    let mut depth = 0usize;
    while end < bytes.len() {
        match bytes[end] {
            b'[' => {
                depth += 1;
                end += 1;
            }
            b']' if depth > 0 => {
                depth -= 1;
                end += 1;
            }
            b';' | b'\n' | b'\r' if depth == 0 => break,
            b'/' if depth == 0 && bytes.get(end + 1) == Some(&b'/') => break,
            _ => end += 1,
        }
    }
    Ok((&i[end..], &i[..end]))
}

fn multiline_value(i: &str) -> IResult<&str, &str> {
    delimited(tag("[[["), take_until("]]]"), tag("]]]")).parse(i)
}

fn kv_value(i: &str) -> IResult<&str, &str> {
    alt((multiline_value, single_line_value)).parse(i)
}

fn key_value(i: &str) -> IResult<&str, Token> {
    let (i, key) = map(key_name, str::trim).parse(i)?;
    let (i, _) = (ws, char('='), ws).parse(i)?;
    let (i, val) = map(kv_value, str::trim).parse(i)?;
    Ok((
        i,
        Token::KeyValue {
            key: key.into(),
            value: val.into(),
        },
    ))
}

fn single_line_pairs(i: &str) -> IResult<&str, Vec<Token>> {
    let (i, first) = key_value(i)?;
    let (i, rest) = many0(preceded((ws, char(';'), ws), key_value)).parse(i)?;
    let (i, _) = opt((ws, char(';'))).parse(i)?; // optional trailing semicolon
    let mut pairs = vec![first];
    pairs.extend(rest);
    Ok((i, pairs))
}

fn end_keyword(i: &str) -> IResult<&str, ()> {
    value((), preceded(ws, tag("end"))).parse(i)
}

// Parse dict body items until we see 'end'
fn dict_body<'a>(name: &str, i: &'a str) -> IResult<&'a str, Vec<Token>> {
    let mut tokens = Vec::new();
    let mut input = i;

    loop {
        let (rest, _) = skip_blanks(input)?;
        input = rest;

        // Check for end keyword
        if let Ok((rest, _)) = end_keyword(input) {
            let (rest, _) = skip_blanks(rest)?;
            tokens.push(Token::DictEnd { name: name.into() });
            return Ok((rest, tokens));
        }

        // Try nested dict
        let (rest, _) = ws(input)?;
        if let Ok((rest, nested)) = dict_block(rest) {
            tokens.extend(nested);
            input = rest;
            continue;
        }

        // Try key-value(s) - may have multiple semicolon-separated pairs on one line
        let (rest, _) = ws(input)?;
        if let Ok((mut rest, kv)) = key_value(rest) {
            tokens.push(kv);
            // Consume any additional semicolon-separated pairs on same line
            while let Ok((r, _)) = (ws, char(';'), ws).parse(rest) {
                if let Ok((r, kv)) = key_value(r) {
                    tokens.push(kv);
                    rest = r;
                } else {
                    rest = r; // allow trailing semicolon
                    break;
                }
            }
            let (rest, _) = skip_blanks(rest)?;
            input = rest;
            continue;
        }

        // Nothing matched - error
        return Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alt,
        )));
    }
}

fn dict_block(i: &str) -> IResult<&str, Vec<Token>> {
    let (i, _) = ws(i)?;
    let (i, (name, key)) = dict_header(i)?;
    let (i, _) = char(':').parse(i)?;
    let (i, _) = ws(i)?;

    // Check if single-line (has content before newline) or multi-line (newline after colon)
    if let Ok((_, _)) = peek(alt((line_ending::<&str, nom::error::Error<&str>>, eof))).parse(i) {
        // Multi-line: colon followed by newline
        let (i, _) = alt((line_ending, eof)).parse(i)?;
        let mut tokens = vec![Token::DictStart {
            name: name.into(),
            key: key.into(),
        }];
        let (i, body) = dict_body(name, i)?;
        tokens.extend(body);
        Ok((i, tokens))
    } else {
        // Single-line: has pairs on same line
        let (i, pairs) = single_line_pairs(i)?;
        let (i, _) = skip_blanks(i)?;
        let mut tokens = vec![Token::DictStart {
            name: name.into(),
            key: key.into(),
        }];
        tokens.extend(pairs);
        tokens.push(Token::DictEnd { name: name.into() });
        Ok((i, tokens))
    }
}

fn file(i: &str) -> IResult<&str, Vec<Token>> {
    let (i, _) = skip_blanks(i)?;
    let (i, blocks) = many0(preceded(skip_blanks, dict_block)).parse(i)?;
    let (i, _) = skip_blanks(i)?;
    Ok((i, blocks.into_iter().flatten().collect()))
}

pub fn parse(input: &str) -> Result<Vec<Token>, String> {
    match all_consuming(file).parse(input) {
        Ok((_, tokens)) => Ok(tokens),
        Err(e) => Err(format!("parse error: {}", e)),
    }
}
