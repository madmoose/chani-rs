use nom::{
    IResult, Parser,
    branch::alt,
    bytes::complete::{tag, take_until, take_while, take_while1},
    character::complete::{char, line_ending, not_line_ending},
    combinator::{all_consuming, eof, map, not, opt, peek, value},
    multi::many0,
    sequence::{delimited, pair, preceded},
};

use crate::SmallString;

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    DictStart {
        name: SmallString,
        key: SmallString,
        line: u32,
    },
    DictEnd {
        name: SmallString,
        line: u32,
    },
    KeyValue {
        key: SmallString,
        value: SmallString,
        line: u32,
    },
}

fn line_of(original: &str, current: &str) -> u32 {
    let offset = current.as_ptr() as usize - original.as_ptr() as usize;
    original[..offset].bytes().filter(|&b| b == b'\n').count() as u32 + 1
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

fn trim_multiline_lines(s: &str) -> String {
    let lines: Vec<&str> = s.lines().collect();

    let start = lines
        .iter()
        .position(|line| !line.trim().is_empty())
        .unwrap_or(lines.len());

    let end = lines
        .iter()
        .rposition(|line| !line.trim().is_empty())
        .map(|i| i + 1)
        .unwrap_or(start);

    let lines = &lines[start..end];

    let min_indent = lines
        .iter()
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.bytes().take_while(|&b| b == b' ').count())
        .min()
        .unwrap_or(0);

    lines
        .iter()
        .map(|line| {
            if line.trim().is_empty() {
                ""
            } else {
                &line[min_indent..]
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

fn key_value<'a>(original: &str, i: &'a str) -> IResult<&'a str, Token> {
    let line = line_of(original, i);
    let (i, key) = map(key_name, str::trim).parse(i)?;
    let (i, _) = (ws, char('='), ws).parse(i)?;
    let (i, val) = alt((
        map(multiline_value, trim_multiline_lines),
        map(single_line_value, |s: &str| s.trim().to_owned()),
    ))
    .parse(i)?;
    Ok((
        i,
        Token::KeyValue {
            key: key.into(),
            value: val.as_str().into(),
            line,
        },
    ))
}

fn single_line_pairs<'a>(original: &str, i: &'a str) -> IResult<&'a str, Vec<Token>> {
    let (i, first) = key_value(original, i)?;
    let (i, rest) = many0(preceded((ws, char(';'), ws), move |i| {
        key_value(original, i)
    }))
    .parse(i)?;
    let (i, _) = opt((ws, char(';'))).parse(i)?; // optional trailing semicolon
    let mut pairs = vec![first];
    pairs.extend(rest);
    Ok((i, pairs))
}

fn end_keyword(i: &str) -> IResult<&str, ()> {
    value(
        (),
        preceded(
            ws,
            (
                tag("end"),
                // not part of a longer identifier (e.g. `endemic`)
                not(take_while1(|c: char| c.is_alphanumeric() || c == '_')),
                // not a property assignment (e.g. `end = 0xdd1d`)
                not(preceded(ws, char('='))),
            ),
        ),
    )
    .parse(i)
}

// Parse dict body items until we see 'end'
fn dict_body<'a>(original: &str, name: &str, i: &'a str) -> IResult<&'a str, Vec<Token>> {
    let mut tokens = Vec::new();
    let mut input = i;

    loop {
        let (rest, _) = skip_blanks(input)?;
        input = rest;

        // Check for end keyword
        if let Ok((rest, _)) = end_keyword(input) {
            let line = line_of(original, input);
            let (rest, _) = skip_blanks(rest)?;
            tokens.push(Token::DictEnd {
                name: name.into(),
                line,
            });
            return Ok((rest, tokens));
        }

        // Try nested dict
        let (rest, _) = ws(input)?;
        if let Ok((rest, nested)) = dict_block(original, rest) {
            tokens.extend(nested);
            input = rest;
            continue;
        }

        // Try key-value(s) - may have multiple semicolon-separated pairs on one line
        let (rest, _) = ws(input)?;
        if let Ok((mut rest, kv)) = key_value(original, rest) {
            tokens.push(kv);
            // Consume any additional semicolon-separated pairs on same line
            while let Ok((r, _)) = (ws, char(';'), ws).parse(rest) {
                if let Ok((r, kv)) = key_value(original, r) {
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

        // Nothing matched - unrecoverable: we're inside a dict body that we committed to
        return Err(nom::Err::Failure(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Alt,
        )));
    }
}

fn dict_block<'a>(original: &str, i: &'a str) -> IResult<&'a str, Vec<Token>> {
    let (i, _) = ws(i)?;
    let line = line_of(original, i);
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
            line,
        }];
        let (i, body) = dict_body(original, name, i)?;
        tokens.extend(body);
        Ok((i, tokens))
    } else {
        // Single-line: has pairs on same line
        let (i, pairs) = single_line_pairs(original, i)?;
        let (i, _) = skip_blanks(i)?;
        let mut tokens = vec![Token::DictStart {
            name: name.into(),
            key: key.into(),
            line,
        }];
        tokens.extend(pairs);
        tokens.push(Token::DictEnd {
            name: name.into(),
            line,
        });
        Ok((i, tokens))
    }
}

fn file<'a>(original: &str, i: &'a str) -> IResult<&'a str, Vec<Token>> {
    let (mut i, _) = skip_blanks(i)?;
    let mut tokens = Vec::new();
    loop {
        let (rest, _) = skip_blanks(i)?;
        match dict_block(original, rest) {
            Ok((rest, block_tokens)) => {
                tokens.extend(block_tokens);
                i = rest;
            }
            Err(nom::Err::Error(_)) => {
                // No dict block at this position — end of top-level dicts
                i = rest;
                break;
            }
            Err(e) => return Err(e),
        }
    }
    let (i, _) = skip_blanks(i)?;
    Ok((i, tokens))
}

pub fn parse(input: &str) -> Result<Vec<Token>, String> {
    match all_consuming(|i| file(input, i)).parse(input) {
        Ok((_, tokens)) => Ok(tokens),
        Err(e) => {
            let remaining = match &e {
                nom::Err::Error(e) | nom::Err::Failure(e) => e.input,
                nom::Err::Incomplete(_) => input,
            };
            Err(parse_error_message(input, remaining))
        }
    }
}

fn parse_error_message(original: &str, remaining: &str) -> String {
    let offset = remaining.as_ptr() as usize - original.as_ptr() as usize;
    let prefix = &original[..offset];
    let line_num = prefix.bytes().filter(|&b| b == b'\n').count() + 1;
    let col = prefix.rfind('\n').map_or(offset, |p| offset - p - 1);
    let line_text = remaining.lines().next().unwrap_or("(end of input)");
    format!(
        "parse error at line {line_num}, column {}: unexpected input\n  {line_text}\n  {arrow}",
        col + 1,
        arrow = " ".repeat(col) + "^",
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kv(tokens: &[Token], idx: usize) -> (&str, &str) {
        if let Token::KeyValue { key, value, .. } = &tokens[idx] {
            (key.as_str(), value.as_str())
        } else {
            panic!("token {idx} is not a KeyValue: {:?}", tokens[idx]);
        }
    }

    #[test]
    fn end_as_property_key() {
        let input = "segment[seg001]:\n    type  = data\n    start = 0\n    end   = 0xdd1d\nend\n";
        let tokens = parse(input).expect("should parse");
        // DictStart, type=data, start=0, end=0xdd1d, DictEnd
        assert_eq!(tokens.len(), 5);
        assert_eq!(kv(&tokens, 1), ("type", "data"));
        assert_eq!(kv(&tokens, 2), ("start", "0"));
        assert_eq!(kv(&tokens, 3), ("end", "0xdd1d"));
        assert!(matches!(tokens[4], Token::DictEnd { .. }));
    }

    #[test]
    fn end_standalone_closes_block() {
        let input = "segment[s]:\n    x = 1\nend\n";
        let tokens = parse(input).expect("should parse");
        assert_eq!(tokens.len(), 3); // DictStart, x=1, DictEnd
        assert!(matches!(tokens[2], Token::DictEnd { .. }));
    }
}
