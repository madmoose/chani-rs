use crate::{SmallString, parser::Token};

/// A DREAMM document containing top-level dictionaries
#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    pub dicts: Vec<Dict>,
}

/// A dictionary block with name, key, and ordered contents
#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub name: SmallString,
    pub key: SmallString,
    pub items: Vec<Item>,
    pub line: u32,
}

/// An item within a dictionary - either a property or nested dict
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Property {
        key: SmallString,
        value: SmallString,
        line: u32,
    },
    Dict(Dict),
}

impl Document {
    pub fn from_tokens(tokens: Vec<Token>) -> Result<Self, String> {
        let mut stack: Vec<Dict> = Vec::new();
        let mut result: Vec<Dict> = Vec::new();

        for token in tokens {
            match token {
                Token::DictStart { name, key, line } => {
                    stack.push(Dict {
                        name,
                        key,
                        items: Vec::new(),
                        line,
                    });
                }
                Token::KeyValue { key, value, line } => {
                    let current = stack.last_mut().ok_or_else(|| {
                        format!("line {line}: key-value '{}' outside of any dictionary", key)
                    })?;
                    // Last wins: remove existing property with same key
                    current
                        .items
                        .retain(|item| !matches!(item, Item::Property { key: k, .. } if k == &key));
                    current.items.push(Item::Property { key, value, line });
                }
                Token::DictEnd { name, line } => {
                    let dict = stack.pop().ok_or_else(|| {
                        format!("line {line}: unexpected 'end' for '{}' with no open dictionary", name)
                    })?;
                    if dict.name != name {
                        return Err(format!(
                            "line {line}: mismatched 'end': expected '{}', got '{}'",
                            dict.name, name
                        ));
                    }
                    if let Some(parent) = stack.last_mut() {
                        parent.items.push(Item::Dict(dict));
                    } else {
                        result.push(dict);
                    }
                }
            }
        }

        if !stack.is_empty() {
            let dict = stack.last().unwrap();
            return Err(format!(
                "line {}: unclosed dictionary '{}'",
                dict.line, dict.name
            ));
        }

        Ok(Document { dicts: result })
    }
}
