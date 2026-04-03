use crate::parser::Token;

/// A DREAMM document containing top-level dictionaries
#[derive(Debug, Clone, PartialEq)]
pub struct Document {
    pub dicts: Vec<Dict>,
}

/// A dictionary block with name, key, and ordered contents
#[derive(Debug, Clone, PartialEq)]
pub struct Dict {
    pub name: String,
    pub key: String,
    pub items: Vec<Item>,
}

/// An item within a dictionary - either a property or nested dict
#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Property { key: String, value: String },
    Dict(Dict),
}

impl Document {
    pub fn from_tokens(tokens: Vec<Token>) -> Result<Self, String> {
        let mut stack: Vec<Dict> = Vec::new();
        let mut result: Vec<Dict> = Vec::new();

        for token in tokens {
            match token {
                Token::DictStart { name, key } => {
                    stack.push(Dict {
                        name,
                        key,
                        items: Vec::new(),
                    });
                }
                Token::KeyValue { key, value } => {
                    let current = stack.last_mut().ok_or("KeyValue outside of dictionary")?;
                    // Last wins: remove existing property with same key
                    current
                        .items
                        .retain(|item| !matches!(item, Item::Property { key: k, .. } if k == &key));
                    current.items.push(Item::Property { key, value });
                }
                Token::DictEnd { name } => {
                    let dict = stack
                        .pop()
                        .ok_or_else(|| format!("unexpected end for '{}'", name))?;
                    if dict.name != name {
                        return Err(format!(
                            "mismatched end: expected '{}', got '{}'",
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
            return Err(format!(
                "unclosed dictionary: '{}'",
                stack.last().unwrap().name
            ));
        }

        Ok(Document { dicts: result })
    }
}
