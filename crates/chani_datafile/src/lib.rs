pub mod ast;
pub mod parser;
pub mod writer;

pub use writer::{BlockDict, InlineDict};

pub type SmallString = String; //smallstr::SmallString<[u8; 55]>;
