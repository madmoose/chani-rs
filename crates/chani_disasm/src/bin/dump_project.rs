use std::io;

use chani_datafile::{ast, parser};
use chani_disasm::project::Project;

fn main() {
    let path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: dump_project <file.chani>");
        std::process::exit(1);
    });

    let content = std::fs::read_to_string(&path).unwrap_or_else(|e| {
        eprintln!("{path}: {e}");
        std::process::exit(1);
    });

    let tokens = parser::parse(&content).unwrap();
    let doc = ast::Document::from_tokens(tokens).unwrap();
    let project = Project::from_document(doc).unwrap();

    let mut stdout = io::BufWriter::new(io::stdout().lock());
    if let Err(e) = project.write_to(&mut stdout) {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}
