use std::io;

use chani_disasm::project::Project;

fn main() {
    let path = std::env::args().nth(1).unwrap_or_else(|| {
        eprintln!("Usage: dump_project <file.chani>");
        std::process::exit(1);
    });

    let project = Project::from_project_file(&path).unwrap_or_else(|e| {
        eprintln!("{path}: {e}");
        std::process::exit(1);
    });

    let mut stdout = io::BufWriter::new(io::stdout().lock());
    if let Err(e) = project.write_to(&mut stdout) {
        eprintln!("error: {e}");
        std::process::exit(1);
    }
}
