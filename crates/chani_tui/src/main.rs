//! Interactive terminal browser for `.chani` disassembly projects.
//!
//! The listing is the exact same one the HTML exporter
//! (`chani_disasm::bin::disasm --html`) produces — both walk the shared widget
//! grid from [`chani_disasm::layout::generate_widgets_with_options`] — but here
//! it is coloured into terminal cells and made keyboard/mouse navigable: every
//! link the HTML makes clickable is a followable target (see [`app::App`]).

use std::io::{self, Stdout};
use std::path::PathBuf;

use anyhow::{Context, Result};
use chani_disasm::project::Project;
use clap::Parser;
use crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, SetTitle, disable_raw_mode, enable_raw_mode,
};
use ratatui::Terminal;
use ratatui::backend::CrosstermBackend;

mod app;

use app::App;

type Tui = Terminal<CrosstermBackend<Stdout>>;

#[derive(Parser)]
#[command(about = "Interactive terminal browser for .chani disassembly projects")]
struct Cli {
    /// Path to a `.chani` project file.
    project: PathBuf,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let path = cli.project.to_str().context("project path is not UTF-8")?;
    // The authoritative (un-analyzed) document; the App derives the analyzed
    // view itself, and re-derives it after each edit.
    let base = Project::from_project_file(path).map_err(|e| anyhow::anyhow!("{path}: {e}"))?;

    let mut app = App::new(base, cli.project.clone());

    let title = format!(
        "Chani — {}",
        cli.project
            .file_name()
            .map(|n| n.to_string_lossy())
            .unwrap_or_else(|| path.into())
    );
    let mut terminal = init_terminal(&title).context("failed to initialize terminal")?;
    let result = run(&mut terminal, &mut app);
    restore_terminal().context("failed to restore terminal")?;
    result
}

/// Enter raw mode + alternate screen + mouse capture, set the window `title`,
/// and install a panic hook so a crash always restores the terminal before
/// printing the panic message.
fn init_terminal(title: &str) -> io::Result<Tui> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(
        stdout,
        EnterAlternateScreen,
        EnableMouseCapture,
        SetTitle(title)
    )?;

    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let _ = restore_terminal();
        default_hook(info);
    }));

    Terminal::new(CrosstermBackend::new(stdout))
}

fn restore_terminal() -> io::Result<()> {
    disable_raw_mode()?;
    execute!(io::stdout(), LeaveAlternateScreen, DisableMouseCapture)
}

/// The draw / read-event loop. Returns when the user quits.
fn run(terminal: &mut Tui, app: &mut App) -> Result<()> {
    while !app.should_quit() {
        terminal.draw(|frame| app.render(frame))?;
        match event::read()? {
            Event::Key(key) if key.kind == KeyEventKind::Press => app.on_key(key),
            Event::Mouse(mouse) => app.on_mouse(mouse),
            Event::Resize(_, _) => {}
            _ => {}
        }
    }
    Ok(())
}
