//! Interactive terminal browser for `.chani` disassembly projects.
//!
//! The listing is the exact same one the HTML exporter
//! (`chani_disasm::bin::disasm --html`) produces — both walk the shared widget
//! grid from [`chani_disasm::layout::generate_widgets_with_options`] — but here
//! it is coloured into terminal cells and made keyboard/mouse navigable: every
//! link the HTML makes clickable is a followable target (see [`app::App`]).

use std::io::{self, Stdout};
use std::path::PathBuf;
use std::sync::mpsc::{self, Receiver};
use std::time::Duration;

use anyhow::{Context, Result};
use chani_disasm::project::Project;
use clap::Parser;
use crossterm::event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyEventKind};
use crossterm::execute;
use crossterm::terminal::{
    EnterAlternateScreen, LeaveAlternateScreen, SetTitle, disable_raw_mode, enable_raw_mode,
};
use notify::{RecursiveMode, Watcher};
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

    // Watch the project file for external changes (e.g. `chaniq set`). We watch
    // the containing directory non-recursively rather than the file itself, since
    // editors and tools commonly replace the file via rename — a direct file
    // watch would stop firing after the first such write. Events are filtered to
    // the project's file name and coalesced into a single signal per loop tick.
    let (tx, rx) = mpsc::channel();
    let watch_name = cli.project.file_name().map(|n| n.to_owned());
    let mut watcher = notify::recommended_watcher(move |res: notify::Result<notify::Event>| {
        if let Ok(event) = res {
            let touches_file = match &watch_name {
                Some(name) => event.paths.iter().any(|p| p.file_name() == Some(name)),
                None => true,
            };
            if touches_file {
                let _ = tx.send(());
            }
        }
    })
    .context("failed to create file watcher")?;
    let watch_dir = cli
        .project
        .parent()
        .filter(|p| !p.as_os_str().is_empty())
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));
    // A watch failure is non-fatal: the TUI still works, just without auto-reload.
    let _ = watcher.watch(&watch_dir, RecursiveMode::NonRecursive);

    let title = format!(
        "Chani — {}",
        cli.project
            .file_name()
            .map(|n| n.to_string_lossy())
            .unwrap_or_else(|| path.into())
    );
    let mut terminal = init_terminal(&title).context("failed to initialize terminal")?;
    let result = run(&mut terminal, &mut app, &rx);
    restore_terminal().context("failed to restore terminal")?;
    drop(watcher);
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
///
/// Input is serviced via a timed `poll` so the loop can also drain file-change
/// signals from `rx` (the project-file watcher) and ask the app to react. The
/// poll timeout only bounds how quickly a file change is noticed; key/mouse
/// latency is unaffected because `poll` returns immediately when input is ready.
fn run(terminal: &mut Tui, app: &mut App, rx: &Receiver<()>) -> Result<()> {
    while !app.should_quit() {
        terminal.draw(|frame| app.render(frame))?;
        if event::poll(Duration::from_millis(200))? {
            match event::read()? {
                Event::Key(key) if key.kind == KeyEventKind::Press => app.on_key(key),
                Event::Mouse(mouse) => app.on_mouse(mouse),
                Event::Resize(_, _) => {}
                _ => {}
            }
        }
        // Coalesce a burst of watcher events into a single reload attempt.
        let mut changed = false;
        while rx.try_recv().is_ok() {
            changed = true;
        }
        if changed {
            app.handle_external_change();
        }
    }
    Ok(())
}
