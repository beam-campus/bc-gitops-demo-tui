//! Demo TUI for bc_gitops demonstration
//!
//! Displays the status of demo_counter and allows interaction.

use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen},
};
use ratatui::{
    backend::CrosstermBackend,
    layout::{Constraint, Direction, Layout, Rect},
    style::{Color, Modifier, Style},
    text::{Line, Span},
    widgets::{Block, Borders, Gauge, Paragraph},
    Frame, Terminal,
};
use serde::Deserialize;
use std::{
    env,
    io::{self, stdout},
    time::{Duration, Instant},
};

const REFRESH_RATE: Duration = Duration::from_millis(500);
const DEFAULT_COUNTER_URL: &str = "http://localhost:8080";

#[derive(Debug, Deserialize)]
struct CounterStatus {
    count: i64,
}

#[derive(Debug, Deserialize)]
struct HealthStatus {
    status: String,
    app: String,
    count: i64,
}

struct App {
    counter_url: String,
    count: i64,
    health: String,
    last_error: Option<String>,
    should_quit: bool,
}

impl App {
    fn new(counter_url: String) -> Self {
        Self {
            counter_url,
            count: 0,
            health: "unknown".to_string(),
            last_error: None,
            should_quit: false,
        }
    }

    fn fetch_status(&mut self) {
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(2))
            .build()
            .unwrap();

        // Fetch health
        match client
            .get(format!("{}/health", self.counter_url))
            .send()
            .and_then(|r| r.json::<HealthStatus>())
        {
            Ok(health) => {
                self.health = health.status;
                self.count = health.count;
                self.last_error = None;
            }
            Err(e) => {
                self.health = "offline".to_string();
                self.last_error = Some(format!("Error: {}", e));
            }
        }
    }

    fn increment(&mut self) {
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(2))
            .build()
            .unwrap();

        match client
            .post(format!("{}/increment", self.counter_url))
            .send()
            .and_then(|r| r.json::<CounterStatus>())
        {
            Ok(status) => {
                self.count = status.count;
                self.last_error = None;
            }
            Err(e) => {
                self.last_error = Some(format!("Error: {}", e));
            }
        }
    }

    fn reset(&mut self) {
        let client = reqwest::blocking::Client::builder()
            .timeout(Duration::from_secs(2))
            .build()
            .unwrap();

        match client
            .post(format!("{}/reset", self.counter_url))
            .send()
        {
            Ok(_) => {
                self.count = 0;
                self.last_error = None;
            }
            Err(e) => {
                self.last_error = Some(format!("Error: {}", e));
            }
        }
    }
}

fn main() -> io::Result<()> {
    let counter_url = env::var("COUNTER_URL").unwrap_or_else(|_| DEFAULT_COUNTER_URL.to_string());

    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    // Create app
    let mut app = App::new(counter_url);
    let mut last_tick = Instant::now();

    loop {
        // Refresh data periodically
        if last_tick.elapsed() >= REFRESH_RATE {
            app.fetch_status();
            last_tick = Instant::now();
        }

        // Draw UI
        terminal.draw(|f| ui(f, &app))?;

        // Handle input
        if event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    match key.code {
                        KeyCode::Char('q') | KeyCode::Esc => app.should_quit = true,
                        KeyCode::Char('i') | KeyCode::Up => app.increment(),
                        KeyCode::Char('r') | KeyCode::Char('R') => app.reset(),
                        KeyCode::Char(' ') => app.fetch_status(),
                        _ => {}
                    }
                }
            }
        }

        if app.should_quit {
            break;
        }
    }

    // Restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())
}

fn ui(f: &mut Frame, app: &App) {
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .margin(2)
        .constraints([
            Constraint::Length(3),  // Title
            Constraint::Length(7),  // Status box
            Constraint::Length(5),  // Counter gauge
            Constraint::Length(3),  // Error
            Constraint::Min(0),     // Help
        ])
        .split(f.area());

    // Title
    let title = Paragraph::new(Line::from(vec![
        Span::styled(
            " bc_gitops ",
            Style::default()
                .fg(Color::Cyan)
                .add_modifier(Modifier::BOLD),
        ),
        Span::raw("Demo TUI"),
    ]))
    .block(Block::default().borders(Borders::ALL).title("bc-gitops-demo-tui"));
    f.render_widget(title, chunks[0]);

    // Status
    let health_color = match app.health.as_str() {
        "healthy" => Color::Green,
        "offline" => Color::Red,
        _ => Color::Yellow,
    };

    let status_text = vec![
        Line::from(vec![
            Span::raw("Status: "),
            Span::styled(&app.health, Style::default().fg(health_color)),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::raw("Counter: "),
            Span::styled(
                format!("{}", app.count),
                Style::default().fg(Color::Cyan).add_modifier(Modifier::BOLD),
            ),
        ]),
        Line::from(""),
        Line::from(vec![
            Span::styled("URL: ", Style::default().fg(Color::DarkGray)),
            Span::styled(&app.counter_url, Style::default().fg(Color::DarkGray)),
        ]),
    ];

    let status = Paragraph::new(status_text)
        .block(Block::default().borders(Borders::ALL).title("demo_counter Status"));
    f.render_widget(status, chunks[1]);

    // Counter gauge (0-100 scale, wraps)
    let gauge_value = (app.count % 100) as u16;
    let gauge = Gauge::default()
        .block(Block::default().borders(Borders::ALL).title("Counter Progress"))
        .gauge_style(Style::default().fg(Color::Cyan))
        .percent(gauge_value)
        .label(format!("{}/100", gauge_value));
    f.render_widget(gauge, chunks[2]);

    // Error display
    let error_text = app.last_error.as_deref().unwrap_or("No errors");
    let error_color = if app.last_error.is_some() {
        Color::Red
    } else {
        Color::DarkGray
    };
    let error = Paragraph::new(error_text)
        .style(Style::default().fg(error_color))
        .block(Block::default().borders(Borders::ALL).title("Last Error"));
    f.render_widget(error, chunks[3]);

    // Help
    let help = Paragraph::new(Line::from(vec![
        Span::styled(" i/Up ", Style::default().fg(Color::Yellow)),
        Span::raw("Increment  "),
        Span::styled(" r ", Style::default().fg(Color::Yellow)),
        Span::raw("Reset  "),
        Span::styled(" Space ", Style::default().fg(Color::Yellow)),
        Span::raw("Refresh  "),
        Span::styled(" q/Esc ", Style::default().fg(Color::Yellow)),
        Span::raw("Quit"),
    ]))
    .block(Block::default().borders(Borders::ALL).title("Controls"));
    f.render_widget(help, chunks[4]);
}
