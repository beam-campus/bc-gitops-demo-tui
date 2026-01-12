# bc-gitops-demo-tui

Rust TUI + Erlang OTP wrapper for [bc_gitops](https://github.com/beam-campus/bc-gitops) demonstration.

## Overview

This demonstrates how bc_gitops can deploy applications with non-BEAM executables using the OTP `priv/` directory pattern:

1. **Rust TUI**: A terminal user interface that displays `demo_counter` status
2. **Erlang Wrapper**: An OTP application that launches the TUI from `priv/`

## Architecture

```
demo_tui/
  src/                    # Erlang OTP wrapper
    demo_tui.erl          # API to launch TUI
    demo_tui_app.erl      # Application behavior
    demo_tui_sup.erl      # Supervisor
  rust/                   # Rust TUI source
    src/main.rs           # TUI implementation
    Cargo.toml            # Rust dependencies
  priv/                   # Precompiled binaries
    linux-x86_64/demo-tui
    linux-aarch64/demo-tui
    macos-x86_64/demo-tui
    macos-aarch64/demo-tui
```

## Quick Start

### Using Precompiled Binaries

```bash
# Add to your rebar.config
{deps, [
    {demo_tui, "0.1.0"}
]}.

# In Erlang shell
application:start(demo_tui).
demo_tui:start().  %% Launches TUI

# Or with custom counter URL
demo_tui:start("http://other-host:8081").
```

### Building from Source

```bash
# Clone the repo
git clone https://github.com/beam-campus/bc-gitops-demo-tui.git
cd bc-gitops-demo-tui

# Build Rust TUI
cd rust
cargo build --release

# Copy to priv/
mkdir -p ../priv/linux-x86_64
cp target/release/demo-tui ../priv/linux-x86_64/

# Build Erlang wrapper
cd ..
rebar3 compile
```

## TUI Controls

| Key | Action |
|-----|--------|
| `i` / `Up` | Increment counter |
| `r` | Reset counter |
| `Space` | Refresh status |
| `q` / `Esc` | Quit |

## Configuration

| Key | Default | Description |
|-----|---------|-------------|
| `counter_url` | `http://localhost:8080` | URL of demo_counter API |
| `auto_start` | `false` | Auto-launch TUI on application start |

## GitOps Deployment

To deploy via bc_gitops, add an `app.config` to your GitOps repo:

```erlang
#{
    name => demo_tui,
    source => {hex, "demo_tui", "0.1.0"},
    config => #{
        counter_url => "http://localhost:8081",
        auto_start => true
    }
}.
```

## Platform Support

Precompiled binaries are provided for:

- Linux x86_64
- Linux aarch64 (ARM64)
- macOS x86_64 (Intel)
- macOS aarch64 (Apple Silicon)

## Related

- [bc_gitops](https://github.com/beam-campus/bc-gitops) - GitOps for the BEAM
- [bc-gitops-demo-web](https://github.com/beam-campus/bc-gitops-demo-web) - Host application
- [bc-gitops-demo-counter](https://github.com/beam-campus/bc-gitops-demo-counter) - Counter app
- [bc-gitops-demo-repo](https://github.com/beam-campus/bc-gitops-demo-repo) - GitOps specs

## License

MIT
