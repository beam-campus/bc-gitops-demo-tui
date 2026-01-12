#!/bin/bash
set -euo pipefail

# Build script for demo-tui
# Compiles Rust TUI and copies to priv/

cd "$(dirname "$0")/.."
ROOT_DIR="$(pwd)"

# Detect platform
OS="$(uname -s | tr '[:upper:]' '[:lower:]')"
ARCH="$(uname -m)"

case "$OS" in
    linux) OS_NAME="linux" ;;
    darwin) OS_NAME="macos" ;;
    *) echo "Unsupported OS: $OS"; exit 1 ;;
esac

case "$ARCH" in
    x86_64) ARCH_NAME="x86_64" ;;
    aarch64|arm64) ARCH_NAME="aarch64" ;;
    *) echo "Unsupported architecture: $ARCH"; exit 1 ;;
esac

PLATFORM="${OS_NAME}-${ARCH_NAME}"
PRIV_DIR="$ROOT_DIR/priv/$PLATFORM"

echo "Building demo-tui for $PLATFORM..."

# Build Rust TUI
cd "$ROOT_DIR/rust"
cargo build --release

# Create priv directory
mkdir -p "$PRIV_DIR"

# Copy binary
cp target/release/demo-tui "$PRIV_DIR/"
chmod +x "$PRIV_DIR/demo-tui"

echo "Binary installed to $PRIV_DIR/demo-tui"

# Build Erlang wrapper
cd "$ROOT_DIR"
rebar3 compile

echo "Build complete!"
