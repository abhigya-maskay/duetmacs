# DuetMacs

![CI](https://github.com/abhigya-maskay/duetmacs/actions/workflows/ci.yml/badge.svg)

A Haskell CLI project in early development.

## What's Implemented

The `duet-rpc` binary currently provides:
- `--version` and `version` command showing semantic version from Cabal
- `--help` command displaying available commands
- Structured logging to stderr with `--log-level` control
- `DUET_RPC_LOG` environment variable for log file redirection
- Color-aware output respecting `NO_COLOR` and `--no-color`
- TTY detection for automatic color handling

## Build & Run

```bash
cd duet-rpc
cabal build
cabal run duet-rpc -- --version
cabal run duet-rpc -- --help
```

## Documentation

See [duetmacs-docs](./duetmacs-docs/) for architecture decisions, implementation plans, and development guides.
