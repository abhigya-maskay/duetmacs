# Story 001: CLI Version/Help Bootstrap
UX need: Light

Phase: Verify install and discover commands, Goal: Confirm binary responds and exposes version/help
Entry: User has terminal access and `codex-rpc` installed; Exit: Version printed; help lists subcommands; process exits 0

Steps
- User runs `codex-rpc --version` → prints `0.1.0`; exit 0
- User runs `codex-rpc version` → prints `0.1.0`; exit 0
- User runs `codex-rpc --help` or `codex-rpc` → prints help; exit 0

Touchpoints
- Commands: `codex-rpc --version`, `codex-rpc version`, `codex-rpc --help`, `codex-rpc`
- Global options: `-V/--version`, `-h/--help`

Copy
- Version output: `0.1.0`
- Help synopsis: `codex-rpc [COMMAND] [OPTIONS]`
- version: Print version information
- doctor: Diagnose environment (not yet implemented)
- rpc: Start RPC server (not yet implemented)
- prompt: Run prompt tools (not yet implemented)
- Footer: See 'codex-rpc <command> --help' for more information.

Patterns and States
- Success: outputs match above; exit code 0
- Error: unknown subcommand/flag → error + usage; exit code 2; no stack traces
- Performance: responds within ~100ms; newline-terminated output
- Color: colorized help when TTY; honor `NO_COLOR`; plain when piped

Alt/Recovery
- Invalid input: show error + usage; suggest `--help`
- No args: show help; exit 0

Data/Decisions
- Version authority: package manifest (e.g., Cargo.toml)
- Help layout: CLI framework defaults (e.g., Clap)
- Exit codes: 0 success; 2 incorrect usage

Defaults
- No args → help
- `-h/--help` and `-V/--version` supported globally

Integration
- None; telemetry not collected for this story

GSM
- Goal: Provide discoverable CLI entrypoint
- Signals: Correct text and exit codes across platforms
- Metrics: CI checks for `--version`/`--help` pass; local smoke test documented
