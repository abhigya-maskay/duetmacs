# Story 001: CLI Version/Help Bootstrap

As a CLI user, I want a minimal `codex-rpc` binary that builds and exposes version and help so that I can verify installation and discover available commands.

## Priority
- Must

## Dependencies
- None

## Non-Goals
- Implementing behavior for `doctor`, `rpc`, or `prompt`
- Logging, configuration precedence, packaging/release, or Emacs integration

## Business Rationale
- Ensures the toolchain is correctly installed and discoverable
- Establishes a testable CLI entrypoint to anchor subsequent stories

## Acceptance Criteria (Given/When/Then)
- Given the binary is installed, When I run `codex-rpc --version`, Then it prints a semantic version (e.g., `0.1.0`) and exits with code 0.
- Given the binary is installed, When I run `codex-rpc version`, Then it prints the same semantic version and exits with code 0.
- Given the binary is installed, When I run `codex-rpc --help`, Then it shows a synopsis and lists subcommands `version`, `doctor`, `rpc`, and `prompt` with one-line descriptions, and exits 0.
- Given no arguments, When I run `codex-rpc`, Then it prints the help text and exits 0.
- Given the help output, When I read it, Then it clearly indicates that `doctor`, `rpc`, and `prompt` are available subcommands (their behavior implemented in later stories).
 - Given an unknown subcommand or flag is provided, When I run `codex-rpc <unknown>`, Then the CLI prints an error followed by usage/help, exits with code 2, and does not print a stack trace.
 - Given any output is printed, Then it is newline-terminated; help output is colorized when attached to a TTY, honors `NO_COLOR`, and is plain (no color) when piped.
 - Given a help synopsis is shown, Then it includes `codex-rpc [COMMAND] [OPTIONS]` and a footer: `See 'codex-rpc <command> --help' for more information.`
 - Given typical developer hardware, Then the above commands respond within ~100ms.

## Assumptions / Open Questions
- Assumption: Project uses a single source of truth for version (e.g., `--version` reads from build metadata) — Confidence: high — Impact if wrong: duplicate version sources drift — Validation: choose and document a single version authority.
- Open question: Preferred CLI framework (e.g., native, Clap, Cobra, etc.)? — Resolve before implementation story.
 - Assumption: Version authority is the package manifest (e.g., Cargo.toml) and the CLI framework's default help layout is used.

## Defaults
- No args shows help and exits 0.
- `-h/--help` and `-V/--version` supported globally.

## Success Metrics
- `codex-rpc --version` and `codex-rpc --help` run successfully in CI on all target platforms.
- Developer can build and run the binary locally with the same outputs.
