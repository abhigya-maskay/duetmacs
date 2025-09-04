# Story 003: CLI Doctor

As a CLI user, I want a `duet-rpc doctor` command that inspects my environment and reports actionable checks so that I can diagnose setup issues quickly.

## Priority
- Must

## Dependencies
- Story 001 (CLI Version/Help Bootstrap)

## Non-Goals
- Network/provider connectivity checks
- Emacs integration
- Writing configs or changing user environment
- Performance benchmarking

## Business Rationale
- Provides immediate, actionable feedback to reduce setup friction and support requests.
- Establishes a consistent diagnostic surface for both local use and CI.

## Acceptance Criteria (Given/When/Then)
- Given the binary is installed, When I run `duet-rpc doctor`, Then it returns exit code 0 on success and prints a human-readable checklist including: binary version, OS/arch, config file discovery locations, and PATH resolution of the binary.
- Given a missing or unreadable default config, When I run `duet-rpc doctor`, Then it prints a clear warning with the expected locations and an example minimal config.
- Given the `--json` flag, When I run `duet-rpc doctor --json`, Then it outputs structured JSON with the same checks and machine-readable statuses.
- Given an optional `--verbose` flag, When I run `duet-rpc doctor --verbose`, Then it includes additional details like resolved paths, permissions, and environment overrides.

- Given terminal capabilities and env, When I run with `--no-color` or `NO_COLOR=1` or in a non-TTY, Then output disables colors and Unicode icons; otherwise statuses are colorized.
- Given failure conditions, When any required check fails, Then the command returns exit code 1; Given an unexpected internal error, Then it returns exit code 2; Given only warnings, Then it returns exit code 0.
- Given `--verbose`, When the command runs, Then it includes a config discovery precedence trace (search order and attempted paths with statuses) and enumerates PATH entries along with the resolved executable path.
- Given config is loaded, When doctor runs, Then it reports which config file (if any) was loaded and its path.
- Given config is loaded, When doctor runs with `--verbose`, Then it shows parsed config values (redacting secrets like API keys).
- Given `--json`, When the command runs, Then the output includes fields: `version`, `platform` (`os`,`arch`), `checks` (each with `id`,`title`,`status`,`severity`,`details`,`doc_url?`), a `summary` counts object, `config` (with `loaded_from` path and `validated` boolean), and `elapsed_ms`.

## Assumptions / Open Questions
- Assumption: Config discovery follows a predictable precedence (global → project) — Confidence: high — Impact: inconsistent diagnoses — Validation: document and align with Configuration epic.
- Open question: Exact config filenames/paths to check (e.g., XDG, `~/.config/duet-rpc/config.toml`, project `.duet-rpc.toml`).

## Decisions
- Config precedence order: `DUET_RPC_CONFIG` (env override) → project `./.duet-rpc.toml` then `./duet-rpc.toml` → XDG `$XDG_CONFIG_HOME/duet-rpc/config.toml` else `~/.config/duet-rpc/config.toml` → no system/global path in v1.
- Exit codes mapping: 0 = OK or WARN-only; 1 = at least one required FAIL; 2 = internal/unexpected error.
- Supported OS/arch targets: darwin-amd64, darwin-arm64, linux-amd64, linux-arm64, windows-amd64.
- Minimal config example (TOML):
  
  ```toml
  [rpc]
  endpoint = "http://localhost:8000"
  ```

## Success Metrics
- `duet-rpc doctor` passes on supported platforms in CI.
- Users report fewer setup issues due to clear, actionable diagnostics.
 - Median local runtime < 2s (P95 < 5s);
 - JSON schema for `--json` output validated in CI.
