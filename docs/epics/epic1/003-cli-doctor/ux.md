# Story 003: CLI Doctor — UX
UX need: Moderate

Phase: Run Doctor, Goal: actionable diagnostics with clear next steps

Entry: user runs `duet-rpc doctor [--json] [--verbose] [--no-color]`

Exit:
- Complete: all required checks pass; exit 0
- Warn-only: non-fatal issues (e.g., no default config) with guidance; exit 0
- Failure: at least one required check fails; exit 1
- Internal error: unhandled error; exit 2

Steps:
- Parse flags
- Gather environment (OS/arch, TTY, env overrides)
- Discover config using precedence (see Data/Decisions)
- Run checks and collect statuses
- Render output (human or JSON)
- Set exit code per severity mapping

Touchpoints:
- Flags: `--json`, `--verbose`, `--no-color`
- Output: human-readable checklist with statuses and next steps; structured JSON
- Exit code: 0/1/2 per mapping
- Color/Unicode: respect `NO_COLOR`; disable icons when not a TTY

Copy (key labels/messages):
- Checks (ids → titles)
  - binary_version → Binary version
    - OK: "duet-rpc vX.Y.Z"
    - FAIL: "Unable to determine version"
  - platform → OS/arch
    - OK: "darwin-arm64" (example)
    - WARN: "Unsupported platform detected: <value>" (continues)
  - path_resolution → PATH resolution
    - OK: "Executable found on PATH at <path>"
    - FAIL: "Executable not found on PATH; add <dir> to PATH"
  - config_discovery → Config discovery
    - OK: "Using config at <path> (source: project/XDG/ENV)"
    - WARN: "Default config not found; searched: <paths>. See minimal example below."
  - config_read → Config readability/parse
    - OK: "Config parsed (N sections)"
    - WARN: "Unrecognized keys: <list> (ignored)"
    - FAIL: "Config unreadable/invalid at <path>: <error>. See minimal example below."
  - env_overrides → Environment overrides
    - INFO: "DUET_RPC_CONFIG=<path> in effect" (redact if needed)
- Minimal config (TOML):

```
[rpc]
endpoint = "http://localhost:8000"
```

Patterns and States:
- Statuses: ok, warn, fail, skip; map to colors (green/yellow/red/grey), respect `NO_COLOR`
- States: loading (brief), success, warn, fail, error; always render a summary
- Rendering conventions:
  - Human: checklist with per-check status, primary message, and "Next step" where applicable
  - JSON: see schema; no colors; machine-friendly strings only
- Unicode/TTY:
  - Use icons when TTY; fallback to plain text when not a TTY or when `--no-color`/`NO_COLOR` set

Alt/Recovery:
- Missing default config → show searched locations and the minimal TOML; suggest creating at project or XDG path
- Explicit config via `DUET_RPC_CONFIG` missing/unreadable → FAIL with next steps (fix path/permissions)
- Parse error → show truncated error with file/line if available; suggest comment out invalid keys; link to docs when available
- PATH not resolving → show how to add install dir to PATH for the platform
- Unsupported platform (info) → continue checks; note compatibility status
- Permissions → show simple `rwx` and owner; suggest chown/chmod or alternate path

Data/Decisions:
- Config precedence (final):
  1) ENV override: `DUET_RPC_CONFIG` (highest)
  2) Project: `./.duet-rpc.toml`, then `./duet-rpc.toml`
  3) XDG: `$XDG_CONFIG_HOME/duet-rpc/config.toml`, else `~/.config/duet-rpc/config.toml`
  4) System/global: none (skip)
- Required vs optional:
  - Required: binary version, PATH resolution, OS/arch detection, readable config if explicitly specified or present in project
  - Warn-only: missing default config; unreadable default-location config
  - Fail: unreadable config when explicitly specified via `DUET_RPC_CONFIG` or found in project
- Exit codes:
  - 0 = OK or WARN only
  - 1 = at least one required FAIL
  - 2 = internal/unexpected error
- JSON schema (final):
  - `version`: string
  - `platform`: { `os`, `arch` }
  - `checks`: [ { `id`, `title`, `status` (ok|warn|fail|skip), `severity` (info|warn|error), `details`, `doc_url`? } ]
  - `summary`: { `ok`, `warn`, `fail`, `skip` }
  - `elapsed_ms`: number
- Verbose (`--verbose`) includes:
  - Resolved binary path and version/build info
  - PATH entries and whether current exe dir is in PATH
  - Config search order, attempted paths with status (exists/perm/parse), chosen path, and precedence trace
  - Permissions: mode/owner for chosen config; simple `rwx`
  - Env: `DUET_RPC_CONFIG`, `XDG_CONFIG_HOME`, `NO_COLOR`, `CI`, `TERM` (redact sensitive values)
  - Platform: OS, arch, TTY detection

Defaults:
- No network/provider calls
- No writes or config mutation
- Color enabled by default; respect `NO_COLOR` and `--no-color`
- Continue running checks to completion; never abort early (improves diagnostics)
- Typical runtime target: < 2s on local dev

Integration:
- PATH lookup behavior (per-OS guidance)
- XDG paths on Linux/macOS; Windows roaming profile equivalent out of scope for v1
- CI usage: enable `--json`; treat WARN as pass, FAIL as fail; capture elapsed time

GSM:
- Goals: quickly diagnose setup; reduce support requests
- Signals: fail/warn counts, time to complete, first-run success rate
- Metrics:
  - Doctor passes on support matrix in CI
  - Median runtime < 2s; P95 < 5s
  - JSON schema validated in CI

Questions (resolved):
- Config precedence and paths: approved
- Severity mapping for checks: approved
- Exit codes: approved
- JSON schema fields: approved (no changes)
- Verbose contents: approved
- Output behavior (colors/TTY): approved
- Supported OS/arch: darwin-amd64/arm64; linux-amd64/arm64; windows-amd64
- Minimal config: TOML snippet for `[rpc].endpoint` only

Risks:
- Platform-specific path and permission differences → mitigate with targeted guidance per OS
- Inconsistent precedence if CLI deviates from spec → enforce via unit tests and JSON schema checks
- TTY and color handling differences on Windows terminals → fall back to plain text and respect `NO_COLOR`
