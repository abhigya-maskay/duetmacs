# Story 002: Emacs Init and Health Check

As an Emacs user, I want the Emacs package to load, expose a version command, and perform a basic health check so that I can verify the plugin is installed and can reach the CLI.

## Priority
- Must

## Dependencies
- Story 001 (CLI Version/Help Bootstrap)

## Non-Goals
- Editing/chat features or patch flows
- Full RPC protocol beyond a simple ping
- Packaging/release automation

## Business Rationale
- Confirms the Emacs surface can discover and invoke the CLI, reducing setup friction and providing a clear installation verification step.
- Establishes reliable subprocess management that later stories can build upon.

## Acceptance Criteria (Given/When/Then)
- Given the package is installed, When I run `M-x duet-rpc-version`, Then it displays the `duet-rpc` CLI version (via invoking the binary) and exits cleanly.
- Given the package is installed, When I run `M-x duet-rpc-health`, Then it starts a subprocess if needed (auto-start) and shows "connected/ping OK" in the minibuffer (initially basic ping; evolves to full diagnostics in Story 007).
- Given a missing or misconfigured CLI, When I run either command, Then I see a clear, actionable error with guidance to configure the binary path.
- Given the package is loaded, When I open the command palette/commands list, Then entries exist to start, stop, refresh, and check the subprocess.
- Given the subprocess is already running, When I invoke health, Then it reuses the running process and returns within a short, documented timeout.
- Given I attempt to stop the subprocess, When I run `M-x duet-rpc-stop`, Then it asks for confirmation and reports `Stopped.` or `Canceled.` in the minibuffer accordingly.
- Given the CLI is missing or not executable, When I invoke version/health/start, Then I get a concise minibuffer message that suggests `M-x duet-rpc-locate-cli` and references `*DUET Logs*` for details; details are appended to `*DUET Logs*`.
- Given a health check exceeds the default timeout (3s), When I run `M-x duet-rpc-health`, Then it reports a timeout in the minibuffer and appends details (including elapsed time) to `*DUET Logs*`.
- Given errors or logs are produced, Then `*DUET Logs*` never auto-opens and error messages reference it for details.
- Given I am in the `*DUET RPC*` control buffer, When I press `?`, Then the `duet-dispatch` menu opens.
- Given I run `M-x duet-refresh`, Then it performs a quick ping and status check without full diagnostics.
- Given buffer size limits, Then `*DUET RPC*` truncates at 2,000 lines and `*DUET Logs*` truncates at 20,000 lines (oldest entries dropped).

## UI Surfaces and Interactions (per UX)
- Control Buffer: A dedicated `*DUET RPC*` control buffer opens when `duet-rpc-start` is invoked and shows a short status header (e.g., Running/Not running; PID if running). This buffer does not collect logs.
- Log Buffer: A separate append-only `*DUET Logs*` buffer captures logs and error details for troubleshooting; it does not auto-open.
- Transient Menu: `duet-dispatch` opens from the control buffer (e.g., `?`) and provides:
  - Process: `s` Start, `x` Stop (confirm)
  - Health: `h` Health, `v` Version
  - Config: `l` Locate CLI, `c` Customize
  - View: `L` Open logs (`*DUET Logs*`), `r` Refresh status
  - `q` Close menu; `?` Help
- Error Handling: Success and error messages are concise in the minibuffer; full error details (timeouts, missing CLI, permissions) are appended to `*DUET Logs*`. Missing CLI messages include next steps and reference to `duet-rpc-locate-cli`.

## Assumptions / Open Questions
- Assumption: The CLI provides a `rpc --ping` or equivalent for health checks — Confidence: high — Impact if wrong: need a fallback check — Validation: align with CLI story for ping.
- Assumption: Emacs can locate the CLI via PATH or a configurable variable (e.g., `duet-rpc-cli-path`) — Confidence: high — Impact: user setup friction — Validation: document default + override.
- Open question: Preferred names for start/stop/health commands (e.g., `duet-rpc-start`, `duet-rpc-stop`, `duet-rpc-health`)? → UX specifies these names and adds `duet-rpc-version`, `duet-rpc-locate-cli`, and `duet-dispatch`.

## Defaults & Config
- defgroup: `duet-rpc`
- defcustom: `duet-rpc-cli-path` (file path; nil → use PATH)
- defcustom: `duet-rpc-health-timeout` (number; default 3 seconds)
- Helper command: `M-x duet-rpc-customize` opens the Customize group.

## Success Metrics
- `M-x duet-rpc-version` and `M-x duet-rpc-health` verified in manual smoke checks across typical platforms.
- Clear error surfaced and recovery guidance when CLI missing/unavailable.
- Control/log buffers behave as specified: control opens on start; logs do not auto-open; transient menu exposes actions; minibuffer copy concise, logs capture details.
