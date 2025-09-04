# Story 007: Emacs Commands Palette — UX Spec

Scope: Discoverable `M-x codex-*` commands and a minimal, Doom/Evil-first keybinding surface. Use two buffers for visibility and diagnostics without introducing custom UIs.

## Buffers
- *Codex RPC* (control):
  - Purpose: concise, timestamped one-line summaries for user-visible outcomes.
  - Timestamp: local `[HH:MM:SS]`.
  - Auto-open: on `start` only; selects the buffer via `pop-to-buffer` (respects `display-buffer` rules). Background append for other commands.
  - Auto-scroll: disabled (do not follow tail).
  - Truncation: keep last 2,000 lines (drop oldest on append).
  - Errors: on failure/crash, focus this buffer and prompt: `Open logs? (y/N)`.
- *Codex Logs* (verbose):
  - Purpose: detailed diagnostics (e.g., `doctor` output, subprocess stderr/stdout).
  - Auto-open: never (only on user action or upon error prompt acceptance).
  - Truncation: keep last 20,000 lines (drop oldest on append).

## Commands and Feedback
- `codex-rpc-start`:
  - Behavior: if not running, launch process; append one-liner. If already running, prompt to restart (stop → start); default No.
  - One-liner examples:
    - `[12:03:14] start: launched PID 12345 (v1.2.3) in 180ms`
    - `[12:03:14] start: already running (PID 12345) — restart?`
  - On failure: append error summary to *Codex RPC*; prompt to open logs.
- `codex-rpc-stop`:
  - Behavior: confirm stop (default No). Send TERM, wait 2s; if still running, prompt `Force kill? (y/N)`; on Yes, send KILL and report.
  - One-liner examples:
    - `[12:04:22] stop: sent TERM to PID 12345`
    - `[12:04:24] stop: exited cleanly (2.0s)` / `[12:04:24] stop: force-killed after timeout`
- `codex-rpc-status`:
  - Fields: state (running/stopped), PID, uptime, CLI version, binary path.
  - One-liner examples:
    - `[12:05:01] status: running PID 12345, up 1h02m, v1.2.3 /usr/local/bin/codex-rpc`
    - `[12:05:01] status: stopped`
- `codex-rpc-health` (doctor):
  - Default: run full diagnostics (`doctor`). Append concise summary to *Codex RPC*; write detailed output to *Codex Logs*.
  - One-liner examples:
    - `[12:06:10] health: OK (7 checks, 0 warnings, 0 errors, 350ms)`
    - `[12:06:10] health: 2 errors detected — see logs`
- `codex-rpc-version`:
  - One-liner example: `[12:07:33] version: v1.2.3 /usr/local/bin/codex-rpc`
- `codex-refresh` (fast refresh):
  - Behavior: quick ping + status; append one-line summary; do not write to logs.
  - One-liner example: `[12:08:12] refresh: running PID 12345, ping 42ms`

Minibuffer: For all commands, also show a brief `message` mirroring the one-liner result (success/error) without stealing focus, except `start` which opens/selects *Codex RPC*.

## Keybindings (Doom/Evil only)
- Conditional: Define only when Evil/Doom leader is available; no default Emacs bindings.
- Leader prefix: `SPC L`.
- Mappings:
  - `SPC L d` → `codex-dispatch`
  - `SPC L s` → `codex-rpc-start`
  - `SPC L x` → `codex-rpc-stop`
  - `SPC L t` → `codex-rpc-status`
  - `SPC L h` → `codex-rpc-health`
  - `SPC L v` → `codex-rpc-version`
  - `SPC L r` → `codex-refresh`
  - `SPC L l` → open `*Codex Logs*`

## Dispatch Menu
- Command: `codex-dispatch`.
- Style: Use `transient` when available; fallback to `completing-read`.
- Actions: Start/Stop (with confirmations), Status, Health (doctor), Version, Refresh, Open Logs, Locate CLI, Customize.

## States and Indicators
- States: `starting`, `running`, `stopping`, `stopped`, `error`.
- Modeline: show simple indicator — `●` running / `○` stopped.

## Copy Guidelines (one-liners)
- Success: state the action, key result, and latency (when relevant).
- Errors: state issue + next step; common causes include binary missing, permission, port/bind failures.
- Prompts: yes/no prompts default to No to prevent accidental stops/kills; error prompt: `Open logs? (y/N)`.

## Metrics (GSM)
- Events (present tense, snake_case):
  - `rpc:process_start`, `rpc:process_stop`, `rpc:status_view`, `rpc:doctor_run`, `rpc:version_view`, `ui:dispatch_open`, `rpc:refresh_run`.
- Fields: timestamp, project root, result (ok/error), latency (ms), pid, version.

## Validation Checklist
- Commands discoverable via `M-x codex-*` with clear docstrings and help group.
- Doom/Evil mappings appear only when Evil/Doom present; no global Emacs defaults.
- *Codex RPC* appends timestamped lines; auto-opens on start; does not autoscroll; truncates at 2,000 lines.
- *Codex Logs* collects verbose output; never auto-opens; truncates at 20,000 lines.
- Stop flow confirms; TERM→2s wait→optional KILL with confirm; messages reflect outcome.
- Health writes summary to *Codex RPC* and details to *Codex Logs*; on failure, prompt to open logs.
 - Opening *Codex RPC* on start uses `pop-to-buffer` (respects `display-buffer` rules).
