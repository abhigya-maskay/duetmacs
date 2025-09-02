# Story 002: Emacs Init and Health Check
UX need: Standard (multi-step, light recovery)

Overview
- Surface basic health/version commands with predictable feedback.
- Provide a dedicated control surface and a separate log surface.

Touchpoints (M-x)
- codex-rpc-version: Show CLI version in minibuffer.
- codex-rpc-health: Start/reuse process, ping, report in minibuffer.
- codex-rpc-start: Start subprocess and open control buffer.
- codex-rpc-stop: Stop subprocess (ask for confirmation).
- codex-rpc-locate-cli: Prompt to set CLI path; validate and confirm.
- codex-dispatch: Open transient-style menu in the control buffer (Magit-like).

Buffers
- *Codex RPC* (control):
  - Opens automatically when a Codex session starts.
  - Shows a short status header (e.g., Running/Not running; PID if running).
  - In this buffer, pressing "?" opens the `codex-dispatch` menu.
  - Does not collect logs/errors; those go to *Codex Logs*.
- *Codex Logs* (logs):
  - Append-only logs and error details for troubleshooting.
  - Never auto-opens; users open it via the menu or standard buffer switching.

Transient Menu (codex-dispatch)
- Process: s Start, x Stop (confirm)
- Health: h Health, v Version
- Config: l Locate CLI, c Customize
- View: L Open logs (*Codex Logs*), r Refresh status
- q Close menu; ? Help

Flows (happy path and Alt/Recovery)
- Version
  - Entry: M-x codex-rpc-version
  - Success: Minibuffer → "codex-rpc vX.Y.Z"
  - Alt: Missing CLI → Minibuffer → "Codex CLI not found. Set `codex-rpc-cli-path` or ensure `codex-rpc` on PATH. Run M-x codex-rpc-locate-cli. See *Codex Logs*."; details appended to *Codex Logs*.

- Health
  - Entry: M-x codex-rpc-health (starts process if needed)
  - Loading: Minibuffer → "Checking Codex RPC…"
  - Success: Minibuffer → "Connected: ping OK (N ms)"
  - Alt: Timeout → Minibuffer → "Health check timed out after 3s; see *Codex Logs*"; details appended to *Codex Logs*.
  - Alt: Missing CLI/permission errors → concise minibuffer + full details to *Codex Logs*.

- Start
  - Entry: M-x codex-rpc-start
  - Loading: Minibuffer → "Starting Codex RPC…"
  - Success: Minibuffer → "Started (PID NNNN)"; open *Codex RPC*; header shows Running + PID.
  - Alt: Already running → Minibuffer → "Codex RPC already running (PID NNNN)".
  - Errors: concise minibuffer + details to *Codex Logs*.

- Stop (confirmation)
  - Entry: M-x codex-rpc-stop
  - Confirm: Minibuffer → "Stop Codex RPC process PID NNNN? (y/n)"
  - Success: Minibuffer → "Stopped."
  - Alt: Canceled → Minibuffer → "Canceled."
  - Alt: Not running → Minibuffer → "No Codex RPC process is running."

- Locate CLI
  - Entry: M-x codex-rpc-locate-cli
  - Prompt for path (with completion); validate executable.
  - Success: Minibuffer → "CLI path set to …"; optionally offer to save via Customize.
  - Alt: Invalid path → concise minibuffer + details to *Codex Logs*.

States & Microcopy
- States: Not running, Starting, Running (PID), Stopping, Error.
- Minibuffer copy is concise and action-oriented; errors include next step and "see *Codex Logs*" when details exist.
- Header in *Codex RPC*: "Codex RPC: [Running PID NNNN | Not running]. Press ? for menu."

Defaults & Config (Customize)
- defgroup: codex-rpc
- defcustom: codex-rpc-cli-path (file path; nil → use PATH)
- defcustom: codex-rpc-health-timeout (number; default 3 seconds)
- Helper: M-x codex-rpc-customize opens the Customize group.

Validation Checklist
- Commands exist and are discoverable via M-x and the *Codex RPC* menu.
- *Codex RPC* opens on start; *Codex Logs* captures errors/timeouts and does not auto-open.
- Minibuffer shows succinct success/error; logs record details.
- Stop requires confirmation.
- Missing CLI errors provide clear guidance and a path to recovery (locate CLI).

Notes
- Menu uses a transient-style interface similar to Magit for familiarity.
- Avoid auto-opening *Codex Logs* to prevent context switching; always hint users to it on errors.
