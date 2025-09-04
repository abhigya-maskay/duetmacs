# Story 002: Emacs Init and Health Check
UX need: Standard (multi-step, light recovery)

Overview
- Surface basic health/version commands with predictable feedback.
- Provide a dedicated control surface and a separate log surface.

Touchpoints (M-x)
- duet-rpc-version: Show CLI version in minibuffer.
- duet-rpc-health: Start/reuse process, ping, report in minibuffer.
- duet-rpc-start: Start subprocess and open control buffer.
- duet-rpc-stop: Stop subprocess (ask for confirmation).
- duet-rpc-locate-cli: Prompt to set CLI path; validate and confirm.
- duet-dispatch: Open transient-style menu in the control buffer (Magit-like).

Buffers
- *DUET RPC* (control):
  - Opens automatically when a DUET session starts.
  - Shows a short status header (e.g., Running/Not running; PID if running).
  - In this buffer, pressing "?" opens the `duet-dispatch` menu.
  - Does not collect logs/errors; those go to *DUET Logs*.
- *DUET Logs* (logs):
  - Append-only logs and error details for troubleshooting.
  - Never auto-opens; users open it via the menu or standard buffer switching.

Transient Menu (duet-dispatch)
- Process: s Start, x Stop (confirm)
- Health: h Health, v Version
- Config: l Locate CLI, c Customize
- View: L Open logs (*DUET Logs*), r Refresh status
- q Close menu; ? Help

Flows (happy path and Alt/Recovery)
- Version
  - Entry: M-x duet-rpc-version
  - Success: Minibuffer → "duet-rpc vX.Y.Z"
  - Alt: Missing CLI → Minibuffer → "DUET CLI not found. Set `duet-rpc-cli-path` or ensure `duet-rpc` on PATH. Run M-x duet-rpc-locate-cli. See *DUET Logs*."; details appended to *DUET Logs*.

- Health
  - Entry: M-x duet-rpc-health (starts process if needed)
  - Loading: Minibuffer → "Checking DUET RPC…"
  - Success: Minibuffer → "Connected: ping OK (N ms)"
  - Alt: Timeout → Minibuffer → "Health check timed out after 3s; see *DUET Logs*"; details appended to *DUET Logs*.
  - Alt: Missing CLI/permission errors → concise minibuffer + full details to *DUET Logs*.

- Start
  - Entry: M-x duet-rpc-start
  - Loading: Minibuffer → "Starting DUET RPC…"
  - Success: Minibuffer → "Started (PID NNNN)"; open *DUET RPC*; header shows Running + PID.
  - Alt: Already running → Minibuffer → "DUET RPC already running (PID NNNN)".
  - Errors: concise minibuffer + details to *DUET Logs*.

- Stop (confirmation)
  - Entry: M-x duet-rpc-stop
  - Confirm: Minibuffer → "Stop DUET RPC process PID NNNN? (y/n)"
  - Success: Minibuffer → "Stopped."
  - Alt: Canceled → Minibuffer → "Canceled."
  - Alt: Not running → Minibuffer → "No DUET RPC process is running."

- Locate CLI
  - Entry: M-x duet-rpc-locate-cli
  - Prompt for path (with completion); validate executable.
  - Success: Minibuffer → "CLI path set to …"; optionally offer to save via Customize.
  - Alt: Invalid path → concise minibuffer + details to *DUET Logs*.

States & Microcopy
- States: Not running, Starting, Running (PID), Stopping, Error.
- Minibuffer copy is concise and action-oriented; errors include next step and "see *DUET Logs*" when details exist.
- Header in *DUET RPC*: "DUET RPC: [Running PID NNNN | Not running]. Press ? for menu."

Defaults & Config (Customize)
- defgroup: duet-rpc
- defcustom: duet-rpc-cli-path (file path; nil → use PATH)
- defcustom: duet-rpc-health-timeout (number; default 3 seconds)
- Helper: M-x duet-rpc-customize opens the Customize group.

Validation Checklist
- Commands exist and are discoverable via M-x and the *DUET RPC* menu.
- *DUET RPC* opens on start; *DUET Logs* captures errors/timeouts and does not auto-open.
- Minibuffer shows succinct success/error; logs record details.
- Stop requires confirmation.
- Missing CLI errors provide clear guidance and a path to recovery (locate CLI).

Notes
- Menu uses a transient-style interface similar to Magit for familiarity.
- Avoid auto-opening *DUET Logs* to prevent context switching; always hint users to it on errors.
