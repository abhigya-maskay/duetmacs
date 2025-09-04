# Story 005: Emacs Subprocess Management — UX
UX need: Moderate

Scope
- Manage lifecycle of persistent `duet-rpc` daemon with clear status.
- Auto-start on first use; explicit `duet-rpc-start`/`duet-rpc-stop` supported.

Defaults
- Logs: keep `*duet-rpc*` buffer open on stop; append on next start; no file persistence by default.
- Status: surface primarily via minibuffer; optional modeline indicator.

Phases and Flows
- Start
  - Entry: user runs `duet-rpc-start` or auto-start triggered
  - Exit: Running or Error
  - Steps: spawn; show "Starting duet-rpc…"; open/reuse log buffer; ping until healthy; on success show "duet-rpc running (pid X, port Y)"; on timeout show error with next steps
- Stop
  - Entry: user runs `duet-rpc-stop`
  - Exit: Idle or Error
  - Steps: graceful stop; wait; clear state; keep log buffer; show "Stopped duet-rpc"
- Status
  - Entry: user runs `duet-rpc-status`
  - Exit: n/a
  - Steps: compute state; show "running (pid, port), last ping [OK/FAIL/STALE]" or "not running"
- Auto-start on demand
  - Entry: user invokes a dependent command
  - Exit: proceed to command or Error
  - Steps: if not running, show "Starting duet-rpc for this command…"; start; continue or surface error

States and Alt/Recovery
- Duplicate start
  - When running: prompt "duet-rpc is running (pid X, port Y). Restart? [y/N]" → default No; on Yes: stop then start
- Stop when idle
  - Show "duet-rpc is not running"
- Timeouts / start failure
  - Show "Failed to start duet-rpc (timeout). See *duet-rpc* log. Try again?"
- Health check failure / degraded
  - Show "duet-rpc running, ping failed. See log; consider restart."
- Interruptions
  - If process exits unexpectedly: notify "duet-rpc exited (code N). View logs? [y/N]"; open on Yes only

Microcopy
- Style: sentence case, active voice, present tense
- Progress: "Starting duet-rpc…", "Stopping duet-rpc…"
- Success: "duet-rpc running (pid X, port Y)", "Stopped duet-rpc"
- Errors: state issue + next step; e.g., "Failed to start duet-rpc (timeout). See *duet-rpc* log."
- Confirmations: state consequence and primary action; e.g., "Restart duet-rpc now? This will interrupt in-flight operations."

Modeline Indicator
- Default: show `duet-rpc` lighter when running; hide when idle
- Content: `RPC:✓` when healthy; `RPC:!` when ping failed; tooltip with pid/port
- Updates: refresh on start/stop/status and health changes

Metrics and Events
- GSM: Goal—reliable lifecycle; Signal—successful starts/stops, timely health checks; Metrics—start success rate, median start time, ping success rate
- Events: `rpc:start_attempt`, `rpc:start_success`, `rpc:start_timeout`, `rpc:stop_attempt`, `rpc:stop_success`, `rpc:status_view`, `rpc:health_ok`, `rpc:health_fail`, `rpc:unexpected_exit`
- Performance: start feedback within 200 ms; health ping within 1s; no-blocking UI

Data/Decisions and Config
- Health checks: ping interval 1s; failure threshold 2 consecutive failures → show degraded modeline (`RPC:!`)
- Timeouts: start timeout 10s; stop timeout 5s; surface timeouts with next-step guidance
- Restart prompt: default No; exact prompt "duet-rpc is running (pid X, port Y). Restart? [y/N]"
- Auto-start: enabled by default; triggered on first dependent command; show "Starting duet-rpc for this command…"
- Log buffer: name `*duet-rpc*`; preserve on stop; append on next start; do not persist to file
- Unexpected exit: notify with "[y/N]" to open logs; default No
- Modeline: enabled by default; `RPC:✓` healthy; `RPC:!` degraded
- Suggested variables (non-binding): `duet-rpc-auto-start`, `duet-rpc-start-timeout-seconds`, `duet-rpc-stop-timeout-seconds`, `duet-rpc-ping-interval-seconds`, `duet-rpc-modeline-enabled`, `duet-rpc-log-buffer-name`, `duet-rpc-notify-on-exit`

Validation Checklist
- Entry/Exit: start/stop/status and auto-start cover all outcomes
- States: loading/idle/running/degraded/error; duplicates; unexpected exit
- Copy: progress, success, error, confirmations follow Microcopy standards
- Metrics: events named; thresholds set; assumptions/risks noted
