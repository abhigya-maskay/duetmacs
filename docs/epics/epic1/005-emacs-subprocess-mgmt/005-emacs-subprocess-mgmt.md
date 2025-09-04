# Story 005: Emacs Subprocess Management

As an Emacs user, I want commands to start and stop the `duet-rpc` subprocess with clear status so that I can control the background process reliably.

## Priority
- Must

## Dependencies
- Story 004 (CLI RPC Ping) for health signaling

## Non-Goals
- Chat/edit features or streaming
- Prompt assembly or advanced UI beyond minibuffer/messages

## Business Rationale
- Robust process lifecycle is foundational for all subsequent Emacs features and reduces support overhead.

## Acceptance Criteria (Given/When/Then)
- Given the package is installed, When I run `M-x duet-rpc-start`, Then it launches the subprocess, captures logs to a buffer, and sets internal state to "running".
- Given the subprocess is running, When I run `M-x duet-rpc-stop`, Then it terminates the process gracefully, cleans up state, and closes or preserves logs per configuration.
- Given the subprocess state, When I run `M-x duet-rpc-status`, Then it shows whether the process is running, the PID/port if applicable, and the last ping result.
- Given a start attempt while already running, When I run `M-x duet-rpc-start` again, Then it does not start a duplicate and instead surfaces status with an option to restart.
- Given a stop attempt when not running, When I run `M-x duet-rpc-stop`, Then it reports that no process is active and exits cleanly.

## Assumptions / Open Questions
- Assumption: Logs are written to a dedicated buffer (e.g., `*duet-rpc*`) — Confidence: high — Impact: debugging ease — Validation: confirm UX preference.
- Open question: Should the subprocess be a transient per-request process or a persistent daemon during Emacs sessions? Initial assumption: persistent with explicit start/stop.

## Success Metrics
- Start/stop/status commands behave reliably across platforms with minimal flakes.

