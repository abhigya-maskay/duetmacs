# Story 004: CLI RPC Ping

As a CLI user, I want `codex-rpc rpc --ping` to perform a minimal RPC handshake and return a success response so that Emacs and scripts can verify connectivity.

## Priority
- Must

## Dependencies
- Story 001 (CLI Version/Help Bootstrap)

## Non-Goals
- Any LLM/provider calls
- Streaming or long-running sessions
- Prompt assembly or context building
- Emacs UI

## Business Rationale
- Establishes a reliable, scriptable connectivity check used by Emacs health and CI smoke tests.

## Acceptance Criteria (Given/When/Then)
- Given the binary is installed, When I run `codex-rpc rpc --ping`, Then it returns exit code 0 and prints `pong` (with trailing newline) to stdout.
- Given the `--json` flag, When I run `codex-rpc rpc --ping --json`, Then it prints minimal JSON to stdout including `status: "ok"`, `version`, and an ISO-8601 UTC `timestamp`, and exits 0.
- Given a `--timeout <ms>` flag, When I run `codex-rpc rpc --ping --timeout 500`, Then the command respects the timeout and returns exit code 124 with the message `timeout exceeded after 500 ms` written to stderr; with `--json`, it instead outputs a JSON error object `{ "status": "error", "message": "timeout exceeded after 500 ms" }`.
- Given other errors (e.g., handshake/transport failure), When the command fails, Then it exits non-zero (e.g., 1) and prints a concise error to stderr; with `--json`, it prints `{ "status": "error", "message": "<cause>" }`.
- Given unexpected flags or input, When the command is invoked incorrectly, Then it prints helpful usage to stderr and exits non-zero.

## Assumptions / Open Questions
- Assumption: The ping uses the same code path Emacs will use for subprocess handshake — Confidence: high — Impact: duplicated code — Validation: factor shared handshake.
- Open question: Preferred output format default (plain text `pong` vs JSON). Emacs can parse both; default likely plain text for ergonomics.

## Success Metrics
- Used reliably by Emacs health checks and CI smoke tests without flakes.
