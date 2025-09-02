# Story 006: CLI Prompt Dry-Run

As a CLI user, I want `codex-rpc prompt --file <path> --dry-run` to accept inputs and return a deterministic no-op response so that I can validate end-to-end wiring without making edits.

## Priority
- Must

## Dependencies
- Story 001 (CLI Version/Help Bootstrap)

## Non-Goals
- Real model/provider calls
- Patch generation or file writes
- Context assembly or safety caps beyond basic input validation

## Business Rationale
- Enables smoke tests and scripting of the full path from input parsing to output formatting without risking file modifications.

## Acceptance Criteria (Given/When/Then)
- Given a readable file path, When I run `codex-rpc prompt --file README.md --dry-run`, Then it returns exit code 0 and prints a response indicating no-op with echoed inputs (file path, size, and content hash).
- Given `--json`, When I run the command, Then the output is a stable JSON structure containing fields like `mode: "dry-run"`, `file`, `size_bytes`, `sha256`, and `received_at`.
- Given missing or unreadable input, When I run the command, Then it exits non-zero with a clear error message and usage hint.
- Given extra flags like `--provider` or `--model`, When I run with `--dry-run`, Then they are accepted but ignored, with a notice that this is a no-op.

## Assumptions / Open Questions
- Assumption: File hashing uses SHA-256 — Confidence: high — Impact: deterministic validation — Validation: document and test in CI.
- Open question: Should `--stdin` be supported for dry-run as parity with future real prompt modes? Initial scope: optional.

## Success Metrics
- Dry-run behaves deterministically across platforms and is used in CI smoke tests.

