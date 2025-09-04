# Architecture Overview

This document outlines a high-level, three-piece architecture and the responsibilities of each component.

## 1) Emacs UI (ELisp)
- Responsibilities: Editor UX, keybindings, command palette, context capture (region/buffer/file/project), status/progress display, diff review, approval flow, and applying accepted patches to buffers/files.
- Process Lifecycle: Spawn/monitor duet-rpc as a long-lived subprocess; handle version handshake and graceful restarts.
- Configuration: Surface project/global settings (model, presets, ignores) and quick toggles; store lightweight UI preferences.

## 2) RPC CLI Core (duet-rpc, Stdio RPC)
- Responsibilities: Session orchestration, prompt assembly, context building (search, globs, .gitignore), provider calls (streaming), token/rate tracking, diff/patch generation, and safety checks.
- Tools & Feedback: Run tests/linters/build commands; capture errors and feed them back into prompts.
- Persistence: Manage session transcripts, caches, and configuration resolution (global → project) without writing source files directly.
- Interface: JSON-RPC over stdio with streaming events (status/tokens/patch proposals/tool output). One-shot commands also supported for scripts/CI.

## 3) Optional Adapters/Services (Future)
- MCP Adapter: Expose selected tools/providers via MCP for cross-client integrations; reuse CLI logic behind a standard interface.
- Local Daemon/Socket: Optional background service to share caches/indexes across editor instances; not required for v1.
- HTTP/gRPC Endpoint: For multi-app or remote workflows; introduces security and ops overhead—defer unless needed.

## 4) Error Handling & Recovery
- Heartbeat: Periodic keepalive messages between Emacs and duet-rpc (every 30s); detect stalled processes.
- Reconnection: Automatic restart with exponential backoff (1s, 2s, 4s... max 30s) on duet-rpc crash.
- Request Queue: Buffer pending requests during disconnection; replay on reconnect or timeout after 60s.
- Graceful Degradation: Show clear status when duet-rpc unavailable; allow read-only operations and manual recovery.

## 5) Observability
- Structured Logging: Support debug/info/warn/error levels with consistent format; configurable per-component (Emacs UI, duet-rpc).

## 6) Security & Trust Boundaries
- Input Sanitization: Validate file paths and commands; prevent directory traversal and command injection.
- Secrets Handling: API keys never logged or cached; use secure environment variables or keychain integration.

---

Notes
- Naming: `Claude Code` refers to Anthropic's CLI; `duet-rpc` is our RPC/CLI binary used by Emacs and for one-shots.
- v1 targets Emacs UI + duet-rpc over stdio; no network server required.
- CLI proposes patches; Emacs applies edits after user approval (dry-run by default, allowlist paths, size/file caps).
