# Architecture Overview

This document outlines a high-level, three-piece architecture and the responsibilities of each component.

## 1) Emacs UI (ELisp)
- Responsibilities: Editor UX, keybindings, command palette, context capture (region/buffer/file/project), status/progress display, diff review, approval flow, and applying accepted patches to buffers/files.
- Process Lifecycle: Spawn/monitor Codex CLI as a long-lived subprocess; handle version handshake and graceful restarts.
- Configuration: Surface project/global settings (model, presets, ignores) and quick toggles; store lightweight UI preferences.

## 2) Codex CLI Core (Stdio RPC)
- Responsibilities: Session orchestration, prompt assembly, context building (search, globs, .gitignore), provider calls (streaming), token/rate tracking, diff/patch generation, and safety checks.
- Tools & Feedback: Run tests/linters/build commands; capture errors and feed them back into prompts.
- Persistence: Manage session transcripts, caches, and configuration resolution (global → project) without writing source files directly.
- Interface: JSON-RPC over stdio with streaming events (status/tokens/patch proposals/tool output). One-shot commands also supported for scripts/CI.

## 3) Optional Adapters/Services (Future)
- MCP Adapter: Expose selected tools/providers via MCP for cross-client integrations; reuse CLI logic behind a standard interface.
- Local Daemon/Socket: Optional background service to share caches/indexes across editor instances; not required for v1.
- HTTP/gRPC Endpoint: For multi-app or remote workflows; introduces security and ops overhead—defer unless needed.

---

Notes
- v1 targets Emacs UI + Codex CLI over stdio; no network server required.
- CLI proposes patches; Emacs applies edits after user approval (dry-run by default, allowlist paths, size/file caps).
