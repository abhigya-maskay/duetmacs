# Epics: AI IDE (Vertical Slices: UI + CLI)

This document defines vertical-slice epics so that each delivers a testable, end-to-end feature via both Emacs UI and an equivalent CLI one-shot. Each epic lists goal, scope (split UI/CLI), dependencies, acceptance (must be verifiable from UI and CLI), and phase.

Note on naming
- codex: External OpenAI CLI tool (not this project).
- codex-rpc: Our RPC/CLI binary used by Emacs and for one-shots.

## Epic: Project Bootstrap (CLI + Emacs Plugin)
- Goal: Establish working skeletons for codex-rpc (our CLI) and the Emacs package with tooling, packaging, CI, and a minimal RPC handshake so both can run end-to-end in a no-op mode.
- UI Scope: Emacs package scaffolding (package metadata, init, keymap, command palette entry, config variables); subprocess management to launch CLI; version/health commands; basic minibuffer status; `*Codex RPC*` control buffer (opens on start, shows status/PID); `*Codex Logs*` append-only log buffer (does not auto-open); transient-style `codex-dispatch` menu (Start/Stop with confirm, Health/Version, Locate CLI/Customize, Open Logs/Refresh); install/build instructions.
- CLI Scope: CLI project scaffolding (binary: `codex-rpc`; language toolchain; entrypoint; subcommands: `version`, `doctor`, `rpc --ping`, `prompt --dry-run`); logging; config loading precedence; packaging/release scripts.
- Dependencies: None.
- Acceptance:
  - CLI: `codex-rpc --version` prints version; `codex-rpc doctor` checks env and prints a human-readable checklist, emits stable JSON with `--json`, respects `NO_COLOR`/non-TTY, and uses exit codes 0/1/2; `codex-rpc rpc --ping` responds with `pong` (text) or minimal JSON with `--json`, supports `--timeout <ms>` with exit code 124 on timeout; `codex-rpc prompt --file <f> --dry-run` returns a no-op response.
    - Help behavior: synopsis `codex-rpc [COMMAND] [OPTIONS]`; footer `See 'codex-rpc <command> --help' for more information.`; colorized when TTY and honoring `NO_COLOR`; plain when piped; output is newline-terminated.
    - Error handling: unknown subcommand/flag prints error + usage and exits with code 2; no stack traces.
    - Version authority: version string is sourced from the package manifest.
  - Emacs: Package loads; `M-x codex-rpc-version` shows codex-rpc version; command to start/stop codex-rpc subprocess; health check shows connected/ping OK; basic command palette entry present.
- CI: Lints/builds/tests run on PR; `codex-rpc doctor` included in smoke checks across the support matrix; release artifacts produced for CLI; packaging instructions validated.
- Phase: V1 Foundation.

## Epic: Chat-to-Patch (Single File, E2E)
- Goal: Minimal end-to-end chat that produces a single-file patch with review/apply and backups.
- UI Scope: Start/attach session; send region/buffer/file; stream status/tokens; single-file diff review; apply or rollback.
- CLI Scope: Stdio RPC and one-shot command; prompt assembly for region/buffer/file; single provider; single-file patch generation; safety checks/backups.
- Dependencies: None.
- Acceptance:
  - From Emacs: start chat, request an edit to the current file, see a diff, apply or rollback; status and token counters visible.
  - From CLI one-shot: pass file/region and prompt; receive a diff/patch; apply with backup created.
- Phase: V1 Foundation.

## Epic: Scoped Context & Templates
- Goal: Deterministic prompt assembly with selectable scopes and reusable templates/presets.
- UI Scope: Choose scope (region/buffer/files/project); select preset/template; preview assembled prompt components.
- CLI Scope: Context builder with .gitignore/excludes/globs/types; template/snippet expansion; deterministic assembly.
- Dependencies: Chat-to-Patch (uses assembled prompts to produce patches).
- Acceptance:
  - Emacs: user selects scope + preset; preview shows included files/snippets; resulting patch respects chosen scope.
  - CLI: one-shot displays assembled components (e.g., with --show-prompt) and generates consistent output given the same inputs.
- Phase: V1 Foundation.

## Epic: Safety & Controls (Vertical)
- Goal: Guardrails consistently applied across UI and CLI for writes and long operations.
- UI Scope: Dry-run toggle; write allowlist prompts; caps warnings for size/file count; clear truncation/summarization notices; backup/restore UX.
- CLI Scope: Enforce file/size caps; truncation with summaries; explicit --yes gates to write; automatic backups and restore path.
- Dependencies: Chat-to-Patch.
- Acceptance:
  - Any patch application (UI/CLI) requires explicit approval and respects caps; over-cap inputs are summarized with a clear path to proceed.
  - Backup/restore verified in both UI and CLI flows.
- Phase: V1 Foundation.

## Epic: Provider Switch & Token Budget
- Goal: Switch providers/models and track token usage with budget caps and basic chunking.
- UI Scope: Model/provider switcher; visible token usage and soft caps.
- CLI Scope: Provider abstraction; per-project defaults; counters; basic chunk/summarize long inputs.
- Dependencies: Chat-to-Patch.
- Acceptance:
  - Emacs and CLI can switch providers (where configured); token counters update during streaming; long inputs are chunked/summarized without errors.
- Phase: V1 Foundation (single provider + counters), V1.1 (multi-provider + chunking).

## Epic: Persistence & History
- Goal: Durable sessions that can be reopened, renamed/branched, and exported.
- UI Scope: Titles/tags, reopen/rename/branch; export transcripts.
- CLI Scope: Store/retrieve transcripts + metadata; list/open/continue sessions; export.
- Dependencies: Chat-to-Patch.
- Acceptance:
  - Sessions persist across restarts; listing/search (basic) and reopen work from UI and CLI; export produces a usable artifact.
- Phase: V1 Foundation (basic persistence), V1.1 (tags/search/export).

## Epic: Multi-file Patches & Review
- Goal: Accurate cross-file patch proposals with atomic apply and hunk-level controls.
- UI Scope: Multi-file diff viewer; per-hunk accept/reject; scratch/sandbox buffers; rollback.
- CLI Scope: Cross-file patch generation; correct paths/locations; atomic apply plan; backups.
- Dependencies: Chat-to-Patch; Safety & Controls.
- Acceptance:
  - From UI and CLI, propose a multi-file patch, review per hunk, apply atomically with backup/rollback verified.
- Phase: V1.1 Enhancements.

## Epic: Search & Discovery Context
- Goal: Supply lightweight, relevant context via ripgrep/grep and related-file heuristics.
- UI Scope: Show discovered files/snippets with include/exclude toggles; add selected items to prompt.
- CLI Scope: Ripgrep summaries; related-file lists; respect .gitignore and excludes; structured results for the context builder.
- Dependencies: Scoped Context & Templates.
- Acceptance:
  - UI and CLI display discovered context and allow inclusion; measurable prompt quality improvement on tasks referencing discovered files.
- Phase: V1.1 Enhancements.

## Epic: Test/Tool Quick Fix Loop
- Goal: Run tests/linters/build, capture failures, and generate targeted fixes fed back into prompts.
- UI Scope: Run commands with streamed logs and exit status; quick actions (fix error, explain, add tests).
- CLI Scope: Command execution with streaming; last failure capture/summary; injection into follow-up prompts.
- Dependencies: Chat-to-Patch; Scoped Context & Templates.
- Acceptance:
  - From UI and CLI, a failing test/build is summarized automatically; a "fix error" action produces a patch proposal.
- Phase: V1.1 Enhancements.

## Epic: Configuration & Extensibility
- Goal: Predictable configuration and lightweight customization.
- UI Scope: Discoverable commands and sensible keybindings; inline help/examples/troubleshooting; settings surfaces (global/project).
- CLI Scope: Config precedence (global → project) resolved predictably; hooks (pre/post request, result filters, custom tools) with documented ordering.
- Dependencies: Chat-to-Patch.
- Acceptance:
  - Configured defaults are honored in both UI and CLI; hooks run and failures surface gracefully; commands are discoverable with inline help.
- Phase: V1 Foundation.

## Epic: Optional Adapters & Services (Deferred)
- Goal: Open integration paths post-v1 without complicating the core.
- Scope: MCP adapter; optional local daemon/socket for shared caches; optional HTTP/gRPC endpoint.
- Dependencies: Mature CLI core.
- Acceptance:
  - MCP adapter exposes a useful subset of tools/providers.
  - Local daemon improves cache hit rates across sessions; opt-in and secure.
  - Remote endpoint documented with security posture; not required for v1.
- Phase: V2 Options.

---

## Phasing Summary
- V1 Foundation: Project Bootstrap (CLI + Emacs Plugin), Chat-to-Patch (Single File), Scoped Context & Templates, Safety & Controls (Vertical), Provider Switch & Token Budget (single provider + counters), Persistence (basic), Configuration & Extensibility.
- V1.1 Enhancements: Test/Tool Quick Fix Loop, Multi-file Patches & Review, Search & Discovery Context, Provider multi-switch + chunking, Persistence tags/search/export.
- V2 Options: Adapters/services (MCP adapter, shared-cache daemon, HTTP/gRPC endpoint; advanced discovery/indexing).
