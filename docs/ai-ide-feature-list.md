# AI IDE Package — Themed Feature List

## Chat & Sessions
- Chat + Code Agent: Interactive coding/chat sessions scoped to project or buffer.
- History Management: Persist sessions; reopen, rename, or branch conversations.
- Session Metadata: Titles/tags for organization; export transcripts.
- Status/Progress: Streaming outputs and progress in minibuffer/echo area.

## Context & Prompting
- Context Ingestion: Send region, buffer, files, or project subsets.
- Project Scopes: Restrict by root, subdir, globs, or file types.
- Structured Prompts: Presets for refactor, explain, tests, docs, review.
- Tooling Awareness: Include project type, deps, and commands as hints.
- Error-Aware Prompts: Include last build/test error automatically.
- Templates: Custom prompt templates and reusable instruction snippets.

## Editing & Diffs
- Edit Application: Apply AI-suggested patches with review gates.
- Diff Workflow: Show diffs; accept/reject by hunk or file.
- Cross-File Edits: Apply multi-file patches atomically.
- Code Blocks Handling: Parse fenced blocks; write to correct files/locations.
- Scratch/Sandbox Buffers: Stage edits before touching source files.

## Search & Discovery
- File Discovery: Auto-include related files (impl/tests/headers).
- Search Integration: Ripgrep/grep summaries to build lightweight context.
- File Ignore Rules: Respect .gitignore and user-defined excludes.

## Execution & Feedback Loops
- Inline Operations: Work on current region/buffer without leaving editor.
- Quick Actions: One-shots like fix error, explain, add tests.
- Test Loop Integration: Run tests/linters; feed results back into chat.

## Models & Tokens
- Multi-Model Support: Switch providers/models; per-project defaults.
- Rate/Token Controls: Track tokens; chunk or summarize long contexts.

## Safety & Controls
- Safety Guards: Dry-run mode, write protection, automatic backups.
- Safety Limits: Max files/size; truncate with summaries when exceeded.

## Configuration & Extensibility
- Configuration: Global/project settings for models, prompts, ignores.
- Command Palette: Discoverable commands and sensible keybindings.
- Extensibility: Hooks for pre/post request, result filters, custom tools.
- Documentation: Inline help, examples, troubleshooting.
