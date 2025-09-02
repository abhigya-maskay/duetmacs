# Story 007: Emacs Commands Palette

As an Emacs user, I want discoverable commands for version, health, start, stop, and status so that I can access core functions via M-x and keybindings.

## Priority
- Must

## Dependencies
- Story 002 (Emacs Init and Health Check)
- Story 005 (Emacs Subprocess Management)

## Non-Goals
- Custom UI buffers beyond minibuffer messages
- Icons/themes or complex palette UIs
- Extensive keybinding presets beyond a minimal default map

## Business Rationale
- Improves discoverability and lowers the barrier for first-time users.

## Acceptance Criteria (Given/When/Then)
- Given the package is installed, When I invoke `M-x` and type `codex-`, Then I see commands: `codex-rpc-version`, `codex-rpc-health`, `codex-rpc-start`, `codex-rpc-stop`, and `codex-rpc-status`.
- Given help is requested, When I run `C-h f` on any command, Then I see concise docstrings with usage and caveats.
- Given a default minor mode, When I enable it, Then a minimal keymap is available with sensible bindings (documented in README) for start/stop/status.
- Given commands are listed, When I search built-in help, Then the package is discoverable with a short overview group.

## Assumptions / Open Questions
- Assumption: A minor mode (e.g., `codex-rpc-mode`) provides the default keymap — Confidence: high — Impact: consistent UX — Validation: document and implement.
- Open question: Preferred default keybindings; ensure no conflicts with popular modes.

## Success Metrics
- Commands appear in `M-x` and help with stable names and docstrings.

