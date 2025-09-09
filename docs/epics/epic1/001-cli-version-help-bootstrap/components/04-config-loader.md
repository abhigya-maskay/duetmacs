# ConfigLoader

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Load and merge configuration from multiple sources with defined precedence rules.

## Responsibilities
- Define configuration search paths and precedence
- Load config from environment, project, and home locations
- Merge configurations with proper override semantics
- Provide default values for all settings

## Boundaries
- In-scope: Path resolution, file loading, config merging, defaults
- Out-of-scope: Config validation (deferred to Story 003), runtime updates, config writing
- Inputs (conceptual): Environment variables, config file paths
- Outputs (conceptual): Merged configuration structure

## Collaborators & Dependencies
- Internal: Logger (for config loading diagnostics)
- External: File system, environment variables, TOML parser (tomland), XDG path resolver (directory)
- Notes: Precedence — flags > env > project `.duet-rpc.toml` > user `~/.config/duet-rpc/config.toml` > defaults; resolve paths via XDG; log sources and overrides at info level; credentials from environment only (ignore and warn on file-stored secrets; always mask in logs). Skeleton only in Story 001; full validation in Story 003.

## Risks & Open Questions
- Decision: Use TOML (tomland) with tolerant parsing; unknown keys tolerated in Story 001.
- Decision: Defer strict schema validation to Story 003; consider adding optional top-level `version` then for migrations.
- Decision: Filenames/paths — `.duet-rpc.toml` (project), `~/.config/duet-rpc/config.toml` (user) confirmed.
- Risk: Schema evolution/backwards compatibility — mitigate with defaults and versioned validation later.
- Risk: Concurrent config file reads — Story 003 must implement sequential loading in deterministic order (env → project → home) and cache parsed config at startup to avoid re-reads during operation.
