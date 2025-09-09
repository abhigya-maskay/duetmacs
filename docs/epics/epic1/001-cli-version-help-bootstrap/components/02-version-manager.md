# VersionManager

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Provide single source of truth for application version information from package manifest.

## Responsibilities
- Read version from package manifest at build/runtime
- Format version string for display
- Ensure version consistency across all outputs

## Boundaries
- In-scope: Version retrieval, formatting, caching
- Out-of-scope: Version bumping, release management, output display
- Inputs (conceptual): Package manifest location
- Outputs (conceptual): Semantic version string

## Collaborators & Dependencies
- Internal: None
- External: Cabal build info (`Paths_duet_rpc.version`)
- Notes: Embed version via Cabal Paths module; expose a formatted helper used by CLI/daemon (e.g., "duet-rpc " <> showVersion).

## Risks & Open Questions
- Decision: For Story 001, --version shows package version only (no git SHA). Consider adding VCS metadata in a later story if needed.
