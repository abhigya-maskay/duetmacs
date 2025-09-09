# Logger

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Provide structured logging with configurable levels and output destinations for debugging and monitoring.

## Responsibilities
- Initialize logging with appropriate default level (warn)
- Route logs to stderr or file based on configuration
- Format log entries with timestamp, level, and message
- Respect --log-level flag and DUET_RPC_LOG environment variable

## Boundaries
- In-scope: Log level filtering, output routing, structured formatting
- Out-of-scope: Log aggregation, remote logging, performance profiling
- Inputs (conceptual): Log level, message, context data
- Outputs (conceptual): Formatted log entries to configured destination

## Collaborators & Dependencies
- Internal: OutputFormatter (for stderr color output)
- External: katip (structured logging), environment variables
- Notes: Use katip; default scribe stderr; default level warn; `--log-level` overrides `DUET_RPC_LOG`; guard expensive debug logs; no built-in rotation in Story 001 (rely on external rotation); consider file scribe later for daemon mode.

## Risks & Open Questions
- Decision: No built-in log rotation in Story 001; rely on external rotation; revisit with file scribe later.
- Decision: Stderr-only scribe in Story 001 (no file scribe yet).
- Risk: Performance impact of debug logging — mitigate by guarding construction of debug messages.
