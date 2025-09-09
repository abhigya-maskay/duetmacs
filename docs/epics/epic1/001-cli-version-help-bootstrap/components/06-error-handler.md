# ErrorHandler

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Provide consistent error formatting and exit code management across all CLI operations.

## Responsibilities
- Format error messages for user-friendly display
- Manage exit codes (0 success, 2 usage/validation errors, 1 runtime/unknown)
- Suppress stack traces in production mode
- Display usage hints on command errors
- Handle subprocess spawn failures with retry logic (3 attempts, 1s delay)
- Provide clear spawn failure diagnostics without exposing system internals

## Boundaries
- In-scope: Error formatting, exit code standards, usage hint generation, spawn retry logic
- Out-of-scope: General error recovery (beyond spawn), detailed system diagnostics
- Inputs (conceptual): Error type, context, command state
- Outputs (conceptual): Formatted error message, exit code

## Collaborators & Dependencies
- Internal: OutputFormatter (for error styling), HelpFormatter (for usage hints)
- External: Process exit APIs
- Notes: Ensures consistent error experience; no stack traces for user errors; for usage errors print parser diagnostics and "Use --help" hint; in debug log-level include exception details.

## Risks & Open Questions
- Decision: Exit codes 0/2/1 mapping confirmed for Story 001.
- Decision: No stack traces in normal mode; include details only in debug mode via logs.
- Decision: English-only error messages in Story 001; i18n later.
