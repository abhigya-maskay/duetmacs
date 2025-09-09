# HelpFormatter

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Generate structured help text with consistent formatting for all commands and subcommands.

## Responsibilities
- Format command synopsis with proper structure
- List subcommands with one-line descriptions
- Generate contextual help footers with navigation hints

## Boundaries
- In-scope: Help text structure, command descriptions, usage patterns
- Out-of-scope: Color application, terminal detection, actual rendering
- Inputs (conceptual): Command metadata, available subcommands, flags
- Outputs (conceptual): Structured help text ready for formatting

## Collaborators & Dependencies
- Internal: OutputFormatter (for final rendering with colors)
- External: Command registry or metadata; optparse-applicative (built-in help)
- Notes: Use optparse-applicative defaults; enable showHelpOnEmpty and showHelpOnError; set header/progDesc/footerDoc for consistent synopsis/hints; apply color via OutputFormatter only when TTY and color-enabled.

## Risks & Open Questions
- Decision: Stick to optparse-applicative default help structure for Story 001.
- Decision: English-only for Story 001; defer i18n to a later story.
