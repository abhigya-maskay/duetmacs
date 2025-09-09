# CLIParser

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Parse command-line arguments, validate input, and route to appropriate subcommands or handlers.

## Responsibilities
- Parse global flags (--version, --help, --log-level, --no-color)
- Identify and route to subcommands (version, doctor, rpc, prompt)
- Validate argument structure and generate usage errors

## Boundaries
- In-scope: Argument parsing, command routing, global flag extraction
- Out-of-scope: Command execution, output formatting, error message styling
- Inputs (conceptual): Raw command-line arguments array
- Outputs (conceptual): Parsed command structure, validation errors

## Collaborators & Dependencies
- Internal: HelpFormatter (to display help), ErrorHandler (for invalid input)
- External: optparse-applicative (CLI framework)
- Notes: Must handle both long and short flag formats; delegates execution to command handlers; enable showHelpOnEmpty and showHelpOnError; global flags: --version, --help, --log-level, --no-color; Story 001 implements `version` subcommand and global help; `doctor`, `rpc`, and `prompt` stubbed with "Not yet implemented" messages.

## Risks & Open Questions
- Decision: Use optparse-applicative; rely on its built-in per-command help handling.
- Note: Subcommand help routing handled by framework; minimal custom logic needed in Story 001.
