# Component Map

## Overview
- Feature/Context: CLI Version/Help Bootstrap (Story 001)
- Scope date: 2025-01-08
- Author: Component Designer

## Components
- CLIParser: Purpose — Parse command-line arguments and route to subcommands. Boundary — Application entry. Key deps — HelpFormatter, ErrorHandler.
- VersionManager: Purpose — Manage version information from package manifest. Boundary — Domain version handling. Key deps — None (reads manifest).
- HelpFormatter: Purpose — Generate and format help text for commands. Boundary — Presentation formatting. Key deps — OutputFormatter.
- OutputFormatter: Purpose — Handle terminal output with color and symbol management. Boundary — Infrastructure I/O. Key deps — None (system calls).
- Logger: Purpose — Provide structured logging with level control. Boundary — Infrastructure logging. Key deps — OutputFormatter (for stderr).
- ConfigLoader: Purpose — Load and merge configuration from multiple sources. Boundary — Infrastructure config. Key deps — Logger.
- ErrorHandler: Purpose — Format errors and manage exit codes consistently. Boundary — Application error handling. Key deps — OutputFormatter.

## Notes
- Decisions:
  - CLI framework: optparse-applicative with showHelpOnEmpty/showHelpOnError.
  - Version: embed via Cabal Paths (package version only in Story 001).
  - Output: ansi-terminal; cache TTY detection; honor NO_COLOR/--no-color; DUET_RPC_ASCII override.
  - Logging: katip; stderr scribe; default level warn; no built-in rotation in Story 001.
  - Config: TOML (tomland); precedence flags > env > project .duet-rpc.toml > user ~/.config/duet-rpc/config.toml > defaults; XDG via directory; env-only credentials; validation deferred to Story 003.
  - Errors: exit codes 0 success, 2 usage, 1 runtime; no stack traces in normal mode.
- Assumptions: CLI framework provides base help structure.
