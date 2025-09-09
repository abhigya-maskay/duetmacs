# Test Strategy — Test 1: CLI Version/Help Bootstrap

## Objectives
- Verify `duet-rpc` installs and responds with version/help consistently.
- Establish deterministic behavior for logging, color, output, and exit codes.
- Anchor future stories with a stable, testable CLI entrypoint.

## In Scope
- Commands: `--version`, `version`, `--help`, no-args help.
- Global options: `-V/--version`, `-h/--help`, `--log-level`, `--no-color`.
- Environment variables: `DUET_RPC_LOG`, `NO_COLOR`.
- Error handling for unknown subcommands/flags.
- Output format: newline-terminated, color rules, help structure.
- Performance target for version/help (~100ms typical hardware).

## Out of Scope
- Behavior for `doctor`, `rpc`, `prompt`.
- Full configuration loading/validation (skeleton only).
- Packaging, release, and editor integration.

## Inputs & References
- Epic: `docs/epics/epic1/001-cli-version-help-bootstrap/001-cli-version-help-bootstrap.md`
- UX: `docs/epics/epic1/001-cli-version-help-bootstrap/ux.md`
- Components: CLIParser, HelpFormatter, OutputFormatter, Logger, VersionManager, ErrorHandler, ConfigLoader.
- Architecture: `docs/architecture/duet-rpc/technical-architecture.md` (Haskell + Cabal; optparse-applicative; tasty/hedgehog).

## Approach
- Feature-first, risk-based coverage with P0 critical-path priority.
- Start with E2E command invocations; add integration/unit for determinism.
- Use golden snapshots for help text; regex-pattern assertions for logs and ANSI detection.
- Control environment (env vars, TTY vs pipe) to validate modes.

## Test Levels & Coverage Targets
- Unit: 70–80% on changed code; 100% on critical branches (VersionManager, ErrorHandler, CLI parser critical paths).
- Integration: 100% of P0 interfaces (CLI routing, help generation via optparse-applicative).
- E2E: 100% of P0 flows (version/help, error on unknown).
- Optional: Property tests (hedgehog) on parser invariants where valuable.

## Prioritization
- P0: Version/help correctness; error handling; color rules off paths; logging debug path; newline termination; exit codes.
- P1: Short flags parity; color-on-TTY; log redirection to file; default log-level behavior.
- P2: Config skeleton constants existence and precedence doc.

## Test Matrix (Selected)

### Version
- E2E | P0 | `duet-rpc --version` prints semver; exit 0.
- E2E | P0 | `duet-rpc version` prints same semver; exit 0.
- Unit | P0 | VersionManager returns single-source version; formatted consistently.

### Help
- E2E | P0 | `duet-rpc --help` and `duet-rpc` show synopsis, subcommands, footer; exit 0.
- Integration | P0 | Unknown subcommand/flag → error + usage; exit 2; no stack trace.
- Unit | P1 | HelpFormatter/CLI metadata builds synopsis `duet-rpc [COMMAND] [OPTIONS]`, lists `version`, `doctor`, `rpc`, `prompt`, footer.

### Color/TTY
- E2E | P0 | `NO_COLOR=1` or `--no-color` → no ANSI in help.
- E2E | P1 | TTY attached → colors present; piped → plain.

### Logging
- E2E | P0 | `--log-level debug version` emits structured debug logs to stderr; stdout is version; exit 0.
- E2E | P1 | Default warn+ only; `version` outputs no logs by default.
- E2E | P1 | `DUET_RPC_LOG=<file>` routes logs to file; stderr quiet.
- E2E | P1 | `DUET_RPC_LOG` invalid path → logger falls back to stderr with a warning; command output/exit unchanged.

### Performance/Format
- E2E | P0 | `--version`/`--help` complete ~100ms (CI threshold relaxed).
- E2E | P0 | All outputs newline-terminated.

## Key Test Cases
- T-CLI-VER-001 (P0): `duet-rpc --version`
  - Expect: stdout `<semver>\n`; stderr empty; exit 0.
- T-CLI-VER-002 (P0): `duet-rpc version`
  - Expect: same as T-CLI-VER-001; versions equal.
- T-CLI-HLP-001 (P0): `duet-rpc --help`
  - Expect: synopsis `duet-rpc [COMMAND] [OPTIONS]`; lists `version`, `doctor`, `rpc`, `prompt`; footer “See 'duet-rpc <command> --help'...”; exit 0; newline.
- T-CLI-HLP-002 (P0): `duet-rpc` (no args)
  - Expect: same as `--help`; exit 0.
- T-CLI-ERR-001 (P0): `duet-rpc frobnicate`
  - Expect: error + usage on stderr; exit 2; no stack trace; stdout empty.
- T-CLI-CLR-001 (P0): `NO_COLOR=1 duet-rpc --help`
  - Expect: no ANSI sequences; exit 0.
- T-CLI-CLR-002 (P0): `duet-rpc --no-color --help`
  - Expect: no ANSI regardless of TTY; exit 0.
- T-CLI-TTY-001 (P1): TTY-attached `duet-rpc --help`
  - Expect: ANSI color present; piped variant plain.
- T-CLI-LOG-001 (P0): `duet-rpc --log-level debug version`
  - Expect: stderr structured log line(s) with timestamp/level/message; stdout version; exit 0.
- T-CLI-LOG-002 (P1): `DUET_RPC_LOG=$TMP/duet.log duet-rpc version`
  - Expect: logs written to file; stderr quiet; file lines include timestamp/level/message.
- T-CLI-LOG-NEG-001 (P1): `DUET_RPC_LOG=$TMP/nonexistent/subdir/file.log duet-rpc --log-level debug version`
  - Expect: stderr contains a clear warning about failing to open/write log file (no stack trace); stdout version; exit 0.
- T-CLI-PERF-001 (P0): Measure duration of `--version` and `--help`
  - Expect: under threshold (e.g., <200ms CI); warmup first run.
- T-CLI-SYN-001 (P1): `-h` and `-V` parity with long flags.
- T-CLI-FMT-001 (P0): Newline at end of stdout/stderr outputs.

## Data & Environment Strategy
- Process: Spawn `duet-rpc`; capture stdout, stderr, exit code separately.
- Env control: Set `NO_COLOR`, `DUET_RPC_LOG`, `--log-level` per test; isolate with per-test temp dirs.
- TTY simulation: Prefer `script -q -c "<cmd>" /dev/null` on Unix-like CI to emulate a TTY; gate or use ConPTY alternatives on Windows.
- Assertions: Golden help text (plain variant) via tasty-golden; regex for ANSI absence/presence and log structure; compare `version` outputs for equality.
- Performance: Warmup then measure; soft threshold with CI headroom.
- Determinism: Avoid asserting timestamps; match ISO-8601 pattern and level names only.
  - For invalid log path, assert presence of a warning substring (e.g., "log" and "failed"/"cannot") rather than exact wording.

## Traceability (AC → Tests)
- `--version` prints semver; exit 0 → T-CLI-VER-001
- `version` prints same semver; exit 0 → T-CLI-VER-002
- `--help` shows synopsis/subcommands/footer; exit 0 → T-CLI-HLP-001
- No args shows help; exit 0 → T-CLI-HLP-002
- Help lists `doctor`, `rpc`, `prompt` → T-CLI-HLP-001 golden
- Unknown subcommand → error + usage; exit 2; no stack trace → T-CLI-ERR-001
- Newline-terminated output → T-CLI-FMT-001
- ~100ms responsiveness → T-CLI-PERF-001
- `--log-level debug` logs structured debug → T-CLI-LOG-001
- Default warn+ only → T-CLI-LOG-001 (stderr quiet on success)
- `DUET_RPC_LOG` to file → T-CLI-LOG-002
- Invalid `DUET_RPC_LOG` path → fallback warning to stderr; command unaffected → T-CLI-LOG-NEG-001
- `NO_COLOR` or `--no-color` → plain → T-CLI-CLR-001/002
- TTY without NO_COLOR → colored help → T-CLI-TTY-001

## Tooling (Haskell Stack)
- Runner: `tasty` with `tasty-hunit` for assertions; `tasty-hedgehog` for properties where useful.
- Golden snapshots: `tasty-golden` to store help text (plain variant) under `test/golden/`.
- Process spawning: `typed-process` or `System.Process` to run the built binary; ensure `cabal build` artifacts are discoverable in tests.
- Temp files/dirs: `temporary` for per-test isolation (e.g., log file paths).
- ANSI checks: Regex for `\x1b\[[0-9;]*m` to detect color sequences.
- TTY emulation: Use `script -q -c` on Unix to force TTY; conditionally skip on Windows or use platform-appropriate PTY.
- Commands: `cabal test` to run; `cabal run duet-rpc -- --help` for manual smoke.

## Entry/Exit Criteria
- Entry: `duet-rpc` builds and is invocable in test environment.
- Exit: All P0 pass; no open severity-1 defects; golden baselines approved.

## Risks & Mitigations
- PTY flakiness on Windows → gate or use ConPTY; ensure non-TTY path covered everywhere.
- Performance variance in CI → relaxed thresholds; skip timing on constrained runners.
- Help text drift → review golden diffs; update intentionally.

## Open Questions
- Decision: For Story 001, tests cover `--log-level` only; a `--debug` alias (if added) will be covered in a later story.
- Decision: CI OS matrix — Linux/macOS run full suite including TTY-based color tests (via `script`); Windows skips TTY color tests and runs non-TTY and NO_COLOR/`--no-color` tests.
- Decision: Include negative case for invalid `DUET_RPC_LOG` path — logger must fall back to stderr with a warning; commands do not crash and exit codes follow normal semantics.
