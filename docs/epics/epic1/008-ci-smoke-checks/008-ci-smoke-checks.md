# Story 008: CI Smoke Checks

As a maintainer, I want CI to lint, build, and run smoke checks for `codex-rpc` and the Emacs package so that we ensure Epic 1 acceptance is verifiable automatically.

## Priority
- Must

## Dependencies
- Stories 001–007

## Non-Goals
- Release packaging and publishing
- Full multi-platform matrix beyond a minimal set
- Artifact signing

## Business Rationale
- Prevents regressions and validates the end-to-end bootstrap in an automated, repeatable way.

## Acceptance Criteria (Given/When/Then)
- Given a pull request, When CI runs, Then it builds `codex-rpc`, runs `--version`, `doctor`, and `rpc --ping`, and validates non-zero exit on malformed args.
- Given the Emacs package, When CI runs, Then it byte-compiles and runs an Emacs batch script to invoke `codex-rpc-version` and a simulated health check that asserts expected output.
- Given linting, When CI runs, Then code formatting/lint tools run and fail the build on errors.
- Given artifacts, When CI completes, Then logs include outputs for version/doctor/ping and Emacs batch checks for traceability.

## Assumptions / Open Questions
- Assumption: CI environment includes both the language toolchain for CLI and Emacs in batch mode — Confidence: medium — Impact: pipeline setup effort — Validation: define a minimal Docker or actions matrix.
- Open question: Preferred linting/formatting tools for the chosen CLI language; capture in bootstrap docs.

## Success Metrics
- Stable CI runs that surface clear failures for version/doctor/ping and Emacs batch checks.

