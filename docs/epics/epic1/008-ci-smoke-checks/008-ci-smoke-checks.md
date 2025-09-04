# Story 008: CI Smoke Checks

As a maintainer, I want CI to lint, build, and run smoke checks for `duet-rpc` and the Emacs package so that we ensure Epic 1 acceptance is verifiable automatically.

## Priority
- Must

## Dependencies
- Stories 001–007

## Non-Goals
- Artifact publishing to package registries
- Code signing certificates (prepared but not required)
- Windows installer packages

## Business Rationale
- Prevents regressions and validates the end-to-end bootstrap in an automated, repeatable way.
- Produces release-ready artifacts for distribution when tagged.

## Additional Scope: Release Artifacts

### Build Matrix
- Platforms: darwin-amd64, darwin-arm64, linux-amd64, linux-arm64, windows-amd64
- Artifacts: binary + README + LICENSE in .tar.gz (or .zip for Windows)
- Checksums: SHA256 for each artifact
- Naming convention: `duet-rpc-{version}-{os}-{arch}.tar.gz`

### Version Embedding
- Git tag (v*.*.*) becomes embedded version string
- Binary built with optimizations and stripped debug symbols
- Reproducible builds with consistent timestamps

## Acceptance Criteria (Given/When/Then)
- Given a pull request, When CI runs, Then it builds `duet-rpc`, runs `--version`, `doctor`, and `rpc --ping`, and validates non-zero exit on malformed args.
- Given the Emacs package, When CI runs, Then it byte-compiles and runs an Emacs batch script to invoke `duet-rpc-version` and a simulated health check that asserts expected output.
- Given linting, When CI runs, Then code formatting/lint tools run and fail the build on errors.
- Given artifacts, When CI completes, Then logs include outputs for version/doctor/ping and Emacs batch checks for traceability.
- Given a git tag matching `v*.*.*`, When CI runs, Then it produces release artifacts for all supported platforms (darwin-amd64/arm64, linux-amd64/arm64, windows-amd64).
- Given release artifacts, When produced, Then each includes the binary, README.md, and LICENSE file compressed as .tar.gz (.zip for Windows).
- Given the CLI binary, When built for release, Then it embeds the git tag as its version string (verified by `duet-rpc --version`).
- Given release artifacts, When created, Then each has an accompanying .sha256 checksum file.
- Given macOS artifacts, When created, Then they are prepared for code signing if certificates become available.
- Given release builds, When compiled, Then debug symbols are stripped and binary size is optimized.

## Assumptions / Open Questions
- Assumption: CI environment includes both the language toolchain for CLI and Emacs in batch mode — Confidence: medium — Impact: pipeline setup effort — Validation: define a minimal Docker or actions matrix.
- Open question: Preferred linting/formatting tools for the chosen CLI language; capture in bootstrap docs.

## Success Metrics
- Stable CI runs that surface clear failures for version/doctor/ping and Emacs batch checks.
- Release artifacts build successfully for all target platforms when tagged.
- Binary sizes remain reasonable: < 20MB for CLI binary (stripped).

