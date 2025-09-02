# Story 004: CLI RPC Ping

UX level: Light, specify output, flags, errors, examples.

## Summary
- Default success prints `pong` with trailing newline to stdout; exit code 0.
- JSON mode prints minimal object with `status`, `version`, `timestamp` to stdout; exit code 0.
- Timeouts and errors return non-zero exit code, with concise message to stderr. In `--json` mode, errors emit JSON.

## Output Formats
- Text (default):
  - Success: `pong\n` (stdout), exit 0
  - No additional text, version, or timestamp in text mode
- JSON (`--json`):
  - Success (stdout):
    ```json
    {"status":"ok","version":"<semver>","timestamp":"<ISO-8601-UTC>"}
    ```
  - Exit 0

## Flags
- `--json`: switches output to JSON as defined above
- `--timeout <ms>`: maximum time in milliseconds to complete handshake
- Unknown/invalid flags: print helpful usage to stderr and exit non-zero

## Errors and Exit Codes
- Timeout:
  - Exit: 124
  - stderr: `timeout exceeded after <ms> ms`
  - With `--json`: stdout (or stderr) emits JSON: `{"status":"error","message":"timeout exceeded after <ms> ms"}`; prefer stdout for machine parsing
- Generic error (handshake failure, transport error, etc.):
  - Exit: non-zero (e.g., 1)
  - stderr: concise cause summary (no stack by default)
  - With `--json`: JSON object with fields:
    - `status`: `"error"`
    - `message`: concise human-readable message
    - Optional: `code` (string identifier) when available

## Streams
- Success output goes to stdout only
- Error details go to stderr in text mode; JSON error responses may go to stdout to ease parsing in scripts

## Examples
- Default success:
  - Command: `codex-rpc rpc --ping`
  - stdout: `pong\n`
  - exit: 0
- JSON success:
  - Command: `codex-rpc rpc --ping --json`
  - stdout: `{"status":"ok","version":"0.1.0","timestamp":"2025-01-15T12:34:56Z"}`
  - exit: 0
- Timeout (text):
  - Command: `codex-rpc rpc --ping --timeout 500`
  - stderr: `timeout exceeded after 500 ms`
  - exit: 124
- Timeout (json):
  - Command: `codex-rpc rpc --ping --timeout 500 --json`
  - stdout: `{"status":"error","message":"timeout exceeded after 500 ms"}`
  - exit: 124
- Invalid flag:
  - Command: `codex-rpc rpc --ping --unknown`
  - stderr: usage with brief help for `--ping`
  - exit: non-zero

## Edge Cases
- If both stdout and stderr are produced in error mode, ensure JSON consumers can rely on a single JSON object on stdout; keep human-readable diagnostics on stderr
- Honor `--timeout` across all transport steps (startup, connect, handshake)
- Locale-independent outputs: fixed strings, ISO-8601 UTC timestamps
- Do not print ANSI color or formatting in JSON mode

