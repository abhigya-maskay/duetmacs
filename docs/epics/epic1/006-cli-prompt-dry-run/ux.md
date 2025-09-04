# Story 006: CLI Prompt Dry-Run — UX

UX level: Light, specify output, flags, errors, examples.

## Summary
- Purpose: validate end-to-end wiring with deterministic no-op results; never writes files or call providers.
- Success (text): labeled block to stdout; exit code 0.
- Success (JSON): stable object to stdout; exit code 0.
- Ignored flags: accepted and ignored; warn (stderr in text, warnings[] in JSON); exit code 0.
- Errors: concise messages; usage errors (2) include a one-line help hint; file I/O errors (3) show the cause only.

## Output Formats
- Text (default, stdout):
  - Dry run: no changes made
  - File: <as-provided-path>
  - Size: <n> bytes
  - SHA-256: <64-char-hex>
- JSON (`--json`, stdout; snake_case fields; key order unspecified):
  - Success:
    ```json
    {"mode":"dry-run","file":"<path>","size_bytes":1234,"sha256":"<hex>","received_at":"<ISO-8601-UTC>","warnings":[]}
    ```
  - Error:
    ```json
    {"error":{"code":2,"message":"<reason>"}}
    ```

## Flags
- `--file <path>`: required; path echoed as provided.
- `--dry-run`: required to enable this mode.
- `--json`: switches to JSON output.
- `--provider`, `--model`, and other prompt-related flags: accepted but ignored in dry-run.
  - Text mode: warning lines go to stderr.
  - JSON mode: warnings added to `warnings` array; stdout contains JSON only.
- `--stdin` or `--file -`: not supported for dry-run; usage error (exit 2).

## Behavior Details
- Hashing: SHA-256 over raw file bytes; lowercase hex.
- Size: byte count of file contents (integer).
- Timestamp: include `received_at` only in JSON; ISO‑8601 UTC with trailing Z.
- Large files: if size > 50 MB, warn-only (no failure). Text: stderr notice. JSON: warning string in `warnings[]`.

## Errors and Exit Codes
- Success (exit 0): outputs as defined above; no extra text in stdout beyond the block/JSON.
- Usage error (exit 2): missing `--file`, unsupported `--stdin`, or bad combinations.
  - Text (stderr): concise reason + hint “Run `duet-rpc prompt --help`.”
  - JSON (stdout): `{"error":{"code":2,"message":"<reason>"}}` (and optional `warnings` if present).
- File I/O error (exit 3): unreadable/missing file, permission denied.
  - Text (stderr): "Cannot read file '<path>': <reason>"
  - JSON (stdout): `{"error":{"code":3,"message":"Cannot read file '<path>': <reason>"}}`.

## Streams
- Success: all primary output on stdout only.
- Warnings: stderr in text mode; `warnings[]` in JSON mode.
- Errors: text mode emits to stderr; JSON mode emits a single JSON object to stdout; do not mix human text on stdout in JSON mode.

## Examples
- Success (text):
  - Command: `duet-rpc prompt --file README.md --dry-run`
  - stdout:
    - Dry run: no changes made
    - File: README.md
    - Size: 1,234 bytes
    - SHA-256: 89abcdef...0123
  - stderr: (empty unless warnings)
  - exit: 0
- Success (JSON):
  - Command: `duet-rpc prompt --file README.md --dry-run --json`
  - stdout: `{"mode":"dry-run","file":"README.md","size_bytes":1234,"sha256":"89abcdef...0123","received_at":"2025-01-02T03:04:05Z","warnings":[]}`
  - exit: 0
- Ignored flags (text):
  - Command: `duet-rpc prompt --file README.md --dry-run --provider oai --model gpt-4o`
  - stderr: "Notice: ignoring --provider, --model in --dry-run"
  - exit: 0
- Usage error:
  - Command: `duet-rpc prompt --dry-run`
  - stderr: "Missing required --file. Run 'duet-rpc prompt --help'."
  - exit: 2
- File not found:
  - Command: `duet-rpc prompt --file missing.md --dry-run`
  - stderr: "Cannot read file 'missing.md': No such file or directory"
  - exit: 3
- Large file warning:
  - Command: `duet-rpc prompt --file big.bin --dry-run`
  - stderr: "Notice: file size 85.2 MB exceeds 50 MB; proceeding"
  - exit: 0

## Edge Cases
- Determinism: hash computed over raw bytes; no normalization.
- Locale-independent outputs: fixed English strings; ISO‑8601 UTC timestamps.
- Do not print ANSI color or formatting in JSON mode.
- Path echo: show as provided; consider resolved path behind a future `--verbose`.
