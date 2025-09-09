# OutputFormatter

## Meta
- Scope date: 2025-01-08
- Author: Component Designer

## Purpose
- Handle all terminal output with proper color management, TTY detection, and symbol fallbacks.

## Responsibilities
- Detect TTY vs pipe output mode
- Apply color schemes based on context and environment
- Provide unicode/ASCII symbol fallbacks
- Honor NO_COLOR and --no-color flags
- Sanitize input to prevent terminal escape injection

## Boundaries
- In-scope: Terminal detection, ANSI color codes, symbol selection, output rendering
- Out-of-scope: Content generation, message composition, logging
- Inputs (conceptual): Text content, formatting hints, color preferences
- Outputs (conceptual): Formatted text to stdout/stderr

## Collaborators & Dependencies
- Internal: None (base infrastructure)
- External: System terminal APIs, environment variables
- Notes: All output components use this for consistent formatting; cache TTY detection at startup; honor NO_COLOR and --no-color; route human output to stdout and errors/logs to stderr; provide Unicode/ASCII palettes with DUET_RPC_ASCII override.

## Risks & Open Questions
- Cross-platform terminal compatibility (ensure ansi-terminal covers Windows VT)
- Decision: Use ansi-terminal for cross-platform color support.
- Decision: Adopt DUET_RPC_ASCII env var for ASCII fallback.

## Technical Design TODO — FCIS

### Data Contracts
- Input: FormatRequest
  - stream: StdStream = Stdout | Stderr
  - style: StyleName = Success | Warn | Error | Info | Plain | Dim
  - text: Text
  - newline: Bool (default True)
  - scope: Help | General (hint for palette nuances; optional)
  - colorFlag: NoColorFlag (from global flags)
  - env: { NO_COLOR: Maybe Text, DUET_RPC_ASCII: Maybe Text, LANG/LC_*: Maybe Text }
  - ttyInfo: { stdoutIsTTY :: Bool, stderrIsTTY :: Bool }
- Output: Rendered text (newline-terminated) written to the selected stream (shell), and/or pure-rendered Text for testing
- Errors: FormatterError = InvalidPalette | UnsupportedEncoding | InternalInvariant
- Domain types:
  - ColorMode = ColorEnabled | ColorDisabled
  - Encoding = Unicode | ASCII
  - Palette (success, warn, error, info, dim) -> [SGR]
  - SymbolSet (ok, warn, fail, info) -> Text

Example (Haskell-style) core types:
```
data StdStream = Stdout | Stderr
data StyleName = Success | Warn | Error | Info | Plain | Dim
data ColorMode = ColorEnabled | ColorDisabled
data Encoding = Unicode | ASCII

data TerminalMode = TerminalMode
  { stdoutTTY     :: Bool
  , stderrTTY     :: Bool
  , colorMode     :: ColorMode
  , encoding      :: Encoding
  }

data Palette = Palette
  { palSuccess :: [SGR]
  , palWarn    :: [SGR]
  , palError   :: [SGR]
  , palInfo    :: [SGR]
  , palDim     :: [SGR]
  }

data SymbolSet = SymbolSet
  { symOK   :: Text
  , symWarn :: Text
  , symFail :: Text
  , symInfo :: Text
  }

data FormatRequest = FormatRequest
  { frStream    :: StdStream
  , frStyle     :: StyleName
  , frText      :: Text
  , frNewline   :: Bool
  }
```

### Core TODO (Pure)
- [ ] Define domain types and invariants
  - [ ] TerminalMode and precedence: `--no-color` > `NO_COLOR` > isTTY
  - [ ] Encoding choice: `DUET_RPC_ASCII=1|true` → ASCII; else UTF-8 heuristics → Unicode; fallback ASCII
- [ ] Input sanitization:
  - [ ] Strip control characters except newline/tab
  - [ ] Remove existing ANSI escape sequences from input
  - [ ] Validate text before applying our own ANSI codes
- [ ] Implement pure decisions:
```
decideColorMode :: { noColorFlag :: Bool, noColorEnv :: Bool, isTTY :: Bool } -> ColorMode
decideEncoding  :: { asciiEnv :: Bool, utf8Env :: Bool } -> Encoding
derivePalette   :: ColorMode -> Palette
deriveSymbols   :: Encoding  -> SymbolSet
```
- [ ] Pure rendering helpers (no IO):
```
renderStyled :: Palette -> StyleName -> Text -> Text
withNewline  :: Bool -> Text -> Text
prefixSymbol :: SymbolSet -> StyleName -> Text -> Text  -- e.g., "✓ info text"
```
- [ ] Error taxonomy (pure): `FormatterError`
- [ ] Unit tests (happy/edge): precedence rules, ANSI absence when disabled, newline termination, symbol fallback

### Shell TODO (Impure)
- [ ] Ports/interfaces for side effects:
```
data Ports = Ports
  { readEnv   :: Text -> IO (Maybe Text)
  , isTTY     :: Handle -> IO Bool
  , hWrite    :: Handle -> Text -> IO ()
  }
```
- [ ] Terminal probing and mode assembly:
```
probeMode :: Ports -> { noColorFlag :: Bool } -> IO TerminalMode
```
- [ ] Adapters/libraries:
  - System.Console.ANSI for SGR codes (colors)
  - System.IO for handles; `hIsTerminalDevice` for TTY
  - System.Environment for env vars; LANG/LC_* detection
- [ ] Mapping external → domain:
  - `NO_COLOR`/`--no-color` → ColorDisabled
  - Piped (non-TTY) → ColorDisabled
  - `DUET_RPC_ASCII` → ASCII symbols
- [ ] Logging/metrics at boundaries (later): optional debug logs for mode decisions (stderr); do not color logs unless explicitly desired by logger component
- [ ] Adapter-level tests: spawnless tests for ANSI presence/absence; env/TTY simulation where feasible

### Wiring/Composition
- [ ] Compose core + adapters at startup
```
data OutputFormatter = OutputFormatter
  { writeStyled :: FormatRequest -> IO ()
  , renderHelp  :: StyleName -> Text -> Text      -- pure helper for HelpFormatter
  , symbols     :: SymbolSet
  , palette     :: Palette
  }

makeOutputFormatter :: Ports -> { noColorFlag :: Bool } -> IO OutputFormatter
```
- [ ] Minimal usage (Haskell-style):
```
fmt <- makeOutputFormatter ports flags
fmt.writeStyled (FormatRequest Stdout Info "duet-rpc [COMMAND] [OPTIONS]" True)
```

### Open Questions (with proposed answers)
- Q: Cross-platform terminal compatibility (Windows VT)?
  - A: Use ansi-terminal (enables Windows VT where available). If VT unsupported, treat as ColorDisabled; still output plain text.
- Q: Confirm `ansi-terminal` as the color backend?
  - A: Yes. `System.Console.ANSI` for SGR; rely on library’s Windows support.
- Q: Semantics for `DUET_RPC_ASCII`?
  - A: When set to `1`/`true` (case-insensitive), force ASCII symbol set. Otherwise prefer Unicode; fall back to ASCII if UTF-8 not indicated by environment.
- Q: Where to detect TTY (stdout vs stderr)?
  - A: Decide per-target handle. Help/user output uses stdout TTY; errors/logs use stderr TTY.
- Q: Newline handling?
  - A: Always newline-terminate user-facing lines (acceptance requires it).
- Q: Logger coupling?
  - A: Avoid direct coupling; logger styles its own output. OutputFormatter exposes pure helpers if logger wants colorization later, but Story 001 keeps them separate.

### DoD
- Meets Story 001 acceptance:
  - Colorized help only when TTY and not disabled by `NO_COLOR`/`--no-color`
  - Plain output when piped or color-disabled
  - Newline-terminated outputs
- Core is pure; effects isolated behind Ports
- Public interfaces (types + `makeOutputFormatter`) are documented and stable
