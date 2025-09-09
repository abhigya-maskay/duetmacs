# Epic 1: Project Bootstrap - Technical Risk Register

## Risk Register

| Title | Category | Description | Likelihood | Impact | Severity | Status |
|---|---|---|---|---|---|---|
| RPC subprocess spawn failure | Integrations | Emacs subprocess management may fail on restricted systems; no fallback for spawn errors in acceptance criteria | Medium | High | Critical | Mitigation Planned |
| Cross-platform TTY detection | Legacy | TTY detection behaves differently across Windows/Unix; NO_COLOR handling may be inconsistent | Medium | Medium | Medium | In Progress |
| Config file parsing race | Integrations | Multiple config sources (env/project/home) loaded without lock; concurrent reads during startup may cause inconsistent state | Low | High | Medium | Mitigation Planned |
| Log file permission denied | Security | DUET_RPC_LOG path may point to restricted directories; fallback to stderr not guaranteed to preserve all log data | Medium | Low | Low | Resolved |
| Version string mismatch | Legacy | Version sourced from package manifest; build-time vs runtime version may diverge in dev environments | Low | Low | Low | Resolved |
| CLI response time breach | Performance | 100ms response requirement for help/version; cold start on slower systems may exceed limit | Low | Medium | Low | Accepted |
| Colorization terminal escape | Security | ANSI escape sequences in help output; potential for terminal injection if not properly sanitized | Low | High | Medium | Mitigation Planned |
| Ping timeout handling | Performance | RPC ping with --timeout may hang if subprocess is unresponsive; exit code 124 not guaranteed | Medium | Medium | Medium | Mitigation Planned |
| Structured log format drift | Integrations | Log format (timestamp/level/message) not versioned; consumers may break on format changes | Low | Medium | Low | Accepted |
| Unicode fallback symbols | Legacy | Unicode detection for symbols (OK/WARN/FAIL); may display incorrectly in some terminals | Low | Low | Low | Resolved |

## Critical Risks Summary

### 1. RPC Subprocess Spawn Failure (Critical)
**Evidence**: Acceptance criteria mentions subprocess management but no error recovery specified
**Mitigation Status**: IMPLEMENTED - Added to acceptance criteria and ErrorHandler component
**Mitigation Details**: 
- 3 retry attempts with 1s delay between attempts
- Clear error messages with spawn failure reason
- Debug logging for each attempt
- ErrorHandler component updated with spawn retry logic

### 2. Cross-Platform TTY Detection (Medium)
**Evidence**: Multiple platform support implied but Windows-specific behavior not detailed
**Mitigation Status**: IN PROGRESS - OutputFormatter component fully designed
**Mitigation Details**:
- ansi-terminal library provides cross-platform support
- Windows VT enablement handled by library
- Fallback to ColorDisabled when VT unsupported
- Separate TTY detection for stdout and stderr
- Complete technical design in OutputFormatter component

### 3. Config File Parsing Race (Medium)
**Evidence**: Multiple config sources loaded without synchronization mechanism
**Mitigation Status**: PLANNED - Deferred to Story 003
**Mitigation Details**:
- Sequential loading in deterministic order (env → project → home)
- Cache parsed config at startup to avoid re-reads
- No file locking needed if configs are read-only at runtime
- Added to ConfigLoader component risks for Story 003 implementation

### 4. Log File Permission Denied (Low)
**Evidence**: DUET_RPC_LOG may point to restricted directories
**Mitigation Status**: RESOLVED - Already in acceptance criteria
**Mitigation Details**:
- Acceptance criteria explicitly handles this case
- Falls back to stderr with single warning message
- No stack trace shown to user
- Command continues with normal operation

### 5. Version String Mismatch (Low)
**Evidence**: Version sourced from package manifest
**Mitigation Status**: RESOLVED - Single source of truth established
**Mitigation Details**:
- Cabal package version via generated Paths module
- VersionManager component ensures consistency
- No manual version strings to maintain
- Build-time version automatically available at runtime

### 6. CLI Response Time Breach (Low)
**Evidence**: 100ms response requirement for help/version
**Mitigation Status**: ACCEPTED - Performance target, not hard requirement
**Decision Details**:
- Low severity risk acceptable for Story 001
- Focus on functionality over performance optimization
- Can profile and optimize in later stories if needed
- Typical developer hardware expected to meet target

### 7. Colorization Terminal Escape (Medium)
**Evidence**: ANSI codes in help output without sanitization mention
**Mitigation Status**: PLANNED - Added to OutputFormatter component
**Mitigation Details**:
- Strip control characters except newline/tab
- Remove existing ANSI sequences from input
- Validate text before applying our own ANSI codes
- Added sanitization responsibility to OutputFormatter

### 8. Ping Timeout Handling (Medium)
**Evidence**: RPC ping with --timeout flag mentioned but no implementation details
**Mitigation Status**: PLANNED - Deferred to RPC story
**Mitigation Details**:
- Out of scope for Story 001 (only version/help)
- To be implemented when RPC command is added
- Should use exit code 124 for timeout
- Implement proper signal handling (SIGTERM then SIGKILL)

### 9. Structured Log Format Drift (Low)
**Evidence**: Log format not versioned for backwards compatibility
**Mitigation Status**: ACCEPTED - Low risk for initial implementation
**Decision Details**:
- Low severity acceptable for Story 001
- Initial format establishes baseline
- Versioning can be added when external consumers exist
- Internal debugging focus reduces compatibility concerns

### 10. Unicode Fallback Symbols (Low)
**Evidence**: Unicode detection for terminal symbols
**Mitigation Status**: RESOLVED - Complete design in OutputFormatter
**Mitigation Details**:
- DUET_RPC_ASCII environment variable for forcing ASCII
- UTF-8 detection from LANG/LC_* environment variables
- Explicit fallback symbols defined: OK→[OK], WARN→[WARN], etc.
- SymbolSet type with Unicode and ASCII variants implemented

## Risk Categories Coverage

- **Integrations**: 4 risks identified (RPC, config, logging, structured formats)
- **Performance**: 2 risks identified (response time, timeout handling)
- **Security**: 2 risks identified (log permissions, terminal escape)
- **Legacy**: 3 risks identified (cross-platform, version, unicode)

## Status Definitions
- **Assessed**: Risk has been identified and scored but no mitigation planned yet
- **Mitigation Planned**: Approach to address risk has been defined
- **In Progress**: Mitigation work is underway
- **Resolved**: Risk has been eliminated
- **Accepted**: Risk acknowledged but will not be mitigated