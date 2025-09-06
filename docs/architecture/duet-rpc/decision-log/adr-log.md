# Architecture Decision Log: duet-rpc

This document contains the detailed architecture decision records (ADRs) for the duet-rpc tool. For the architecture overview, see [../technical-architecture.md](../technical-architecture.md).

---

## ADR-001: Architecture Style

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Need to support both Emacs subprocess integration and CLI one-shot commands
- Startup latency matters for Emacs UX
- Must handle long-running AI streaming operations

### Options
- A: Simple CLI tool (stateless, per-command execution)
- B: Client-server (persistent daemon, RPC protocol)  
- C: Hybrid (daemon mode + one-shot commands)
- D: Microservices (separate services)

### Trade-offs
- A: ✓ Simple ✗ High startup latency for Emacs
- B: ✓ Low latency ✗ No CLI one-shot support
- C: ✓ Best of both ✗ More complex architecture
- D: ✓ Scalable ✗ Overkill for single-user tool

### Decision Scope
- In scope: Process model, communication patterns
- Out of scope: Deployment, packaging

### Decision
Hybrid model (C) - daemon for Emacs, one-shot commands for CLI

### Consequences
- Positive: Low latency for Emacs, scriptable CLI
- Negative: Two code paths to maintain
- Follow-up: Ensure shared business logic between modes

### Open Questions
None

### References
- Epic: Project Bootstrap

---

## ADR-002: RPC Protocol

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Emacs needs bidirectional communication with daemon
- Must support request/response and streaming
- Protocol should be well-specified and tooling-friendly

### Options
- A: JSON-RPC 2.0 over stdio
- B: Custom line-delimited JSON
- C: MessagePack RPC
- D: gRPC

### Trade-offs
- A: ✓ Standard, good tooling ✗ Verbose
- B: ✓ Simple ✗ No standard, custom parsing
- C: ✓ Efficient ✗ Less tooling, binary format
- D: ✓ Powerful ✗ Complex, needs code generation

### Decision Scope
- In scope: Wire protocol, message format
- Out of scope: Transport (using stdio)

### Decision
JSON-RPC 2.0 (A) with LSP-style length-prefixed framing

### Consequences
- Positive: Well-specified, extensive tooling, Emacs support
- Negative: JSON overhead for large payloads
- Follow-up: Implement streaming via notifications

### Open Questions
None

### References
- Story 004: CLI RPC Ping
- Story 005: Emacs Subprocess Management

---

## ADR-003: Haskell Build System

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Building a production CLI tool in Haskell
- Need reproducible builds and dependency management
- Developer experience matters

### Options
- A: Stack (curated package sets)
- B: Cabal (official, flexible)
- C: Nix (reproducible)
- D: Bazel (powerful)

### Trade-offs
- A: ✓ Reproducible, beginner-friendly ✗ Less flexible
- B: ✓ Flexible, official ✗ Dependency conflicts
- C: ✓ Perfect reproducibility ✗ Complex, niche
- D: ✓ Scalable ✗ Steep learning curve

### Decision Scope
- In scope: Build system, dependency management
- Out of scope: CI/CD pipeline

### Decision
Cabal (B) with GHC 9.6

### Consequences
- Positive: Official tooling, maximum flexibility
- Negative: Need careful dependency management
- Follow-up: Setup freeze files for reproducibility

### Open Questions
None

### References
- Story 001: CLI Version/Help Bootstrap

---

## ADR-004: Core Libraries

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Need robust libraries for CLI parsing, JSON, logging, HTTP
- Consistency and maintainability important
- Performance requirements are moderate

### Options
Multiple library choices per category (see details below)

### Trade-offs
- CLI: optparse-applicative for composability
- JSON: aeson + deriving-aeson for less boilerplate
- Logging: katip for structured logging
- HTTP: req for balance of safety and simplicity
- Concurrency: async + STM for standard patterns

### Decision Scope
- In scope: Core library selection
- Out of scope: Version pinning

### Decision
- CLI: optparse-applicative
- JSON: aeson + deriving-aeson
- Logging: katip
- HTTP: req
- Concurrency: async + STM
- Config: tomland

### Consequences
- Positive: Well-maintained, standard libraries
- Negative: Multiple dependencies to manage
- Follow-up: Setup proper bounds in cabal file

### Open Questions
None

### References
- All Epic 1 stories

---

## ADR-005: Error Handling

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Need consistent, explicit error handling throughout codebase
- Haskell offers multiple error handling strategies
- Want to avoid hidden exceptions and runtime surprises

### Options
- A: Exceptions for all errors
- B: Either/ExceptT for expected errors
- C: Typed errors with error ADTs everywhere
- D: Mix: Either for domain, exceptions for IO

### Trade-offs
- A: ✓ Simple ✗ Hidden control flow
- B: ✓ Explicit expected errors ✗ Exceptions still possible
- C: ✓ Fully explicit, type-safe ✗ Verbose
- D: ✓ Pragmatic ✗ Inconsistent

### Decision Scope
- In scope: Error handling strategy, error types
- Out of scope: Error message formatting

### Decision
Typed errors with ADTs everywhere (C)

### Consequences
- Positive: No hidden exceptions, explicit error handling
- Negative: More verbose, IO operations need wrapping
- Follow-up: Define error ADTs per module

### Open Questions
None

### References
- Story 003: CLI Doctor

---

## ADR-006: State Management

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Daemon needs to manage concurrent operations safely
- Session state, connections, and streaming responses
- Haskell offers STM for safe concurrent state

### Options
- A: STM with TVar/TMVar
- B: ReaderT pattern with IORef
- C: State monad transformer
- D: Message-passing only

### Trade-offs
- A: ✓ Composable, deadlock-free ✗ Learning curve
- B: ✓ Simple ✗ Race conditions possible
- C: ✓ Pure interface ✗ Not concurrent-safe
- D: ✓ Actor-like ✗ Complex for simple state

### Decision Scope
- In scope: Runtime state management
- Out of scope: Persistence

### Decision
STM with TVar/TMVar (A)

### Consequences
- Positive: Safe concurrent access, composable
- Negative: STM learning curve
- Follow-up: Design state types carefully

### Open Questions
None

### References
- Story 005: Emacs Subprocess Management

---

## ADR-007: Data Persistence

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Need to persist chat sessions and configuration
- Want simple, debuggable storage
- No complex querying requirements

### Options
- A: JSON files in XDG directories
- B: SQLite database
- C: Directory structure with markdown
- D: In-memory only

### Trade-offs
- A: ✓ Simple, portable, debuggable ✗ No queries
- B: ✓ Queryable, ACID ✗ Binary format, dependency
- C: ✓ Human-readable ✗ Complex parsing
- D: ✓ Simplest ✗ No persistence

### Decision Scope
- In scope: Session storage, config storage
- Out of scope: Caching, metrics storage

### Decision
JSON files in XDG directories (A)

### Consequences
- Positive: Simple, version-control friendly
- Negative: Manual indexing for search
- Follow-up: Define JSON schemas

### Open Questions
None

### References
- Epic: Persistence & History

---

## ADR-008: File Safety

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Tool modifies user files based on AI suggestions
- Must prevent data loss and allow recovery
- Security concerns about path traversal

### Options
- A: Dry-run mode with confirmation
- B: Temporary workspace
- C: Container isolation
- D: Permission checks only

### Trade-offs
- A: ✓ Simple, explicit ✗ No isolation
- B: ✓ Safe preview ✗ Complex workflow
- C: ✓ Full isolation ✗ Very complex
- D: ✓ Minimal ✗ Insufficient safety

### Decision Scope
- In scope: File modification safety, backups
- Out of scope: Network isolation

### Decision
Dry-run with confirmation (A) + path canonicalization + jail to project root

### Consequences
- Positive: Simple, effective safety
- Negative: Relies on user attention
- Follow-up: Implement .bak timestamped backups

### Open Questions
None

### References
- Epic: Safety & Controls

---

## ADR-009: Testing Strategy

**Status**: Accepted  
**Date**: 2025-01-05

### Context
- Need reliable testing for RPC protocol and business logic
- Property testing valuable for parsers/serializers
- Integration testing critical for Emacs interop

### Options
- A: Primarily unit tests
- B: Mix of unit and property
- C: Property for core, unit for integration
- D: Minimal testing

### Trade-offs
- A: ✓ Simple, specific ✗ Missing edge cases
- B: ✓ Balanced ✗ More complex
- C: ✓ Targeted approach ✗ Need both frameworks
- D: ✓ Fast development ✗ Poor quality

### Decision Scope
- In scope: Test strategy, frameworks
- Out of scope: Coverage targets

### Decision
Property tests for core logic, unit for integration (C) using tasty framework

### Consequences
- Positive: Good coverage, catches edge cases
- Negative: Two testing styles to maintain
- Follow-up: Setup tasty with QuickCheck integration

### Open Questions
None

### References
- Story 008: CI Smoke Checks

---

## Decision Tracking

| ADR | Area | Decision | Status |
|-----|------|----------|--------|
| 001 | Architecture Style | Hybrid daemon + CLI | Accepted |
| 002 | RPC Protocol | JSON-RPC 2.0 over stdio | Accepted |
| 003 | Build System | Cabal with GHC 9.6 | Accepted |
| 004 | Core Libraries | See detailed list | Accepted |
| 005 | Error Handling | Typed ADTs everywhere | Accepted |
| 006 | State Management | STM with TVar/TMVar | Accepted |
| 007 | Persistence | JSON files in XDG | Accepted |
| 008 | File Safety | Dry-run + canonicalization | Accepted |
| 009 | Testing | Property + unit with tasty | Accepted |