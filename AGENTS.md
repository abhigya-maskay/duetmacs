# Agents Guidance

Prefer point-free code in this repository whenever it preserves clarity.

Prefer the imperative shell functional core philosophy when choosing implementation strategies.
Prefer modular code and separation of concerns.

- Follow the `duet-rpc` cabal defaults (`-Wall`, `StrictData`, `OverloadedStrings`, `NamedFieldPuns`, `RecordWildCards`) when adding modules so extensions stay consistent (`duet-rpc/duet-rpc.cabal`).
- Keep module export lists explicit and provide type signatures for top-level definitions to preserve clear APIs (`duet-rpc/src/Duet/Rpc/CLI/Core.hs`, `duet-rpc/src/Duet/Rpc/Logger.hs`).
- Group imports by origin with blank lines and qualify shared namespaces (e.g. `OA`, `T`, `K`) to avoid ambiguity (`duet-rpc/src/Duet/Rpc/CLI/Shell.hs`).
- Prefer `Text` for user-facing IO and logging, only converting to `String` at boundaries (`duet-rpc/src/Duet/Rpc/OutputFormatter/Core.hs`, `duet-rpc/src/Duet/Rpc/Logger.hs`).
- Extend the CLI via the existing registry pattern—update `CliCommand`, `CommandAction`, `commandInfos`, and the dispatcher instead of branching ad hoc (`duet-rpc/src/Duet/Rpc/CLI/Core.hs`).
- Send terminal output through `ShellFormatter` to honor color detection and newline handling (`duet-rpc/src/Duet/Rpc/OutputFormatter/Shell.hs`, `duet-rpc/src/Duet/Rpc/CLI/Shell.hs`).
- Use `Duet.Rpc.Logger` helpers for Katip logging configuration instead of instantiating loggers inline (`duet-rpc/src/Duet/Rpc/Logger.hs`).
