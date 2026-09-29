# Changelog

## 0.1.0

First release.

- `fsbreaks run`: tries each break (written into its file, the guarding tests built and run, the file put back byte for byte), and fails on a break missed, stale or not building. A file left broken by a killed run is put back by the next one.
- `fsbreaks check`: finds each break's text and type-checks the file with it in its project (F# compiler service, Ionide.ProjInfo), without building or running.
- `fsbreaks suggest`: candidate breaks for a file or one binding in it.
- Breaks files: `test:` for the file's tests, `in Module.function` anchors for F# files, `guarded by` for one break's own tests.
- Tests run with `dotnet run` (Expecto, Microsoft.Testing.Platform) or `dotnet test`; failed tests named from their output.
