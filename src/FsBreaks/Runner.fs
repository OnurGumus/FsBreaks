/// Trying the breaks: each one written into its file, the tests that guard it
/// built and run, and the file put back.
module FsBreaks.Runner

open System
open System.Diagnostics
open System.IO
open System.Text
open System.Text.Json
open System.Text.RegularExpressions
open FsBreaks.Breaks
open FsBreaks.Config

/// How a run of tests went.
type Outcome =
    | Passed
    | Failed of tests: string list
    | RanNothing
    | Hung

/// What came of a break.
type Verdict =
    /// A test failed on it: the rule is guarded.
    | Caught of string
    /// Every test still passed: nothing guards the rule, or the code decides nothing.
    | Missed of string
    /// Its text was not found once: the code has moved on.
    | Stale of string
    /// It did not build, so nothing was learned.
    | Unbuildable of string

/// Runs dotnet in the root with the settings' environment: its exit code and
/// its output without colours, or None if it ran past the timeout.
let dotnet (config: Config) (args: string list) (timeout: TimeSpan) =
    let info =
        ProcessStartInfo(
            "dotnet",
            args,
            RedirectStandardOutput = true,
            RedirectStandardError = true,
            WorkingDirectory = config.Root
        )

    for KeyValue(name, value) in config.Env do
        info.Environment[name] <- value

    use p = Process.Start info
    let out = p.StandardOutput.ReadToEndAsync()
    let err = p.StandardError.ReadToEndAsync()

    if p.WaitForExit timeout then
        p.WaitForExit()
        Some(p.ExitCode, Regex.Replace(out.Result + err.Result, @"\x1b\[[0-9;?]*[A-Za-z]", ""))
    else
        p.Kill true
        None

let private relative (config: Config) (text: string) =
    text.Replace(config.Root.TrimEnd('/', '\\') + string Path.DirectorySeparatorChar, "")

/// Builds the project, and with it the source it is built from.
let build (config: Config) (project: string) (restore: bool) =
    let args = [ "build"; project; "--nologo"; "-v"; "q" ] @ (if restore then [] else [ "--no-restore" ])

    match dotnet config args (TimeSpan.FromMinutes 10.) with
    | Some(0, _) -> Ok()
    | Some(_, out) ->
        match
            out.Split '\n'
            |> Array.filter (fun l -> l.Contains ": error ")
            |> Array.map (fun l -> (relative config l).Trim())
            |> Array.distinct
            |> Array.truncate 3
        with
        | [||] -> Error "the build failed"
        | errors -> Error(String.concat "\n" errors)
    | None -> Error "the build did not finish"

/// Whether the project runs its own tests (`dotnet run`), as Expecto and
/// Microsoft.Testing.Platform projects do, or is run by `dotnet test`.
let private runsItself (config: Config) (project: string) =
    let path = Path.Combine(config.Root, project)

    let file =
        if Directory.Exists path then
            Directory.GetFiles(path, "*.*proj") |> Array.tryHead
        else
            Some path

    file
    |> Option.exists (fun f ->
        File.Exists f
        && Regex.IsMatch(File.ReadAllText f, @"<OutputType>\s*Exe\s*</OutputType>", RegexOptions.IgnoreCase))

/// The names of the failed tests, as Expecto, `dotnet test` and
/// Microsoft.Testing.Platform write them.
let private failedPatterns =
    [ @"^\[[^\]]*\] (.+?) (?:failed|errored) in \d"
      @"^\s*Failed (.+?) \[[\d<]"
      @"^\s*failed (.+?) \(\d" ]

/// What the runners say when a filter leaves no test to run.
let private nothingPatterns =
    [ @"\b0 tests run\b"
      "No test matches the given testcase filter"
      "No test is available"
      "Zero tests ran"
      @"Total tests: 0\b" ]

let outcomeOf (code: int) (out: string) =
    let failed =
        out.Split '\n'
        |> Array.map _.TrimEnd('\r')
        |> Array.choose (fun line ->
            failedPatterns
            |> List.tryPick (fun p ->
                let m = Regex.Match(line, p)
                if m.Success then Some(m.Groups[1].Value.Trim()) else None))
        |> Array.distinct
        |> List.ofArray

    if nothingPatterns |> List.exists (fun p -> Regex.IsMatch(out, p)) then RanNothing
    elif code = 0 then Passed
    elif failed.IsEmpty then Failed [ $"the tests exited with {code}" ]
    else Failed failed

/// Runs the guard's tests, built already.
let test (config: Config) (guard: Guard) =
    let args =
        if runsItself config guard.Project then
            [ "run"; "--project"; guard.Project; "--no-build"; "--" ] @ guard.Args
        else
            [ "test"; guard.Project; "--no-build" ] @ guard.Args

    match dotnet config args config.Timeout with
    | None -> Hung
    | Some(code, out) -> outcomeOf code out

// A break is in place while its tests run. If the run is stopped, the file is
// put back on the way out; if it is killed, the next run puts it back from the
// copy kept in .fsbreaks.

let private journalOf (config: Config) = Path.Combine(config.Root, ".fsbreaks", "restore.json")

/// Puts back a file a stopped run left broken.
let recover (config: Config) (log: string -> unit) =
    let journal = journalOf config

    if File.Exists journal then
        let saved = JsonDocument.Parse(File.ReadAllText journal).RootElement
        let file = saved.GetProperty("file").GetString()
        File.WriteAllBytes(Path.Combine(config.Root, file), saved.GetProperty("bytes").GetBytesFromBase64())
        File.Delete journal
        log $"{file} was left broken by a run that was stopped: it is put back."

let private keep (config: Config) (file: string) (bytes: byte array) =
    let journal = journalOf config
    let folder = Path.GetDirectoryName journal
    Directory.CreateDirectory folder |> ignore
    File.WriteAllText(Path.Combine(folder, ".gitignore"), "*\n")
    File.WriteAllText(journal, JsonSerializer.Serialize(dict [ "file", file; "bytes", Convert.ToBase64String bytes ]))

let mutable private putBack: unit -> unit = ignore
let private whenStopped = lazy (Console.CancelKeyPress.Add(fun _ -> putBack ()); AppDomain.CurrentDomain.ProcessExit.Add(fun _ -> putBack ()))

let private bom = Encoding.UTF8.GetPreamble()

let private hasBom (bytes: byte array) =
    bytes.Length >= bom.Length && bytes.AsSpan(0, bom.Length).SequenceEqual(bom)

/// The file's text, and how to write text back as the file had it.
let readSource (path: string) =
    let bytes = File.ReadAllBytes path
    let skip = if hasBom bytes then bom.Length else 0
    let encode (text: string) = Array.append bytes[.. skip - 1] (UTF8Encoding(false).GetBytes text)
    bytes, Encoding.UTF8.GetString(bytes, skip, bytes.Length - skip), encode

/// The tests' names without what the guard's arguments already say: a
/// filter the names all start with.
let private shortNames (guard: Guard) (tests: string list) =
    let short (name: string) =
        guard.Args
        |> List.filter (fun a -> a.Length > 2 && name.Length > a.Length + 1 && name.StartsWith a && ".:/ ".Contains name[a.Length])
        |> List.sortByDescending _.Length
        |> List.tryHead
        |> Option.map (fun a -> name.Substring(a.Length + 1).Trim())
        |> Option.defaultValue name

    tests |> List.map short

let private shortList (tests: string list) =
    match tests with
    | a :: b :: c :: (_ :: _ as rest) -> $"""{String.concat "; " [ a; b; c ]}; and {rest.Length} more"""
    | few -> String.concat "; " few

/// Writes the break, runs the tests that guard it, and puts the file back.
/// The file is written back, not moved back: its time has to be later than
/// the build's, or the next build keeps the broken assemblies.
let tryBreak (config: Config) (guard: Guard) (b: Break) =
    let path = Path.Combine(config.Root, b.File)

    if not (File.Exists path) then
        Stale $"{b.File} is not there"
    else
        let original, source, encode = readSource path

        match Syntax.apply b.File source b.Anchor b.Old b.New with
        | Error why -> Stale why
        | Ok broken ->
            whenStopped.Force()
            keep config b.File original

            putBack <-
                fun () ->
                    File.WriteAllBytes(path, original)
                    File.Delete(journalOf config)

            try
                File.WriteAllBytes(path, encode broken)

                match build config guard.Project false with
                | Error e -> Unbuildable $"it does not build:\n{e}"
                | Ok() ->
                    match test config guard with
                    | Passed -> Missed $"every test in {guard.Project} still passes"
                    | RanNothing -> Missed $"no test in {guard.Project} ran"
                    | Hung -> Caught "the tests hung"
                    | Failed tests -> Caught("by " + shortList (shortNames guard tests))
            finally
                putBack ()
                putBack <- ignore

/// The tests for the break: its own, its file's, or the settings'. `{name}`
/// in their arguments is the breaks file's name.
let guardFor (config: Config) (file: BreaksFile) (b: Break) : Result<Guard, string> =
    let named (g: Guard) =
        { g with Args = g.Args |> List.map _.Replace("{name}", file.Name) }

    match b.GuardedBy, file.Test, config.Test with
    | Some g, _, _
    | None, Some g, _ -> Ok(named g)
    | None, None, Some line -> guardOf line |> Result.map named |> Result.mapError (fun e -> $"{fileName}: test: {e}")
    | None, None, None -> Error """no tests guard it: give the breaks file a "test:" line, or fsbreaks.json a "test" """

/// The breaks files named, by name or path, or every one in the breaks folder.
let breaksFiles (config: Config) (names: string list) : Result<string list, string list> =
    let folder = Path.Combine(config.Root, config.Breaks)

    let all =
        if Directory.Exists folder then
            Directory.GetFiles(folder, "*.breaks") |> Array.sort |> List.ofArray
        else
            []

    let pick (name: string) =
        if name.EndsWith ".breaks" && File.Exists name then
            Ok(Path.GetFullPath name)
        else
            all
            |> List.tryFind (fun p -> Path.GetFileNameWithoutExtension p = name)
            |> Option.map Ok
            |> Option.defaultValue (Error $"no breaks file {name} in {config.Breaks}")

    match names with
    | [] when all.IsEmpty -> Error [ $"no .breaks files in {folder}" ]
    | [] -> Ok all
    | names ->
        let picked = names |> List.map pick

        match picked |> List.choose (function Error e -> Some e | Ok _ -> None) with
        | [] -> Ok(picked |> List.choose (function Ok p -> Some p | Error _ -> None))
        | errors -> Error errors

/// The breaks of the file the heading filter keeps.
let selected (only: string option) (file: BreaksFile) =
    file.Breaks
    |> List.filter (fun b -> only |> Option.forall (fun o -> b.Heading.Contains(o, StringComparison.OrdinalIgnoreCase)))

let private mark (verdict: Verdict) =
    match verdict with
    | Caught _ -> "caught "
    | Missed _ -> "MISSED "
    | Stale _ -> "STALE  "
    | Unbuildable _ -> "NO BUILD"

let detail (verdict: Verdict) =
    match verdict with
    | Caught d
    | Missed d
    | Stale d
    | Unbuildable d -> d

/// A break's lines in the report: a mark, its heading, and what came of it.
let line (log: string -> unit) (mark: string) (heading: string) (detail: string) =
    log $"  {mark,-8} {heading}"

    for l in detail.Split '\n' do
        log $"{String(' ', 11)}{l}"

let report (log: string -> unit) (heading: string) (verdict: Verdict) =
    line log (mark verdict) heading (detail verdict)

/// Tries every break of the files named. The number of problems: breaks
/// missed, stale or not building, and anything that kept breaks from being tried.
let run (config: Config) (names: string list) (only: string option) (log: string -> unit) =
    recover config log
    let problems = ResizeArray<string>()
    let verdicts = ResizeArray<Verdict>()
    let built = Collections.Generic.HashSet<string>()

    match breaksFiles config names with
    | Error errors -> problems.AddRange errors
    | Ok paths ->
        for path in paths do
            match Breaks.read path with
            | Error errors -> problems.AddRange errors
            | Ok file ->
                log $"\n{file.Name}"

                let breaks =
                    selected only file |> List.map (fun b -> b, guardFor config file b)

                for b, guard in breaks do
                    match guard with
                    | Error e ->
                        report log b.Heading (Unbuildable e)
                        problems.Add $"{file.Name}: {b.Heading}: {e}"
                    | Ok _ -> ()

                // A break only counts as caught by tests that passed before it.
                let passing =
                    breaks
                    |> List.choose (fun (_, g) -> Result.toOption g)
                    |> List.distinct
                    |> List.filter (fun guard ->
                        built.Add guard.Project |> ignore

                        let failing why =
                            log $"  {guard.Project} {why}"
                            problems.Add $"{file.Name}: {guard.Project} {why}"
                            false

                        match build config guard.Project true with
                        | Error e -> failing $"does not build before any break:\n{e}"
                        | Ok() ->
                            match test config guard with
                            | Passed -> true
                            | Failed tests -> failing $"fails before any break: {shortList (shortNames guard tests)}"
                            | RanNothing -> failing $"""runs no test with {String.concat " " guard.Args}"""
                            | Hung -> failing "hangs before any break"
                    )
                    |> Set.ofList

                for b, guard in breaks do
                    match guard with
                    | Ok guard when passing.Contains guard ->
                        let verdict = tryBreak config guard b
                        report log b.Heading verdict
                        verdicts.Add verdict

                        match verdict with
                        | Caught _ -> ()
                        | other -> problems.Add $"{file.Name}: {b.Heading}: {(detail other).Split('\n')[0]}"
                    | _ -> ()

        // Leave the assemblies built from the source as it is.
        for project in built do
            build config project false
            |> Result.mapError (fun e -> problems.Add $"the last build of {project} failed:\n{e}")
            |> ignore

    let count f = verdicts |> Seq.filter f |> Seq.length

    log
        $"\n{verdicts.Count} breaks tried: {count _.IsCaught} caught, {count _.IsMissed} missed, {count _.IsStale} stale, {count _.IsUnbuildable} not building."

    if problems.Count > 0 then
        log $"\n{problems.Count} problem(s):"
        problems |> Seq.iter (fun p -> log $"  {p}")

    problems.Count
