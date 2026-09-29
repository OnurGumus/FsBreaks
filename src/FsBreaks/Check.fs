/// Checking the breaks without trying them: each one's text is found, and the
/// file, broken, still type-checks in its project. Seconds, not a build per break.
module FsBreaks.Check

open System
open System.IO
open System.Runtime.CompilerServices
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Text
open Ionide.ProjInfo
open FsBreaks.Breaks
open FsBreaks.Config

let private isFSharp (file: string) =
    [ ".fs"; ".fsx"; ".fsi" ] |> List.contains (Path.GetExtension(file).ToLowerInvariant())

/// The F# projects in the file's folder, or in the nearest folder above it
/// that has one, up to the root.
let private projectsNear (root: string) (file: string) =
    let rec up (dir: DirectoryInfo) =
        if isNull dir || not (dir.FullName.StartsWith root) then []
        else
            match dir.GetFiles "*.fsproj" with
            | [||] -> up dir.Parent
            | found -> found |> Array.map _.FullName |> List.ofArray

    up (FileInfo(file).Directory)

// MSBuild is the SDK's, found by Ionide.ProjInfo before anything uses it:
// the loading is in a method of its own, compiled only after that.

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private findMSBuild (root: string) = Init.init (DirectoryInfo root) None

[<MethodImpl(MethodImplOptions.NoInlining)>]
let private loadWith (tools: Types.ToolsPath) (projects: string list) =
    let loader = WorkspaceLoader.Create(tools, [])
    loader.LoadProjects(projects, [], BinaryLogGeneration.Off) |> List.ofSeq |> FCS.mapManyOptions |> List.ofSeq

let private errorsIn (file: string) (text: string) (options: FSharpProjectOptions) (version: int) =
    let parsed, answer =
        Syntax.checker.Value.ParseAndCheckFileInProject(file, version, SourceText.ofString text, options)
        |> Async.RunSynchronously

    let diagnostics =
        match answer with
        | FSharpCheckFileAnswer.Succeeded result -> result.Diagnostics
        | FSharpCheckFileAnswer.Aborted -> parsed.Diagnostics

    diagnostics
    |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    |> Array.map (fun d -> $"({d.StartLine},{d.StartColumn + 1}) {d.Message}")
    |> List.ofArray

/// Checks every break of the files named. The number of problems.
let check (config: Config) (names: string list) (only: string option) (log: string -> unit) =
    Runner.recover config log
    let problems = ResizeArray<string>()

    let files =
        match Runner.breaksFiles config names with
        | Error errors ->
            problems.AddRange errors
            []
        | Ok paths ->
            paths
            |> List.choose (fun path ->
                match Breaks.read path with
                | Error errors ->
                    problems.AddRange errors
                    None
                | Ok file -> Some file)

    let full (file: string) = Path.GetFullPath(Path.Combine(config.Root, file))

    let fsharpFiles =
        files
        |> List.collect (Runner.selected only)
        |> List.map (fun b -> full b.File)
        |> List.filter (fun f -> isFSharp f && File.Exists f)
        |> List.distinct

    let projects =
        match fsharpFiles |> List.collect (projectsNear config.Root) |> List.distinct with
        | [] -> []
        | wanted ->
            log $"Loading {wanted.Length} project(s)…"

            try
                loadWith (findMSBuild config.Root) wanted
            with e ->
                problems.Add $"the projects did not load (restore them first?): {e.Message}"
                []

    let optionsOf =
        let byFile =
            fsharpFiles
            |> List.map (fun f ->
                f,
                projects
                |> List.tryFind (fun o -> o.SourceFiles |> Array.exists (fun s -> String.Equals(Path.GetFullPath s, f, StringComparison.Ordinal))))
            |> Map.ofList

        fun f -> byFile |> Map.tryFind f |> Option.flatten

    let mutable version = 0

    let typeChecks (file: string) (text: string) (options: FSharpProjectOptions) =
        version <- version + 1
        errorsIn file text options version

    // A break is only judged against a file that type-checks as it is.
    let asItIs =
        fsharpFiles
        |> List.map (fun f ->
            f,
            match optionsOf f with
            | None -> Error "it is in no project that loaded, so only its text is looked for"
            | Some options ->
                let _, source, _ = Runner.readSource f

                match typeChecks f source options with
                | [] -> Ok options
                | errors -> Error $"it does not type-check as it is: {List.head errors}")
        |> Map.ofList

    let mutable checkedCount = 0
    let told = Collections.Generic.HashSet<string>()

    for file in files do
        log $"\n{file.Name}"

        for b in Runner.selected only file do
            let path = full b.File

            let there (project: string) =
                File.Exists(Path.Combine(config.Root, project)) || Directory.Exists(Path.Combine(config.Root, project))

            let outcome =
                match Runner.guardFor config file b with
                | Error e -> Error("NO TESTS", e)
                | Ok g when not (there g.Project) -> Error("NO TESTS", $"its tests, {g.Project}, are not there")
                | Ok _ when not (File.Exists path) -> Error("STALE", $"{b.File} is not there")
                | Ok _ ->
                    let _, source, _ = Runner.readSource path

                    match Syntax.apply b.File source b.Anchor b.Old b.New with
                    | Error why -> Error("STALE", why)
                    | Ok broken ->
                        match asItIs |> Map.tryFind path with
                        | Some(Ok options) ->
                            match typeChecks path broken options with
                            | [] -> Ok "found, and type-checks"
                            | errors -> Error("NO BUILD", $"it does not type-check: {List.head errors}")
                        | Some(Error why) ->
                            if told.Add path then
                                log $"  ({b.File}: {why})"

                            Ok "found"
                        | None -> Ok "found"

            checkedCount <- checkedCount + 1

            match outcome with
            | Ok detail -> Runner.line log "ok" b.Heading detail
            | Error(mark, why) ->
                Runner.line log mark b.Heading why
                problems.Add $"{file.Name}: {b.Heading}: {why}"

    log $"\n{checkedCount} breaks checked, {problems.Count} problem(s)."
    problems |> Seq.iter (fun p -> log $"  {p}")
    problems.Count
