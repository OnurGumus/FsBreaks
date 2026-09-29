/// fsbreaks.json: where the breaks files are, and how their tests run.
///
///   {
///     "breaks": "test/Breaks",
///     "test": "test/Rules.Tests --filter \"{name}\"",
///     "env": { "TESTS_DRIVER": "fast" },
///     "timeoutMinutes": 15
///   }
///
/// Every setting may be left out. The folder the file is in is the root:
/// paths in it and in the breaks files are from there.
module FsBreaks.Config

open System
open System.IO
open System.Text.Json
open System.Text.Json.Nodes

type Config =
    { Root: string
      /// The folder of the breaks files.
      Breaks: string
      /// The tests for a breaks file without a `test:` line, in that line's form.
      Test: string option
      /// Environment variables for the builds and the tests.
      Env: Map<string, string>
      /// How long the tests may run before a break counts as making them hang.
      Timeout: TimeSpan }

let fileName = "fsbreaks.json"

let rec private findUp (dir: DirectoryInfo) =
    if isNull dir then None
    elif File.Exists(Path.Combine(dir.FullName, fileName)) then Some dir.FullName
    else findUp dir.Parent

let private setting (path: string) (config: Config) (key: string) (value: JsonNode) =
    try
        match key with
        | "breaks" -> Ok { config with Breaks = value.GetValue<string>() }
        | "test" -> Ok { config with Test = Some(value.GetValue<string>()) }
        | "timeoutMinutes" -> Ok { config with Timeout = TimeSpan.FromMinutes(value.GetValue<float>()) }
        | "env" ->
            Ok
                { config with
                    Env = value.AsObject() |> Seq.map (fun (KeyValue(k, v)) -> k, v.GetValue<string>()) |> Map.ofSeq }
        | other -> Error $"{path}: \"{other}\" is not a setting; there are breaks, test, env and timeoutMinutes"
    with e ->
        Error $"{path}: \"{key}\": {e.Message}"

/// The settings of the fsbreaks.json in the folder or the nearest above it,
/// or the defaults with the folder as the root.
let load (folder: string) : Result<Config, string> =
    let root = findUp (DirectoryInfo(Path.GetFullPath folder)) |> Option.defaultValue (Path.GetFullPath folder)
    let path = Path.Combine(root, fileName)

    let defaults =
        { Root = root
          Breaks = "breaks"
          Test = None
          Env = Map.empty
          Timeout = TimeSpan.FromMinutes 15. }

    if not (File.Exists path) then
        Ok defaults
    else
        try
            let options = JsonDocumentOptions(CommentHandling = JsonCommentHandling.Skip, AllowTrailingCommas = true)

            match JsonNode.Parse(File.ReadAllText path, documentOptions = options) with
            | :? JsonObject as settings ->
                settings
                |> Seq.fold (fun config (KeyValue(key, value)) -> config |> Result.bind (fun c -> setting path c key value)) (Ok defaults)
            | _ -> Error $"{path}: the settings go in one {{ }} object"
        with e ->
            Error $"{path}: {e.Message}"
