module FsBreaks.Program

open System
open System.IO

let usage =
    """fsbreaks: does a test fail when the rule it guards is switched off?

  fsbreaks [run] [<name>...] [--only <text>]
      Tries each break: writes it into its file, builds and runs the tests
      that guard it, and puts the file back. Fails if a break is missed
      (every test still passes), stale (its text is not found once), or
      does not build.

  fsbreaks check [<name>...] [--only <text>]
      Finds each break's text and type-checks the file with it, in its
      project, without building or running anything.

  fsbreaks suggest <file> [<binding>]
      Candidate breaks for the file, or for one binding in it
      (e.g. Booking.book), written as a breaks file to copy from.

<name> is a breaks file, by its name or its path; without one, every .breaks
file in the breaks folder. --only keeps the breaks whose heading has <text>.
Settings are read from fsbreaks.json, in this folder or the nearest above it.
See https://github.com/OnurGumus/FsBreaks for the breaks file."""

type private Command =
    | Run of names: string list * only: string option
    | Check of names: string list * only: string option
    | Suggest of file: string * binding: string option
    | Help

let rec private namesAndOnly (args: string list) =
    match args with
    | [] -> Ok([], None)
    | "--only" :: text :: rest -> namesAndOnly rest |> Result.map (fun (names, _) -> names, Some text)
    | [ "--only" ] -> Error "--only needs the text a heading has"
    | flag :: _ when flag.StartsWith "-" -> Error $"{flag} is not an option"
    | name :: rest -> namesAndOnly rest |> Result.map (fun (names, only) -> name :: names, only)

let private commandOf (args: string list) =
    match args with
    | [ "--help" ]
    | [ "-h" ]
    | [ "help" ] -> Ok Help
    | "check" :: rest -> namesAndOnly rest |> Result.map Check
    | [ "suggest"; file ] -> Ok(Suggest(file, None))
    | [ "suggest"; file; binding ] -> Ok(Suggest(file, Some binding))
    | "suggest" :: _ -> Error "suggest takes a file, and may take a binding in it"
    | "run" :: rest
    | rest -> namesAndOnly rest |> Result.map Run

[<EntryPoint>]
let main argv =
    let log (line: string) = Console.Out.WriteLine line

    match commandOf (List.ofArray argv) with
    | Error e ->
        eprintfn "%s\n\n%s" e usage
        2
    | Ok Help ->
        printfn "%s" usage
        0
    | Ok(Suggest(file, binding)) ->
        if not (File.Exists file) then
            eprintfn "%s is not there" file
            2
        else
            match Suggest.candidates file (File.ReadAllText file) binding with
            | Ok found ->
                printfn "%s" (Suggest.render file found)
                0
            | Error e ->
                eprintfn "%s" e
                2
    | Ok command ->
        match Config.load Environment.CurrentDirectory with
        | Error e ->
            eprintfn "%s" e
            2
        | Ok config ->
            let problems =
                match command with
                | Check(names, only) -> Check.check config names only log
                | Run(names, only) -> Runner.run config names only log
                | _ -> 0

            if problems > 0 then 1 else 0
