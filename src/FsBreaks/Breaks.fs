/// The breaks file: the rules a set of tests claims to guard, and for each the
/// text that switches it off.
///
///   # Notes start with #.
///   test: tests/Rules.Tests --filter "Booking"
///
///   ## A class is not booked once it has started
///   src/Booking.fs
///   in Booking.book
///   - elif now >= starts then
///   + elif false then
///
/// `test:` names the project whose tests guard the file's breaks, and the
/// arguments they run with. Under a `##` heading a break names its file, may
/// name the binding its text is in (`in`), and has the text to swap: `-` lines
/// for the text as it is, `+` lines for the break. A `guarded by` line names
/// other tests for that one break, in the form `test:` takes.
module FsBreaks.Breaks

open System
open System.IO

/// The tests that guard a break: a project, built, and the arguments its tests run with.
type Guard = { Project: string; Args: string list }

type Break =
    { Heading: string
      File: string
      /// The binding the text is in, as `Module.function`: the text need
      /// only be found once there.
      Anchor: string option
      Old: string
      New: string
      /// Other tests for this break than the file's.
      GuardedBy: Guard option }

type BreaksFile =
    { Name: string
      Test: Guard option
      Breaks: Break list }

/// A guard from its line: the project, then the arguments. `{name}` in an
/// argument is the breaks file's name.
let guardOf (line: string) =
    match Text.words line with
    | Ok(project :: args) -> Ok { Project = project; Args = args }
    | Ok [] -> Error "it names no project"
    | Error e -> Error e

let private after (mark: string) (line: string) =
    if line.StartsWith mark then Some(line.Substring mark.Length)
    elif line = mark.TrimEnd() then Some ""
    else None

let private breakOf (heading: string) (lines: string list) =
    let marked mark =
        match lines |> List.choose (after mark) with
        | [] -> None
        | texts -> Some(String.concat "\n" texts)

    let isText (l: string) = (after "- " l).IsSome || (after "+ " l).IsSome
    let rest = lines |> List.filter (not << isText)
    let anchors = rest |> List.choose (after "in ")
    let guards = rest |> List.choose (after "guarded by ")

    let files =
        rest
        |> List.filter (fun l -> (after "in " l).IsNone && (after "guarded by " l).IsNone)

    let fail why = Error $"\"{heading}\": {why}"

    match files, anchors, guards, marked "- ", marked "+ " with
    | [], _, _, _, _ -> fail "it needs the file the text is in"
    | _ :: _ :: _, _, _, _, _ -> fail $"""one file only, not {String.concat ", " files}"""
    | _, _ :: _ :: _, _, _, _ -> fail "one `in` line only"
    | _, _, _ :: _ :: _, _, _ -> fail "one `guarded by` line only"
    | _, _, _, None, _ -> fail "it needs - lines: the text as it is"
    | _, _, _, _, None -> fail "it needs + lines: the text that breaks the rule"
    | [ file ], anchor, guard, Some o, Some n ->
        let guarded =
            match guard with
            | [] -> Ok None
            | g :: _ -> guardOf g |> Result.map Some |> Result.mapError (fun e -> $"\"{heading}\": guarded by: {e}")

        guarded
        |> Result.map (fun g ->
            { Heading = heading
              File = file
              Anchor = List.tryHead anchor |> Option.map _.Trim()
              Old = o
              New = n
              GuardedBy = g })

let private isNote (line: string) = line.StartsWith "#" && not (line.StartsWith "## ")

/// Reads a breaks file's text.
let parse (name: string) (text: string) : Result<BreaksFile, string list> =
    let lines =
        text.Split '\n'
        |> Array.map _.TrimEnd()
        |> Array.filter (fun l -> l <> "" && not (isNote l))
        |> List.ofArray

    let header = lines |> List.takeWhile (fun l -> not (l.StartsWith "## "))
    let body = lines |> List.skip header.Length

    let test =
        match header with
        | [] -> Ok None
        | [ line ] ->
            match after "test: " line with
            | Some guard -> guardOf guard |> Result.map Some |> Result.mapError (fun e -> [ $"test: {e}" ])
            | None -> Error [ $"\"{line}\" comes before any ## heading" ]
        | _ -> Error [ $"""before the first ## heading only one "test:" line goes, not: {String.concat " / " header}""" ]

    let groups =
        body
        |> List.fold
            (fun groups (line: string) ->
                match after "## " line, groups with
                | Some heading, _ -> (heading.Trim(), []) :: groups
                | None, (heading, ls) :: rest -> (heading, line :: ls) :: rest
                | None, [] -> groups)
            []
        |> List.rev
        |> List.map (fun (heading, ls) -> breakOf heading (List.rev ls))

    let errors =
        (match test with Error e -> e | Ok _ -> [])
        @ (groups |> List.choose (function Error e -> Some e | Ok _ -> None))

    match test, errors with
    | Ok t, [] ->
        Ok
            { Name = name
              Test = t
              Breaks = groups |> List.choose (function Ok b -> Some b | Error _ -> None) }
    | _ -> Error errors

/// Reads a breaks file from disk; its name is the file's, without `.breaks`.
let read (path: string) =
    parse (Path.GetFileNameWithoutExtension path) (File.ReadAllText path)
    |> Result.mapError (List.map (fun e -> $"{path}: {e}"))
