/// FsBreaks on a small project of its own: a library, its Expecto tests, and
/// breaks that are caught, missed, stale and do not build.
module FsBreaks.Tests.SampleTests

open System
open System.IO
open Expecto
open FsBreaks

let private rules =
    """module Rules

/// Whether a mat can be booked.
let canBook (seats: int) (taken: int) (started: bool) =
    if started then false
    elif taken < seats then true
    else false

let price (age: int) =
    match age with
    | a when a < 18 -> 50
    | _ -> 100
"""

let private tests =
    """module Tests

open Expecto

[<Tests>]
let tests =
    testList
        "rules"
        [ test "a free mat is booked" { Expect.isTrue (Rules.canBook 10 3 false) "free" }
          test "a full class is not booked" { Expect.isFalse (Rules.canBook 10 10 false) "full" }
          test "children pay half" { Expect.equal (Rules.price 12) 50 "a child" } ]

[<EntryPoint>]
let main argv = Tests.runTestsInAssemblyWithCLIArgs [] argv
"""

let private breaks =
    """# The sample's rules.
test: test/Rules.Tests --filter rules

## A full class is not booked
src/Rules/Rules.fs
in canBook
- elif taken < seats then
+ elif true then

## A class that has started is not booked
src/Rules/Rules.fs
- if started then false
+ if false then false

## Children pay half
src/Rules/Rules.fs
in price
- a < 18
+ false

## A rule whose code has moved on
src/Rules/Rules.fs
- if closed then
+ if false then

## A break that does not build
src/Rules/Rules.fs
- elif taken < seats then
+ elif taken then
"""

let private write (root: string) (path: string) (text: string) =
    let full = Path.Combine(root, path)
    Directory.CreateDirectory(Path.GetDirectoryName full) |> ignore
    File.WriteAllText(full, text)

/// The sample in a folder of its own, away from this repository's settings.
let private sample () =
    let root = Path.Combine(Path.GetTempPath(), $"fsbreaks-sample-{Guid.NewGuid():N}")
    write root "fsbreaks.json" """{ "breaks": "breaks" }"""
    write root "breaks/rules.breaks" breaks
    write root "src/Rules/Rules.fs" rules

    write
        root
        "src/Rules/Rules.fsproj"
        """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><TargetFramework>net10.0</TargetFramework></PropertyGroup>
  <ItemGroup><Compile Include="Rules.fs" /></ItemGroup>
</Project>"""

    write root "test/Rules.Tests/Tests.fs" tests

    write
        root
        "test/Rules.Tests/Rules.Tests.fsproj"
        """<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup><OutputType>Exe</OutputType><TargetFramework>net10.0</TargetFramework></PropertyGroup>
  <ItemGroup><Compile Include="Tests.fs" /></ItemGroup>
  <ItemGroup><PackageReference Include="Expecto" Version="10.2.3" /></ItemGroup>
  <ItemGroup><ProjectReference Include="../../src/Rules/Rules.fsproj" /></ItemGroup>
</Project>"""

    root

let private configOf root =
    match Config.load root with
    | Ok config -> config
    | Error e -> failtestf "%s" e

let private lines () =
    let found = ResizeArray<string>()
    found, (fun (line: string) -> lock found (fun () -> found.Add line))

let private reported (log: ResizeArray<string>) =
    log
    |> Seq.map _.Trim()
    |> Seq.filter (fun l -> [ "caught"; "MISSED"; "STALE"; "NO BUILD"; "ok" ] |> List.exists l.StartsWith)
    |> List.ofSeq

[<Tests>]
let sampleTests =
    testList
        "a sample project"
        [ testCase "each break is tried, and the file is as it was after"
          <| fun () ->
              let root = sample ()
              let before = File.ReadAllBytes(Path.Combine(root, "src/Rules/Rules.fs"))
              let log, say = lines ()

              let problems = Runner.run (configOf root) [] None say

              Expect.equal
                  (reported log)
                  [ "caught   A full class is not booked"
                    "MISSED   A class that has started is not booked"
                    "caught   Children pay half"
                    "STALE    A rule whose code has moved on"
                    "NO BUILD A break that does not build" ]
                  (String.concat "\n" log)

              Expect.contains log "           by a full class is not booked" "the failed test, named without the filter"
              Expect.equal problems 3 "missed, stale and not building"
              Expect.equal (File.ReadAllBytes(Path.Combine(root, "src/Rules/Rules.fs"))) before "the file is as it was"
              Expect.isFalse (File.Exists(Path.Combine(root, ".fsbreaks/restore.json"))) "nothing left to put back"
              Directory.Delete(root, true)

          testCase "check finds each break's text and type-checks it, without building"
          <| fun () ->
              let root = sample ()
              let restored = Runner.dotnet (configOf root) [ "restore"; "test/Rules.Tests" ] (TimeSpan.FromMinutes 5.)
              Expect.equal (restored |> Option.map fst) (Some 0) "restored"
              let log, say = lines ()

              let problems = Check.check (configOf root) [] None say

              Expect.equal
                  (reported log)
                  [ "ok       A full class is not booked"
                    "ok       A class that has started is not booked"
                    "ok       Children pay half"
                    "STALE    A rule whose code has moved on"
                    "NO BUILD A break that does not build" ]
                  (String.concat "\n" log)

              Expect.equal problems 2 "stale and not type-checking"
              Directory.Delete(root, true)

          testCase "a file a stopped run left broken is put back by the next"
          <| fun () ->
              let root = sample ()
              let file = Path.Combine(root, "src/Rules/Rules.fs")
              let original = File.ReadAllBytes file
              Directory.CreateDirectory(Path.Combine(root, ".fsbreaks")) |> ignore

              File.WriteAllText(
                  Path.Combine(root, ".fsbreaks/restore.json"),
                  $$"""{"file":"src/Rules/Rules.fs","bytes":"{{Convert.ToBase64String original}}"}"""
              )

              File.WriteAllText(file, "broken")
              let log, say = lines ()

              Runner.recover (configOf root) say

              Expect.equal (File.ReadAllBytes file) original "put back"
              Expect.equal (List.ofSeq log) [ "src/Rules/Rules.fs was left broken by a run that was stopped: it is put back." ] "told"
              Directory.Delete(root, true) ]
