module FsBreaks.Tests.RunnerTests

open System
open Expecto
open FsBreaks

let private config =
    { Config.Root = "/repo"
      Config.Breaks = "breaks"
      Config.Test = None
      Config.Env = Map.empty
      Config.Timeout = TimeSpan.FromMinutes 1. }

let private file test breaks : Breaks.BreaksFile =
    { Name = "booking"
      Test = test
      Breaks = breaks }

let private aBreak guard : Breaks.Break =
    { Heading = "A rule"
      File = "src/A.fs"
      Anchor = None
      Old = "a"
      New = "b"
      GuardedBy = guard }

[<Tests>]
let tests =
    testList
        "runner"
        [ test "failed tests are named, as Expecto, dotnet test and Microsoft.Testing.Platform write them" {
              let out =
                  String.concat
                      "\n"
                      [ "[12:00:01 ERR] Booking/a full class is not booked failed in 00:00:00.0100000."
                        "[12:00:01 ERR] Booking/a crash errored in 00:00:00.0100000."
                        "  Failed Booking.Tests.startedClass [12 ms]"
                        "  Failed Booking.Tests.quick [< 1 ms]"
                        "failed Booking.Tests.mtp (3ms)" ]

              Expect.equal
                  (Runner.outcomeOf 1 out)
                  (Runner.Failed
                      [ "Booking/a full class is not booked"
                        "Booking/a crash"
                        "Booking.Tests.startedClass"
                        "Booking.Tests.quick"
                        "Booking.Tests.mtp" ])
                  "every failed test"
          }

          test "a failing run that names no test is told by its exit code" {
              Expect.equal (Runner.outcomeOf 3 "boom") (Runner.Failed [ "the tests exited with 3" ]) "the exit code"
          }

          test "a run with no test in it is told, whatever its exit code" {
              Expect.equal (Runner.outcomeOf 0 "EXPECTO! 0 tests run in 00:00:00 – 0 passed") Runner.RanNothing "Expecto"
              Expect.equal (Runner.outcomeOf 0 "No test matches the given testcase filter `X`") Runner.RanNothing "dotnet test"
              Expect.equal (Runner.outcomeOf 8 "Zero tests ran") Runner.RanNothing "Microsoft.Testing.Platform"
              Expect.equal (Runner.outcomeOf 0 "EXPECTO! 10 tests run in 00:00:01") Runner.Passed "ten tests"
          }

          test "a break's tests are its own, else its file's, else the settings', with the file's name" {
              let own = { Breaks.Project = "tests/Own"; Breaks.Args = [] }
              let files = { Breaks.Project = "tests/File"; Breaks.Args = [ "--filter"; "{name}" ] }
              let settings = { config with Test = Some "tests/All --filter \"{name} rules\"" }

              Expect.equal (Runner.guardFor settings (file (Some files) []) (aBreak (Some own))) (Ok own) "its own"

              Expect.equal
                  (Runner.guardFor settings (file (Some files) []) (aBreak None))
                  (Ok { files with Args = [ "--filter"; "booking" ] })
                  "its file's"

              Expect.equal
                  (Runner.guardFor settings (file None []) (aBreak None))
                  (Ok { Breaks.Project = "tests/All"; Breaks.Args = [ "--filter"; "booking rules" ] })
                  "the settings'"

              Expect.isError (Runner.guardFor config (file None []) (aBreak None)) "none"
          } ]
