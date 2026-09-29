module FsBreaks.Tests.BreaksTests

open Expecto
open FsBreaks.Breaks

let private parsed text =
    match parse "rules" text with
    | Ok file -> file
    | Error errors -> failtestf "%A" errors

let private errorsOf text =
    match parse "rules" text with
    | Ok file -> failtestf "expected errors, read %A" file
    | Error errors -> errors

[<Tests>]
let tests =
    testList
        "breaks file"
        [ test "a break has a heading, a file, the text as it is and the break" {
              let file =
                  parsed
                      """
# A note.
## A full class is not booked
src/Booking.fs
- elif taken < seats then
+ elif true then
"""

              Expect.equal
                  file.Breaks
                  [ { Heading = "A full class is not booked"
                      File = "src/Booking.fs"
                      Anchor = None
                      Old = "elif taken < seats then"
                      New = "elif true then"
                      GuardedBy = None } ]
                  "the break"

              Expect.equal file.Test None "no test line"
          }

          test "the file's tests, a break's anchor and its own tests" {
              let file =
                  parsed
                      """
test: tests/Rules.Tests --filter "Booking rules" --summary

## A class that has started is not booked
src/Booking.fs
in Booking.book
- if started then
+ if false then
guarded by tests/Slow.Tests --filter {name}
"""

              Expect.equal
                  file.Test
                  (Some
                      { Project = "tests/Rules.Tests"
                        Args = [ "--filter"; "Booking rules"; "--summary" ] })
                  "the file's tests, with a quoted argument"

              let b = List.exactlyOne file.Breaks
              Expect.equal b.Anchor (Some "Booking.book") "the anchor"

              Expect.equal
                  b.GuardedBy
                  (Some
                      { Project = "tests/Slow.Tests"
                        Args = [ "--filter"; "{name}" ] })
                  "its own tests"
          }

          test "several - and + lines are one text each, blank lines kept, indentation too" {
              let file =
                  parsed
                      """
## Two lines
src/A.fs
-     if a then
-
-         b
+     if false then
+
+         b
"""

              let b = List.exactlyOne file.Breaks
              Expect.equal b.Old "    if a then\n\n        b" "the old text"
              Expect.equal b.New "    if false then\n\n        b" "the new text"
          }

          test "what a break lacks is told, heading by heading" {
              let errors =
                  errorsOf
                      """
## No file
- a
+ b

## No break
src/A.fs
- a

## Two files
src/A.fs
src/B.fs
- a
+ b
"""

              Expect.equal
                  errors
                  [ "\"No file\": it needs the file the text is in"
                    "\"No break\": it needs + lines: the text that breaks the rule"
                    "\"Two files\": one file only, not src/A.fs, src/B.fs" ]
                  "each problem"
          }

          test "only a test line goes before the first heading" {
              let errors =
                  errorsOf
                      """
src/A.fs
## A
src/A.fs
- a
+ b
"""

              Expect.equal errors [ "\"src/A.fs\" comes before any ## heading" ] "the stray line"
          }

          test "an unclosed quote is told" {
              let errors = errorsOf "test: tests/A --filter \"open\n"
              Expect.equal errors [ "test: a quote is not closed in: tests/A --filter \"open" ] "the quote"
          } ]
