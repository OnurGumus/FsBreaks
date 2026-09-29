module FsBreaks.Tests.SyntaxTests

open Expecto
open FsBreaks

let source =
    """module Studio.Booking

let isFull seats taken = taken >= seats

let book now starts seats taken =
    let late = now >= starts
    if late then Error "started"
    elif isFull seats taken then Error "full"
    else Ok()

let cancel now starts =
    let late = now >= starts
    if late then Error "started" else Ok()

module Waitlist =
    let promote free waiting =
        if free > 0 then List.truncate free waiting else []

type Desk() =
    member this.Book(now, starts) =
        if now >= starts then Error "started" else Ok()
"""

let private paths () =
    match Syntax.parse "Booking.fs" source with
    | Ok tree -> Syntax.bindings tree |> List.map (fun b -> String.concat "." b.Path)
    | Error e -> failtestf "%A" e

[<Tests>]
let tests =
    testList
        "syntax"
        [ test "bindings are named by the modules, types and bindings they are in" {
              Expect.equal
                  (paths ())
                  [ "Studio.Booking.isFull"
                    "Studio.Booking.book"
                    "Studio.Booking.book.late"
                    "Studio.Booking.cancel"
                    "Studio.Booking.cancel.late"
                    "Studio.Booking.Waitlist.promote"
                    "Studio.Booking.Desk.Book" ]
                  "every binding"
          }

          test "a text found more than once in the file is found once in the binding named" {
              let old = "if late then"
              Expect.isError (Syntax.locate "Booking.fs" source None old) "twice in the file"

              let inCancel = Syntax.locate "Booking.fs" source (Some "cancel") old
              Expect.equal inCancel (Ok(source.LastIndexOf old)) "once in cancel"
          }

          test "an anchor names a binding by the end of its path" {
              let at = Syntax.locate "Booking.fs" source (Some "Booking.Waitlist.promote") "free > 0"
              Expect.equal at (Ok(source.IndexOf "free > 0")) "found in promote"

              let inMember = Syntax.locate "Booking.fs" source (Some "Desk.Book") "now >= starts"
              Expect.equal inMember (Ok(source.LastIndexOf "now >= starts")) "found in the member"
          }

          test "an anchor naming no binding, or several, is told" {
              Expect.equal
                  (Syntax.locate "Booking.fs" source (Some "refund") "x")
                  (Error "no binding refund in Booking.fs")
                  "no binding"

              Expect.equal
                  (Syntax.locate "Booking.fs" source (Some "late") "now")
                  (Error
                      "late names 2 bindings in Booking.fs: Studio.Booking.book.late (line 6), Studio.Booking.cancel.late (line 12)")
                  "two bindings"
          }

          test "a text not found once in its binding is stale" {
              Expect.equal
                  (Syntax.locate "Booking.fs" source (Some "book") "starts")
                  (Error "stale: the text is found 2 times in book in Booking.fs")
                  "twice in book"
          }

          test "a break keeps the file's line ends" {
              let crlf = source.Replace("\n", "\r\n")

              let broken =
                  Syntax.apply "Booking.fs" crlf (Some "book") "    if late then Error \"started\"\n    elif" "    if false then Error \"started\"\n    elif"

              Expect.equal
                  broken
                  (Ok(crlf.Replace("if late then Error \"started\"\r\n    elif", "if false then Error \"started\"\r\n    elif")))
                  "swapped with \\r\\n"
          }

          test "the shortest name tells a binding from the others" {
              match Syntax.parse "Booking.fs" source with
              | Error e -> failtestf "%A" e
              | Ok tree ->
                  let all = Syntax.bindings tree
                  let names = all |> List.map (Syntax.shortName all)
                  Expect.equal names [ "isFull"; "book"; "book.late"; "cancel"; "cancel.late"; "promote"; "Book" ] "short names"
          } ]
