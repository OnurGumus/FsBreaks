module FsBreaks.Tests.SuggestTests

open Expecto
open FsBreaks

let private swaps within source =
    match Suggest.candidates "Rules.fs" source within with
    | Ok found -> found |> List.map (fun c -> c.Anchor, fst c.Swap, snd c.Swap)
    | Error e -> failtestf "%s" e

[<Tests>]
let tests =
    testList
        "suggest"
        [ test "an if's condition is never true" {
              let source =
                  """module Rules
let book started =
    if started then Error "started" else Ok()
"""

              Expect.equal (swaps None source) [ Some "book", "started", "false" ] "the condition"
          }

          test "a chain's requirements are dropped one at a time" {
              let source =
                  """module Rules
let canBook open' free late =
    open' && free > 0 && not late
let refund early admin = early || admin
"""

              Expect.equal
                  (swaps None source)
                  [ Some "canBook", "open'", "true"
                    Some "canBook", "free > 0", "true"
                    Some "canBook", "not late", "true"
                    Some "refund", "early", "false"
                    Some "refund", "admin", "false" ]
                  "each requirement, and nothing for its parts"
          }

          test "a comparison that stands alone is turned around, and so is not" {
              let source =
                  """module Rules
let isFull seats taken = taken >= seats
let isOpen closed = not closed
"""

              Expect.equal
                  (swaps None source)
                  [ Some "isFull", ">=", "<"; Some "isOpen", "not closed", "closed" ]
                  "turned around"
          }

          test "a match case's guard is never passed" {
              let source =
                  """module Rules
let price age =
    match age with
    | a when a < 18 -> 50
    | _ -> 100
"""

              Expect.equal (swaps None source) [ Some "price", "a < 18", "false" ] "the guard"
          }

          test "a text found more than once in its binding takes in its whole line" {
              let source =
                  """module Rules
let check a b =
    if a then 1
    elif b then (if a then 2 else 3)
    else 4
"""

              let found =
                  match Suggest.candidates "Rules.fs" source None with
                  | Ok found -> found
                  | Error e -> failtestf "%s" e

              Expect.equal
                  (found |> List.map (fun c -> c.Old, c.New))
                  [ "    if a then 1", "    if false then 1"
                    "    elif b then (if a then 2 else 3)", "    elif false then (if a then 2 else 3)"
                    "    elif b then (if a then 2 else 3)", "    elif b then (if false then 2 else 3)" ]
                  "whole lines where the text is not found once"
          }

          test "one binding's candidates, when one is named" {
              let source =
                  """module Rules
let book started = if started then 1 else 2
let cancel started = if started then 3 else 4
"""

              Expect.equal (swaps (Some "cancel") source) [ Some "cancel", "started", "false" ] "only cancel's"
          }

          test "the candidates are a breaks file the tool reads" {
              let source =
                  """module Rules
let book started full =
    if started then Error "started"
    elif full then Error "full"
    else Ok()
"""

              let text =
                  match Suggest.candidates "src/Rules.fs" source None with
                  | Ok found -> Suggest.render "src/Rules.fs" found
                  | Error e -> failtestf "%s" e

              match Breaks.parse "suggested" text with
              | Error e -> failtestf "%A" e
              | Ok file ->
                  Expect.equal
                      (file.Breaks |> List.map (fun b -> b.File, b.Anchor, b.Old, b.New))
                      [ "src/Rules.fs", Some "book", "    if started then Error \"started\"", "    if false then Error \"started\""
                        "src/Rules.fs", Some "book", "    elif full then Error \"full\"", "    elif false then Error \"full\"" ]
                      "read back"

                  for b in file.Breaks do
                      Expect.isOk (Syntax.apply b.File source b.Anchor b.Old b.New) $"{b.Heading} applies"
          } ]
