/// Small helpers on text: command lines, positions, and finding a text once.
module FsBreaks.Text

open System
open System.Text

/// Splits a command line into words: spaces separate, double quotes group.
let words (line: string) : Result<string list, string> =
    let out = ResizeArray()
    let current = StringBuilder()
    let mutable quoted = false
    let mutable started = false

    for c in line do
        match c with
        | '"' ->
            quoted <- not quoted
            started <- true
        | c when Char.IsWhiteSpace c && not quoted ->
            if started then
                out.Add(string current)
                current.Clear() |> ignore
                started <- false
        | c ->
            current.Append c |> ignore
            started <- true

    if quoted then
        Error $"a quote is not closed in: {line}"
    else
        if started then
            out.Add(string current)

        Ok(List.ofSeq out)

/// Where each line of the text starts.
let lineStarts (text: string) =
    [| yield 0
       for i in 0 .. text.Length - 1 do
           if text[i] = '\n' then
               yield i + 1 |]

/// The offsets where `text` is found in `source`, wholly between `from` and `until`.
let occurrences (source: string) (text: string) (from: int) (until: int) =
    let rec find (at: int) found =
        match source.IndexOf(text, at, StringComparison.Ordinal) with
        | -1 -> List.rev found
        | i when i + text.Length > until -> List.rev found
        | i -> find (i + 1) (i :: found)

    if text = "" then [] else find from []

/// The text with the file's own line ends: a break is written with \n, the file may use \r\n.
let withLineEndsOf (source: string) (text: string) =
    if source.Contains "\r\n" then
        text.Replace("\r\n", "\n").Replace("\n", "\r\n")
    else
        text
