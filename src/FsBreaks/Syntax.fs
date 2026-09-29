/// What FsBreaks knows of F# source: its bindings, by name, and where they are.
module FsBreaks.Syntax

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

let checker = lazy (FSharpChecker.Create())

let private errorsOf (diagnostics: FSharpDiagnostic array) =
    diagnostics
    |> Array.filter (fun d -> d.Severity = FSharpDiagnosticSeverity.Error)
    |> Array.map (fun d -> $"({d.StartLine},{d.StartColumn + 1}) {d.Message}")
    |> List.ofArray

/// The file's untyped tree, or its syntax errors.
let parse (path: string) (source: string) : Result<ParsedInput, string list> =
    let options = { FSharpParsingOptions.Default with SourceFiles = [| path |] }

    let result =
        checker.Value.ParseFile(path, SourceText.ofString source, options)
        |> Async.RunSynchronously

    match errorsOf result.Diagnostics with
    | [] -> Ok result.ParseTree
    | errors -> Error errors

/// A named binding: a let, a local let, or a member, with the names it is
/// inside of (namespace, modules, types, bindings) and where it is.
type Binding = { Path: string list; Range: range }

let private idents (ids: LongIdent) = ids |> List.map _.idText

let rec private patternName pat =
    match pat with
    | SynPat.Named(ident = SynIdent(id, _)) -> Some id.idText
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) -> ids |> List.tryLast |> Option.map _.idText
    | SynPat.Paren(pat, _)
    | SynPat.Typed(pat, _, _)
    | SynPat.Attrib(pat, _, _) -> patternName pat
    | _ -> None

let private nameOf (SynBinding(headPat = pat)) = patternName pat

let private scopeOf (node: SyntaxNode) =
    match node with
    | SyntaxNode.SynModuleOrNamespace(SynModuleOrNamespace(longId = ids)) -> idents ids
    | SyntaxNode.SynModule(SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(longId = ids))) -> idents ids
    | SyntaxNode.SynTypeDefn(SynTypeDefn(typeInfo = SynComponentInfo(longId = ids))) -> idents ids
    | SyntaxNode.SynBinding b -> nameOf b |> Option.toList
    | _ -> []

/// The names a node is inside of, outermost first.
let scope (path: SyntaxVisitorPath) = path |> List.rev |> List.collect scopeOf

/// Every named binding in the tree, in source order.
let bindings (tree: ParsedInput) =
    tree
    |> ParsedInput.fold
        (fun found path node ->
            match node with
            | SyntaxNode.SynBinding b ->
                match nameOf b with
                | Some name ->
                    { Path = scope path @ [ name ]
                      Range = b.RangeOfBindingWithRhs }
                    :: found
                | None -> found
            | _ -> found)
        []
    |> List.rev

let private names (anchor: string) = anchor.Split '.' |> List.ofArray

/// Whether the binding is the one the anchor names: its path ends with the anchor's names.
let isNamed (anchor: string) (b: Binding) =
    let wanted = names anchor
    b.Path.Length >= wanted.Length && List.skip (b.Path.Length - wanted.Length) b.Path = wanted

/// The shortest name that tells the binding from every other in the file.
let shortName (all: Binding list) (b: Binding) =
    let dotted (path: string list) = String.concat "." path

    [ 1 .. b.Path.Length ]
    |> List.map (fun n -> List.skip (b.Path.Length - n) b.Path |> dotted)
    |> List.tryFind (fun name -> all |> List.filter (isNamed name) |> List.length = 1)
    |> Option.defaultValue (dotted b.Path)

/// Offsets of a range in the source.
let span (source: string) (r: range) =
    let starts = Text.lineStarts source
    let at (p: pos) = starts[p.Line - 1] + p.Column
    at r.Start, at r.End

/// The binding the anchor names in the file.
let find (file: string) (tree: ParsedInput) (anchor: string) =
    match bindings tree |> List.filter (isNamed anchor) with
    | [ b ] -> Ok b
    | [] -> Error $"no binding {anchor} in {file}"
    | many ->
        let where = many |> List.map (fun b -> $"""{String.concat "." b.Path} (line {b.Range.StartLine})""")
        Error $"""{anchor} names {many.Length} bindings in {file}: {String.concat ", " where}"""

/// Where the text is in the source: found once in the binding the anchor
/// names, or once in the whole file without one.
let locate (file: string) (source: string) (anchor: string option) (text: string) : Result<int, string> =
    let region =
        match anchor with
        | None -> Ok(0, source.Length, file)
        | Some anchor ->
            match parse file source with
            | Error errors -> Error $"""{file} does not parse: {List.head errors}"""
            | Ok tree ->
                find file tree anchor
                |> Result.map (fun b ->
                    let from, until = span source b.Range
                    from, until, $"{anchor} in {file}")

    region
    |> Result.bind (fun (from, until, where) ->
        match Text.occurrences source text from until with
        | [ at ] -> Ok at
        | found -> Error $"stale: the text is found {found.Length} times in {where}")

/// The source with the break's text swapped in, or why it cannot be.
let apply (file: string) (source: string) (anchor: string option) (old: string) (replacement: string) =
    let old = Text.withLineEndsOf source old
    let replacement = Text.withLineEndsOf source replacement

    locate file source anchor old
    |> Result.map (fun at -> source.Substring(0, at) + replacement + source.Substring(at + old.Length))
