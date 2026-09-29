/// Candidate breaks: the places a function decides something, each with a
/// swap that turns the decision off. A person keeps the ones that break a
/// rule, and names the rule.
module FsBreaks.Suggest

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type Candidate =
    { /// The binding the text is found once in, when it is inside one.
      Anchor: string option
      Old: string
      New: string
      /// What is swapped, in short: the part of the text, and what replaces it.
      Swap: string * string
      Line: int }

/// Comparisons and the one that says the opposite.
let private opposites =
    Map
        [ "op_LessThan", ">="
          "op_LessThanOrEqual", ">"
          "op_GreaterThan", "<="
          "op_GreaterThanOrEqual", "<"
          "op_Equality", "<>"
          "op_Inequality", "=" ]

let private operatorOf (expr: SynExpr) =
    match expr with
    | SynExpr.Ident id
    | SynExpr.LongIdent(longDotId = SynLongIdent(id = [ id ])) -> Some(id.idText, expr.Range)
    | _ -> None

/// The operator of an infix application, `a op b`, and its two sides.
let private infix (expr: SynExpr) =
    match expr with
    | SynExpr.App(funcExpr = SynExpr.App(isInfix = true; funcExpr = op; argExpr = left); argExpr = right) ->
        operatorOf op |> Option.map (fun (name, at) -> name, at, left, right)
    | _ -> None

let private isChain name =
    name = "op_BooleanAnd" || name = "op_BooleanOr"

/// The requirements of an `&&` (or `||`) chain, one by one.
let rec private operands name (expr: SynExpr) =
    match infix expr with
    | Some(n, _, left, right) when n = name -> operands name left @ operands name right
    | _ -> [ expr ]

let private same (a: SynExpr) (b: SynExpr) = Range.equals a.Range b.Range

/// The chain the node is an operand of, if it is one.
let private chainAbove (path: SyntaxVisitorPath) (expr: SynExpr) =
    match path with
    // The left side: `a &&` is an application of its own.
    | SyntaxNode.SynExpr(SynExpr.App(isInfix = true; funcExpr = op; argExpr = left)) :: _ when same left expr ->
        operatorOf op |> Option.map fst |> Option.filter isChain
    | SyntaxNode.SynExpr parent :: _ ->
        match infix parent with
        | Some(name, _, _, right) when isChain name && same right expr -> Some name
        | _ -> None
    | _ -> None

/// Whether the node is an if's or a when's condition.
let private isCondition (path: SyntaxVisitorPath) (expr: SynExpr) =
    match path with
    | SyntaxNode.SynExpr(SynExpr.IfThenElse(ifExpr = condition)) :: _ -> same condition expr
    | SyntaxNode.SynMatchClause(SynMatchClause(whenExpr = Some guard)) :: _ -> same guard expr
    | _ -> false

/// Whether the node is broken on its own, not as a condition or a chain's part.
let private standsAlone path expr =
    not (isCondition path expr) && (chainAbove path expr).IsNone

/// The swaps a node offers: the range of the text that changes, the part of
/// it replaced, and what goes there.
let private swapsOf (source: string) (path: SyntaxVisitorPath) (node: SyntaxNode) : (range * range * string) list =
    let text (r: range) =
        let from, until = Syntax.span source r
        source.Substring(from, until - from)

    match node with
    // An if's branch, never taken.
    | SyntaxNode.SynExpr(SynExpr.IfThenElse(ifExpr = condition)) -> [ condition.Range, condition.Range, "false" ]
    // A match case's guard, never passed.
    | SyntaxNode.SynMatchClause(SynMatchClause(whenExpr = Some guard)) -> [ guard.Range, guard.Range, "false" ]
    | SyntaxNode.SynExpr expr ->
        match infix expr with
        // A chain, one requirement dropped at a time. `a && b && c` is
        // `(a && b) && c`: the inner chain offers the same swaps again, and
        // they are dropped as the same candidates.
        | Some(name, _, _, _) when isChain name ->
            let dropped = if name = "op_BooleanAnd" then "true" else "false"
            operands name expr |> List.map (fun part -> part.Range, part.Range, dropped)
        // A comparison that stands alone, turned around.
        | Some(name, at, _, _) when opposites.ContainsKey name && standsAlone path expr -> [ expr.Range, at, opposites[name] ]
        | _ ->
            match expr with
            // `not x` that stands alone, as `x`.
            | SynExpr.App(isInfix = false; funcExpr = SynExpr.Ident id; argExpr = arg) when
                id.idText = "not" && standsAlone path expr
                ->
                [ expr.Range, expr.Range, text arg.Range ]
            | _ -> []
    | _ -> []

/// The lines a range is on, whole.
let private wholeLines (source: string) (from: int) (until: int) =
    let start = source.LastIndexOf('\n', max 0 (from - 1)) + 1
    let start = if from = 0 then 0 else start

    let finish =
        match source.IndexOf('\n', until) with
        | -1 -> source.Length
        | i when i > 0 && source[i - 1] = '\r' -> i - 1
        | i -> i

    start, finish

/// Candidate breaks in the file, in the binding named when one is.
let candidates (file: string) (source: string) (within: string option) : Result<Candidate list, string> =
    match Syntax.parse file source with
    | Error errors -> Error $"""{file} does not parse: {List.head errors}"""
    | Ok tree ->
        let all = Syntax.bindings tree

        let scope =
            match within with
            | None -> Ok None
            | Some name -> Syntax.find file tree name |> Result.map Some

        scope
        |> Result.map (fun scope ->
            let inScope (r: range) =
                scope |> Option.forall (fun (b: Syntax.Binding) -> Range.rangeContainsRange b.Range r)

            let innermost (r: range) =
                all
                |> List.filter (fun b -> Range.rangeContainsRange b.Range r)
                |> List.sortBy (fun b -> b.Range.EndLine - b.Range.StartLine, b.Range.EndColumn - b.Range.StartColumn)
                |> List.tryHead

            let candidate (whole: range, swapped: range, replacement: string) =
                let from, until = Syntax.span source whole
                let swapFrom, swapUntil = Syntax.span source swapped
                let binding = innermost whole

                let region =
                    match binding with
                    | Some b -> Syntax.span source b.Range
                    | None -> 0, source.Length

                let once (a: int, b: int) =
                    let old = source.Substring(a, b - a)
                    Text.occurrences source old (fst region) (snd region) |> List.length = 1

                let swap (a: int, b: int) =
                    let old = source.Substring(a, b - a)
                    let broken = source.Substring(a, swapFrom - a) + replacement + source.Substring(swapUntil, b - swapUntil)

                    { Anchor = binding |> Option.map (Syntax.shortName all)
                      Old = old
                      New = broken
                      Swap = source.Substring(swapFrom, swapUntil - swapFrom), replacement
                      Line = whole.StartLine }

                // Where the text is not found once, its lines are, as far as
                // they are in the binding.
                let lines =
                    let a, b = wholeLines source from until
                    max a (fst region), min b (snd region)

                if once (from, until) then Some(swap (from, until))
                elif once lines then Some(swap lines)
                else None

            tree
            |> ParsedInput.fold (fun found path node -> List.rev (swapsOf source path node) @ found) []
            |> List.rev
            |> List.filter (fun (whole, _, _) -> inScope whole)
            |> List.choose candidate
            |> List.distinctBy (fun c -> c.Anchor, c.Old, c.New))

let private oneLine (text: string) =
    match text.Split '\n' |> Array.map _.Trim() with
    | [| line |] -> line
    | lines -> lines[0] + " …"

/// The candidates as a breaks file's text, to copy from.
let render (file: string) (found: Candidate list) =
    let lines (mark: string) (text: string) =
        text.Replace("\r\n", "\n").Split '\n' |> Array.map (fun l -> mark + l) |> List.ofArray

    let one (c: Candidate) =
        [ yield ""
          yield $"""## {c.Anchor |> Option.map (fun a -> a + ": ") |> Option.defaultValue ""}{oneLine (fst c.Swap)} → {oneLine (snd c.Swap)}"""
          yield $"# line {c.Line}"
          yield file.Replace('\\', '/')
          match c.Anchor with
          | Some a -> yield $"in {a}"
          | None -> ()
          yield! lines "- " c.Old
          yield! lines "+ " c.New ]

    [ yield $"# Candidate breaks for {file}: each switches off something the code decides."
      yield "# Keep the ones that break a rule your tests should catch, and name the rule"
      yield "# in the heading. The rest decide nothing a test could see, or are not worth it."
      yield! found |> List.collect one ]
    |> String.concat "\n"
