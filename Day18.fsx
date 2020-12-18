#load "Common.fsx"
#load "Inputs.fsx"
open Common


let removeBeginParen (s: string) =
    let count =
        s |> Seq.filter (fun c -> c = '(') |> Seq.length

    Array.append (Array.create count "(") [| s.[count..] |]

let removeEndParen (s: string) =
    let count =
        s |> Seq.filter (fun c -> c = ')') |> Seq.length

    Array.append [| s.[..s.Length - count - 1] |] (Array.create count ")")

let parseLine line =
    line
    |> spaces
    |> Array.collect removeBeginParen
    |> Array.collect removeEndParen
    |> Array.toList

let parse input =
    input
    |> lines
    |> Array.map parseLine
    |> Array.toList

let eval line =
    let rec evalNext remaining =
        match remaining with
        | "(" :: tail -> evalGroup None tail
        | a :: tail -> a |> uint64, tail

    and evalGroup acc remaining =
        // printfn "%A, %A" acc remaining
        match acc, remaining with
        | Some a, [] -> a, []
        | Some a, "+" :: tail ->
            let (result, remaining) = evalNext tail
            evalGroup (Some(a + result)) remaining
        | Some a, "*" :: tail ->
            let (result, remaining) = evalNext tail
            evalGroup (Some(a * result)) remaining
        | None, "(" :: tail ->
            let (paramVal, remaining) = evalGroup None tail
            evalGroup (Some paramVal) remaining
        | Some a, ")" :: tail -> (a, tail)
        | None, a :: tail ->
            let a = a |> uint64
            evalGroup (Some a) tail

    evalGroup None line


let part1 input =
    input |> parse |> List.sumBy (eval >> fst)

let input = Inputs.day18

"(1 + 2) * 3 + 4" |> parseLine |> eval
