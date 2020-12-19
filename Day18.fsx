#load "Common.fsx"
#load "Inputs.fsx"
open Common

type Num = uint64

type Expr<'a> =
    | Op of ('a -> 'a -> 'a) * Expr<'a> * Expr<'a>
    | Atom of 'a

let rec interp =
    function
    | Atom a -> a
    | Op (op, lhs, rhs) -> op (interp lhs) (interp rhs)

let removeBeginParen (s: string) =
    let count =
        s |> Seq.filter (fun c -> c = '(') |> Seq.length

    Array.append (Array.create count "(") [| s.[count..] |]

let removeEndParen (s: string) =
    let count =
        s |> Seq.filter (fun c -> c = ')') |> Seq.length

    Array.append [| s.[..s.Length - count - 1] |] (Array.create count ")")

let tokenizeLine line =
    line
    |> spaces
    |> Array.collect removeBeginParen
    |> Array.collect removeEndParen
    |> Array.toList

let parseLine line =
    let rec inner lhs op remaining =
        match remaining with
        | [] -> Option.get lhs, []
        | "(" :: tail ->
            let parens, after = inner None None tail
            match lhs with
            | Some lhs -> inner (Some(Op((op |> Option.get), lhs, parens))) None after
            | None -> inner (Some(parens)) None after
        | ")" :: tail -> Option.get lhs, tail
        | "+" :: tail -> inner lhs (Some(+)) tail
        | "*" :: tail -> inner lhs (Some(*)) tail
        | num :: tail ->
            let a = num |> uint64 |> Atom
            match lhs with
            | Some lhs -> inner (Some(Op(Option.get op, lhs, a))) None tail
            | None -> inner (Some a) None tail

    inner None None line


let parse input =
    input
    |> lines
    |> Array.map (tokenizeLine >> parseLine)
    |> Array.toList


let part1 input =
    input |> parse |> List.sumBy (fst >> interp)

let input = Inputs.day18

"(1 + 2) * 3 + 4"
|> tokenizeLine
|> parseLine
|> fst
|> interp
