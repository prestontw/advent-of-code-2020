#load "Common.fsx"
#load "Inputs.fsx"
open Common

type Num = uint64

type Operation =
    | Add
    | Mult

type Expr<'a> =
    | Op of Operation * Expr<'a> * Expr<'a>
    | Atom of 'a

let rec interp =
    function
    | Atom a -> a
    | Op (Add, lhs, rhs) -> (interp lhs) + (interp rhs)
    | Op (Mult, lhs, rhs) -> (interp lhs) * (interp rhs)

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
        | "+" :: tail -> inner lhs (Some(Add)) tail
        | "*" :: tail -> inner lhs (Some(Mult)) tail
        | num :: tail ->
            let a = num |> uint64 |> Atom
            match lhs with
            | Some lhs -> inner (Some(Op(Option.get op, lhs, a))) None tail
            | None -> inner (Some a) None tail

    inner None None line

let infixBindingPower =
    function
    | Add -> (3, 4)
    | Mult -> (1, 2)

let parseLine2 line =
    let rec inner remaining minBp =

        let mutable lhs, remaining =
            match remaining with
            | "(" :: remaining ->
                let lhs, remaining = inner remaining 0
                lhs, remaining |> List.tail
            | a :: remaining -> (a |> uint64 |> Atom, remaining)

        let mutable shouldContinue = true
        while shouldContinue do

            let op =
                match remaining with
                | "+" :: _ -> Add
                | "*" :: _ -> Mult
                | _ ->
                    shouldContinue <- false
                    Add

            let (lbp, rbp) = infixBindingPower op
            if shouldContinue then
                if lbp < minBp then
                    shouldContinue <- false
                else
                    let (rhs, newRemaining) = inner (List.tail remaining) rbp
                    lhs <- Op(op, lhs, rhs)
                    remaining <- newRemaining
        lhs, remaining

    inner line 0

let parse f input =
    input
    |> lines
    |> Array.map (tokenizeLine >> f)
    |> Array.toList


let part1 input =
    input
    |> parse parseLine
    |> List.sumBy (fst >> interp)

let part2 input =
    input
    |> parse parseLine2
    |> List.sumBy (fst >> interp)

let input = Inputs.day18

"(1 + 2) * 3 + 4"
|> tokenizeLine
|> parseLine2
|> fst
|> interp
