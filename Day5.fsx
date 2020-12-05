#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = lines input

let rowMultiple (c: char) = if c = 'F' then 0 else 1

let columnMultiple (c: char) = if c = 'R' then 1 else 0

let values = [ 64; 32; 16; 8; 4; 2; 1 ]
let columnValues = [ 4; 2; 1 ]

let row (input: string) =
    input
    |> Seq.map rowMultiple
    |> Seq.indexed
    |> Seq.sumBy (fun (i, s) -> values.[i] * s)

let column (input: string) =
    input
    |> Seq.map columnMultiple
    |> Seq.indexed
    |> Seq.sumBy (fun (i, s) -> columnValues.[i] * s)

let seatId (input: string) =
    let row = row input.[..6]
    let column = column input.[7..]
    row * 8 + column

let common input = input |> parse |> Array.map seatId
let part1 input = input |> common |> Array.max

// manually inspect this instead of logic of front vs back row
let part2 input =
    let complete = [ 0 .. 1024 ] |> set
    Set.difference complete (input |> common |> set)
