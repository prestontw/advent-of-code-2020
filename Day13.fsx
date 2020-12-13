#load "Inputs.fsx"
#load "Common.fsx"
open Common

type BusId =
    | X
    | Minutes of uint64

let cToBusId =
    function
    | "x" -> X
    | a -> Minutes(a |> uint64)

let parse input =
    let lines = input |> lines
    lines.[0] |> uint64, lines.[1] |> commas |> Array.map cToBusId

let difference id earliest =
    let nextTime = ((earliest / id) + 1UL) * id
    nextTime - earliest

let final id minutesWait = id * minutesWait

let part1 input =
    let (earliest, ids) = input |> parse

    let (id, diff) =
        ids
        |> Seq.choose (fun id ->
            match id with
            | X -> None
            | Minutes a -> Some a)
        |> Seq.map (fun id -> id, difference id earliest)
        |> Seq.minBy snd

    final id diff

let input = Inputs.day13

let part2 input =
    let (_, ids) = input |> parse

    let goals =
        ids
        |> Seq.indexed
        |> Seq.choose (fun (idx, i) ->
            match i with
            | X -> None
            | Minutes a -> Some(idx, a))
        |> Array.ofSeq

    let rec attempt multiple =
        let current = (snd goals.[0] |> uint64) * multiple
        if goals.[1..]
           |> Array.forall (fun (offset, id) -> difference id current = (offset |> uint64)) then
            current
        else
            attempt (multiple + 1UL)

    attempt 1UL

let sample = "939
7,13,x,x,59,x,31,19"

part2 sample
