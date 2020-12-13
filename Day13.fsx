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
    (nextTime - earliest) % id

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

let goals ids =
    ids
    |> Seq.indexed
    |> Seq.choose (fun (idx, i) ->
        match i with
        | X -> None
        | Minutes a -> Some(idx |> uint64, a))
    |> Array.ofSeq

let product l =
    l
    |> Array.choose (fun (idx, id) ->
        l
        |> Array.tryFind (fun (otherIdx, otherId) -> id = otherIdx))

let getCurrent idx id multiple = idx * id * multiple - idx

let attempt multiple (idx, id) goals =
    let current = getCurrent idx id multiple
    if goals
       |> Array.forall (fun (offset, id) -> difference id current = offset) then
        Some current
    else
        None

let part2 minimum input =
    let (_, ids) = input |> parse

    let goals = ids |> goals

    let prods =
        goals
        |> product
        |> Array.maxBy (fun (a, b) -> a * b)

    let mutable num =
        if minimum = 0UL then 1UL else (minimum / fst prods / snd prods)

    while (attempt num prods goals) |> Option.isNone do
        num <- num + 1UL

    attempt num prods goals

let sample = "939
7,13,x,x,59,x,31,19"

part2 0UL sample
