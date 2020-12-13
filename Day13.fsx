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

let inverse (remainder, b) =
    if remainder < bigint.Zero then b + remainder else remainder

let goals ids =
    ids
    |> Seq.indexed
    |> Seq.choose (fun (idx, i) ->
        match i with
        | X -> None
        | Minutes a ->
            let _base = a |> bigint

            Some((-idx |> bigint) % (_base), _base))
    |> Array.ofSeq

// val gs : (int64 * int64) [] =
//   [|(0L, 19L); (-9L, 41L); (-13L, 37L); (-19L, 821L); (-32L, 13L); (-36L, 17L);
//     (-48L, 29L); (-50L, 463L); (-73L, 23L)|]

let bezoutCoefficients a b =
    let oldR, r = a, b
    let oldS, s = bigint.One, bigint.Zero
    let oldT, t = bigint.Zero, bigint.One

    let rec inner oldR r oldS s oldT t =
        if r = bigint.Zero then
            oldS, oldT
        else
            let quotient = oldR / r
            let oldR, r = (r, oldR - quotient * r)
            let oldS, s = (s, oldS - quotient * s)
            let oldT, t = (t, oldT - quotient * t)
            inner oldR r oldS s oldT t

    inner oldR r oldS s oldT t

let solvePair (remainder1, base1) (remainder2, base2) =
    let bezouts = bezoutCoefficients base1 base2

    let total =
        base1
        * (fst bezouts)
        * remainder2
        + base2 * (snd bezouts) * remainder1

    let newBase = base1 * base2
    total % newBase, newBase

let solveSystem (equations: (_ * _) []) =
    equations.[1..]
    |> Array.fold (solvePair) equations.[0]

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

let part2 input =
    let (_, ids) = input |> parse

    ids |> goals |> solveSystem


let sample = "939
7,13,x,x,59,x,31,19"

let t1 = "7
17,x,13,19"

let t2 = "7
67,7,59,61"

let t3 = "7
67,x,7,59,61"

let t4 = "7
67,7,x,59,61"

let t5 = "7
1789,37,47,1889"

part2 sample
