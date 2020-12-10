#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input =
    input
    |> lines
    |> Array.map int
    |> Array.sort
    |> Array.append [| 0 |]

let differences (input: int []) =
    let mutable oneDiffs = 0
    let mutable threeDiffs = 0
    for i in 1 .. (input.Length - 1) do
        if input.[i] - input.[i - 1] = 1 then oneDiffs <- oneDiffs + 1 else threeDiffs <- threeDiffs + 1
    (oneDiffs, (threeDiffs + 1))

let part1 input =
    input
    |> parse
    |> differences
    |> fun (f, s) -> f * s

let input = Inputs.day10

part1 input

let numberConfigurations (input: int []): uint64 =
    let m = Array.max input

    let rec inner n =
        if n = m + 3 then
            1UL
        else if not (Array.contains n input) then
            0UL
        else
            fastInner (n + 1)
            + fastInner (n + 2)
            + fastInner (n + 3)

    and fastInner = memoize inner
    inner 0

input |> parse |> numberConfigurations
