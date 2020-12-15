#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = input |> commas |> Array.map int

let part1 until input =
    let parsed = input |> parse

    let inner memory last turnCount =
        // printfn "last: %d for turn %d" last turnCount
        if not (Map.containsKey last memory) then
            let newMap = Map.add last (turnCount) memory
            0, newMap
        else
            // find the difference between last number and this one
            let previousTime = memory.[last]
            let newMap = Map.add last (turnCount) memory
            (turnCount - previousTime), newMap

    let initial =
        parsed.[..parsed.Length - 2]
        |> Array.indexed
        |> Array.map (fun (f, s) -> s, f + 1)
        |> Map.ofArray

    [ (parsed.Length) .. (until - 1) ]
    |> List.fold (fun (lastSaid, memory) turn -> inner memory lastSaid turn) (parsed |> Array.last, initial)

let input = Inputs.day15

let sample = "0,3,6"
part1 30000000 input
