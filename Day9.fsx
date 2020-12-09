#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = input |> lines |> Array.map uint64

let firstInvalid preambleLength (input: uint64 []) =
    let rec ifValid currentIndex =
        let candidate = input.[currentIndex]

        let potentials =
            input.[currentIndex - preambleLength - 1..currentIndex - 1]

        let mutable pair = None
        for i in potentials do
            for j in potentials do
                if i + j = candidate && i <> j then pair <- Some(i, j)
        match pair with
        | Some _ -> ifValid (currentIndex + 1)
        | None -> candidate

    ifValid preambleLength

let part1 input = input |> parse |> firstInvalid 25

let input = Inputs.day9

let contiguousSet (input: uint64 []) (broken: uint64) =
    let rec intern start end_ =
        let candidate = input.[start..end_]
        let sum = Array.sum candidate
        if sum = broken then candidate |> Set.ofArray
        else if sum > broken then intern (start + 1) (start + 2)
        else intern start (end_ + 1)

    intern 0 1


let part2 input =
    let parsed = input |> parse
    let broken = part1 input
    let s = contiguousSet parsed broken
    (Set.minElement s) + (Set.maxElement s)

part2 input
