#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse = reversedBlankLines

let groupAnswers (input: string list) =
    let mutable set = Set.empty

    for line in input do
        for c in line do
            set <- Set.add c set

    set.Count

let input = Inputs.day6

let part1 input =
    input |> parse |> List.sumBy groupAnswers

let allAnswered (input: string list) =
    let set = input |> List.head |> Set.ofSeq

    Set.intersectMany (input |> List.map Set.ofSeq |> Seq.ofList)
    |> Set.count

let part2 input =
    input |> parse |> List.sumBy allAnswered

part2 input
