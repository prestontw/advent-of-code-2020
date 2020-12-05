#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse = lines

let trees right (input: string array): uint64 =
    input
    |> Array.indexed
    |> Array.map (fun (i, line) -> line.[right * i % line.Length])
    |> Array.skip 1
    |> Array.filter (fun c -> c = '#')
    |> Array.length
    |> uint64

let downTrees (input: string array): uint64 =
    input
    |> Array.indexed
    |> Array.filter (fun (i, _) -> i % 2 = 0)
    |> Array.map (fun (i, line) -> line.[i / 2 % line.Length])
    |> Array.skip 1
    |> Array.filter (fun c -> c = '#')
    |> Array.length
    |> uint64

let part1 input = input |> parse |> trees 3

let part2 input =
    let parsed = input |> parse
    let one = trees 1 parsed
    let three = trees 3 parsed
    let five = trees 5 parsed
    let seven = trees 7 parsed
    let two = downTrees parsed

    // one, three, five, seven, two
    one * three * five * seven * two
