#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = input |> lines |> Array.map spaces

let valid (line: string array) =
    let [| minimum; maximum |] = line.[0].Split('-') |> Array.map int
    let celebrity = line.[1].[0]

    let numTimes =
        line.[2]
        |> Seq.filter (fun c -> c = celebrity)
        |> Seq.length

    numTimes >= minimum && numTimes <= maximum

let exactlyOnce (line: string array) =
    let [| firstLoc; otherLoc |] = line.[0].Split('-') |> Array.map int
    let celebrity = line.[1].[0]

    let firstCandidate = line.[2].[firstLoc - 1]
    let secondCandidate = line.[2].[otherLoc - 1]

    xor (celebrity = firstCandidate) (celebrity = secondCandidate)

let part1 input =
    input
    |> parse
    |> Array.filter valid
    |> Array.length

let part2 input =
    input
    |> parse
    |> Array.filter exactlyOnce
    |> Array.length
