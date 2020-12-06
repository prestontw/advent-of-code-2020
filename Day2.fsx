#load "Common.fsx"
#load "Inputs.fsx"
open Common

let line = "(\d*)-(\d*) (.): (\w*)"

let parse input =
    input
    |> lines
    |> Array.choose (extractValues line)

let valid (line: string seq) =
    let [| minimum; maximum; celebrity; word |] = line |> Seq.toArray
    let celebrity = celebrity.[0]
    let minimum = minimum |> int
    let maximum = maximum |> int

    let numTimes =
        word
        |> Seq.filter (fun c -> c = celebrity)
        |> Seq.length

    numTimes >= minimum && numTimes <= maximum

let exactlyOnce (line: string seq) =
    let [| firstLoc; otherLoc; celebrity; word |] = line |> Seq.toArray
    let celebrity = celebrity.[0]
    let firstLoc = firstLoc |> int
    let otherLoc = otherLoc |> int

    let firstCandidate = word.[firstLoc - 1]
    let secondCandidate = word.[otherLoc - 1]

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
