#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input =
    let mutable ret = List.empty
    let mutable current = Map.empty
    for line in (input |> lines) do
        if line.Length = 0 then
            ret <- current :: ret
            current <- Map.empty
        else
            // for each item in the line, add that pair to current
            let items = line |> spaces
            for item in items do
                let [| tag; info |] = item.Split ':'
                current <- current.Add(tag, info)
    ret <- current :: ret
    ret

let valid (m: Map<_, _>) =
    m.Count = 8
    || (m.Count = 7 && (Option.isNone (m.TryFind "cid")))

let part1 input =
    input |> parse |> List.filter valid |> List.length

let byr (input: string) =
    let i = int input
    input.Length = 4 && i >= 1920 && i <= 2002

let iyr (input: string) =
    let i = int input
    input.Length = 4 && i >= 2010 && i <= 2020

let eyr (input: string) =
    let i = int input
    input.Length = 4 && i >= 2020 && i <= 2030

let numbers = [ '0' .. '9' ]
let alpha = [ 'a' .. 'f' ]

let hcl (input: string) =
    input.[0] = '#'
    && input.[1..]
       |> String.forall (fun c -> List.contains c numbers || List.contains c alpha)

let hgt (input: string) =
    let cm = input.Split "cm"
    if cm.Length = 2 then
        let nums = cm.[0] |> int
        cm.[1].Length = 0 && nums >= 150 && nums <= 193
    else
        let possible = input.Split "in"
        possible.Length = 2
        && possible.[1].Length = 0
        && int possible.[0] >= 59
        && int possible.[0] <= 76

let eyecolors =
    [ "amb"
      "blu"
      "brn"
      "gry"
      "grn"
      "hzl"
      "oth" ]

let ecl (input: string) = List.contains input eyecolors

let pid (input: string) =
    input.Length = 9
    && String.forall (fun c -> List.contains c numbers) input
// let part2 input = input |> parse

let valid2 (m: Map<_, _>) =
    let possible =
        m.Count = 8
        || (m.Count = 7 && (Option.isNone (m.TryFind "cid")))

    possible
    && m.["byr"]
    |> byr
    && m.["iyr"] |> iyr
    && m.["eyr"] |> eyr
    && m.["hgt"] |> hgt
    && m.["hcl"] |> hcl
    && m.["ecl"] |> ecl
    && m.["pid"] |> pid

let part2 input =
    input
    |> parse
    |> List.filter valid2
    |> List.length

part2 Inputs.day4
