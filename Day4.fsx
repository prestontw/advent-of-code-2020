#load "Common.fsx"
#load "Inputs.fsx"
open Common
open System.Text.RegularExpressions

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

let hclReg = Regex("#(?:[a-f]|[0-9]){6}")

let hcl (input: string) = hclReg.IsMatch input

let hgtReg = "(\d*)(in|cm)"

let hgt (input: string) =
    let res = extractValues hgtReg input
    match res with
    | Some s ->
        let [ height; metric ] = s |> Seq.toList
        let nums = height |> int
        if metric = "cm" then nums >= 150 && nums <= 193 else nums >= 59 && nums <= 76
    | None -> false

let eyecolors = Regex("(?:amb|blu|brn|gry|grn|hzl|oth)")

let ecl (input: string) = eyecolors.IsMatch input

let pidReg = Regex("^\d{9}$")
let pid (input: string) = pidReg.IsMatch input

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
