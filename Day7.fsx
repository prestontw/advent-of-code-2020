#load "Common.fsx"
#load "Inputs.fsx"
open Common
open System.Text.RegularExpressions

let lineRegex = "(\w*) (\w*) bags contain (.*)\."

let parse input =
    input
    |> lines
    |> Array.choose (extractValues lineRegex)
    |> Array.map Seq.toArray

let addDeps (lines: string [] []) =
    let mutable ret: Map<string * string, Set<string * string>> = Map.empty
    for line in lines do
        let containing = line.[0], line.[1]
        let insides = line.[2].Split ", "
        for key in insides do
            let components = key |> spaces
            let key = components.[1], components.[2]
            match Map.tryFind key ret with
            | Some deps -> ret <- Map.add key (Set.add containing deps) ret
            | None -> ret <- Map.add key ([ containing ] |> Set.ofSeq) ret

    ret

let bagify insides =
    if insides = "no other bags" then
        [| 0, ("", "") |]
    else
        insides.Split ", "
        |> Array.map (spaces >> (fun s -> s.[0] |> int, (s.[1], s.[2])))

let addRequirements (lines: string [] []) =
    let mutable ret = Map.empty
    for line in lines do
        let containing = line.[0], line.[1]
        ret <- Map.add containing (bagify line.[2]) ret

    ret

let rec computeBags start (bags: Map<_, _>) =
    bags.[start]
    |> Array.sumBy (fun (count, bag) -> if count = 0 then 0 else count + count * computeBags bag bags)


let followLinks start (links: Map<_, _>) =
    let mutable visited = [ start ] |> Set.ofList
    let mutable stack = [ start ]
    while not stack.IsEmpty do
        let current = stack.Head
        visited <- Set.add current visited
        stack <- List.tail stack
        if links.ContainsKey current then
            for next in links.[current] do
                if not (visited.Contains next) then stack <- next :: stack

    visited

let part1 input =
    input
    |> parse
    |> addDeps
    |> followLinks ("shiny", "gold")
    |> Set.count
    |> fun x -> x - 1

let input = Inputs.day7

part1 input

input
|> parse
|> addRequirements
|> computeBags ("shiny", "gold")
