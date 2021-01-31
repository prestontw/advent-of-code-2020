#load "Common.fsx"
#load "Inputs.fsx"
open Common


type Rule =
    { Name: string
      FirstClause: int * int
      SecondClause: int * int }

type Input =
    { Rules: Rule list
      YourTicket: int list
      NearbyTickets: int list list }

let ruleRegex = "(.*): (\d*)-(\d*) or (\d*)-(\d*)"
let parseFields input = input |> commas |> Seq.map int

let parse input =
    let [ rules; your; nearby ] = input |> blankLines

    let rules =
        rules
        |> List.choose
            (fun line ->
                extractValues ruleRegex line
                |> Option.map Seq.toList)

        |> List.map
            (fun [ name; firstLow; firstHigh; secondLow; secondHigh ] ->
                { Name = name
                  FirstClause = (firstLow |> int, firstHigh |> int)
                  SecondClause = (secondLow |> int, secondHigh |> int) })

    let your = your.[1] |> parseFields |> Seq.toList

    let nearby =
        nearby.[1..]
        |> List.map (parseFields >> Seq.toList)

    { Rules = rules
      YourTicket = your
      NearbyTickets = nearby }

let checkOutBounds rule value =
    (value < fst rule.FirstClause
     || value > snd rule.FirstClause)
    && (value < fst rule.SecondClause
        || value > snd rule.SecondClause)

let checkInBounds rule value =
    (value >= fst rule.FirstClause
     && value <= snd rule.FirstClause)
    || (value >= fst rule.SecondClause
        && value <= snd rule.SecondClause)

/// Return whether any ticket's value is not in the rule ranges
let invalidTicket rules ticket =
    let outsideAll value =
        let invalid =
            rules
            |> List.forall (fun rule -> checkOutBounds rule value)

        if invalid then Some value else None

    ticket |> List.choose outsideAll

let part1 input =
    let { Rules = rules
          YourTicket = yourticket
          NearbyTickets = nearby } =
        input |> parse

    let invalid = invalidTicket rules
    nearby |> Seq.sumBy (invalid >> List.sum)

let ruleIndex rule tickets alreadyUsed =
    let possibilities =
        [ 0 .. ((List.head tickets) |> fun l -> List.length l - 1) ]
        |> Set.ofList
        |> fun s -> Set.difference s alreadyUsed

    let validForAll index =
        tickets
        |> List.map (List.item index)
        |> List.forall (fun value -> checkInBounds rule value)

    possibilities |> Seq.filter validForAll

/// Given possibilities, use process of elimination to arrive at assignments
let solve constraints =
    let mutable assignments = Map.empty

    let pending =
        constraints
        |> List.map (fun (name, possibilities) -> name, possibilities |> Set.ofList)

    let mutable assigned = Set.empty

    while pending |> List.length <> (assigned |> Set.count) do
        for (name, possibilities) in pending do
            let possibilities = Set.difference possibilities assigned

            if possibilities |> Set.count = 1 then
                let element = possibilities |> Set.minElement
                assigned <- Set.add element assigned
                assignments <- Map.add name element assignments


    assignments

let part2 input =
    let { Rules = rules
          YourTicket = yourticket
          NearbyTickets = nearby } =
        input |> parse

    let invalid = invalidTicket rules

    let valids =
        nearby |> List.filter (invalid >> List.isEmpty)

    rules
    |> List.map (fun rule -> rule.Name, ruleIndex rule valids Set.empty |> Seq.toList)
    |> solve,
    yourticket


let calculateSixValues mappings (yourticket: int list) =
    mappings
    |> Map.toList
    |> List.filter (fun (name: string, _indices) -> name.StartsWith "departure")
    |> List.map snd
    |> List.fold (fun acc index -> acc * yourticket.[index]) 1

let input = Inputs.day16

let sample = "class: 1-3 or 5-7
row: 6-11 or 33-44
seat: 13-40 or 45-50

your ticket:
7,1,14

nearby tickets:
7,3,47
40,4,50
55,2,20
38,6,12"

let sample2 = "class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9"

input |> part2
// |> (fun (mappings, yt) -> calculateSixValues mappings yt)
