#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = input |> lines |> Array.map spaces

type ExitStatus =
    | RanToCompletion of int
    | DuplicateInstruction of int

let execute (input: string [] []) =
    let rec operate acc programCounter seen changed =
        if Set.contains programCounter seen then
            DuplicateInstruction acc
        else if programCounter = input.Length then
            RanToCompletion acc
        else

            let [| command; details |] = input.[programCounter]
            let sign = details.[0]

            let number =
                (details.[1..] |> int)
                * if sign = '+' then 1 else -1

            match command with
            | "nop" ->
                let result =
                    operate acc (programCounter + 1) (Set.add programCounter seen) changed

                match result with
                | DuplicateInstruction _ when not changed ->
                    operate acc (programCounter + number) (Set.add programCounter seen) true
                | _ -> result
            | "acc" -> operate (acc + number) (programCounter + 1) (Set.add programCounter seen) changed
            | "jmp" ->

                let result =
                    operate acc (programCounter + number) (Set.add programCounter seen) changed

                match result with
                | DuplicateInstruction _ when not changed ->
                    operate acc (programCounter + 1) (Set.add programCounter seen) true
                | _ -> result
            | _ ->
                printf "error!\n"
                DuplicateInstruction 0

    operate 0 0 Set.empty false


// this includes part2 too... easier to edit execute instead of making a second one
let part1 (input) = input |> parse |> execute

let input = Inputs.day8

part1 input
