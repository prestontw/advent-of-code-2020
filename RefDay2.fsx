let input =
    "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,5,23,2,23,9,27,1,5,27,31,1,9,31,35,1,35,10,39,2,13,39,43,1,43,9,47,1,47,9,51,1,6,51,55,1,13,55,59,1,59,13,63,1,13,63,67,1,6,67,71,1,71,13,75,2,10,75,79,1,13,79,83,1,83,10,87,2,9,87,91,1,6,91,95,1,9,95,99,2,99,10,103,1,103,5,107,2,6,107,111,1,111,6,115,1,9,115,119,1,9,119,123,2,10,123,127,1,127,5,131,2,6,131,135,1,135,5,139,1,9,139,143,2,143,13,147,1,9,147,151,1,151,2,155,1,9,155,0,99,2,0,14,0"

type Opcode =
    | Halt
    | Add
    | Multiply

type Opcode with
    member _.Num = 3

let intToOpcode =
    function
    | 99 -> Some Halt
    | 1 -> Some Add
    | 2 -> Some Multiply
    | _ -> None

let interp (ops: int array) =
    let mutable index = 0

    while ops.[index] <> 99 do
        let op =
            match ops.[index] with
            | 1 -> (+)
            | 2 -> (*)
            | _ -> fun _ _ -> 0

        ops.[ops.[index + 3]] <- op ops.[ops.[index + 1]] ops.[ops.[index + 2]]
        index <- index + 4

    ops

let sample = "1,9,10,3,2,3,11,0,99,30,40,50"

let parse (input: string) = input.Split(',') |> Seq.map int

let value (arr: int array) = arr.[0]

let replace noun verb (arr: int array) =
    arr.[1] <- noun
    arr.[2] <- verb
    arr

let part1 lines =
    lines
    |> parse
    |> Seq.toArray
    |> replace 12 2
    |> interp
    |> value


let finalAnswer noun verb = 100 * noun + verb

let part2 lines =
    let mutable ret = None
    let initial = lines |> parse |> Seq.toArray

    for i in 0..99 do
        for j in 0..99 do
            let current = Array.copy initial
            let result = replace i j current |> interp |> value

            if result = 196_907_20 then
                ret <- Some(i, j)

    ret
