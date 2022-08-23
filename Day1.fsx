#load "Inputs.fsx"
#r "nuget: Expecto"

open Expecto

let parse (input: string) = input.Split '\n' |> Seq.map int

let part1 input =
    let list = parse input
    let mutable ret = 0

    for i in list do
        for j in list do
            if i + j = 2020 then ret <- (i * j)

    ret

let part2 input =
    let list = parse input
    let mutable ret = 0

    for i in list do
        for j in list do
            for k in list do
                if i + j + k = 2020 then
                    ret <- (i * j * k)

    ret

let tests =
    testList
        "parts"
        [ test "part 1" {
              let subject = part1 Inputs.day1
              Expect.equal subject 793524 ""
          }

          test "part 2" {
              let subject = part2 Inputs.day1
              Expect.equal subject 61515678 ""
          } ]

let main = runTestsWithCLIArgs [] [||] tests
