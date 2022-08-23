#load "Inputs.fsx"
#r "nuget: Expecto"

open Expecto

let parse (input: string) = input.Split '\n' |> Seq.map int

let part1 input =
    let list = parse input

    for i in list do
        for j in list do
            if i + j = 2020 then printf "%d" (i * j)

let part2 input =
    let list = parse input

    for i in list do
        for j in list do
            for k in list do
                if i + j + k = 2020 then
                    printf "%d\n" (i * j * k)

let tests =
    test "A simple test" {
        let subject = "Hello Wrorld"
        Expect.equal subject "Hello World" "The strings should equal"
    }

let main = runTestsWithCLIArgs [] [||] tests
