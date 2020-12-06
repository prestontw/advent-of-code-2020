#load "Common.fsx"
#load "Inputs.fsx"
open Common

let parse input = input |> lines

let part1 input = input |> parse

let input = Inputs.day7

part1 input
