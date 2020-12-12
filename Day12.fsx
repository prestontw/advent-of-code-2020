#load "Common.fsx"
#load "Inputs.fsx"
open Common

type Direction =
    | N
    | S
    | E
    | W

let directionToUnit =
    function
    | N -> (0, 1)
    | S -> (0, -1)
    | E -> (1, 0)
    | W -> (-1, 0)

let charToDirection =
    function
    | 'N' -> N
    | 'E' -> E
    | 'S' -> S
    | 'W' -> W

let angleToUnit =
    function
    | 0 -> (1, 0)
    | 90 -> 0, 1
    | 180 -> -1, 0
    | 270 -> 0, -1
    | x ->
        printf "%d" x
        (0, 0)

let parse input =
    input
    |> lines
    |> Array.map (fun l -> (l.[0], l.[1..] |> int))

let move x y currenHeading instruction times =
    match instruction with
    | 'F' ->
        let (deltaX, deltaY) = currenHeading |> angleToUnit
        (x + (deltaX) * times, y + (deltaY) * times, currenHeading)
    | 'N'
    | 'S'
    | 'E'
    | 'W' ->
        let (deltaX, deltaY) =
            instruction |> charToDirection |> directionToUnit

        (x + deltaX * times, y + deltaY * times, currenHeading)
    | 'L' ->
        let newHeading = (currenHeading + times + 360) % 360
        x, y, newHeading
    | 'R' ->
        let newHeading = (currenHeading - times + 360) % 360
        x, y, newHeading

let part1 input =
    let parsed = input |> parse

    let (x, y, final) =
        Array.fold (fun (x, y, heading) (i, times) -> move x y heading i times) (0, 0, 0) parsed

    manhattanDistance (x, y)

let input = Inputs.day12

let sample = "F10
N3
F7
R90
F11"

sample |> part1
