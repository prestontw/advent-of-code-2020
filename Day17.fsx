#load "Common.fsx"
#load "Inputs.fsx"
open Common

type State =
    | Active
    | Inactive

let next state count =
    match state, count with
    | Active, 2
    | Active, 3 -> Active
    | Active, _ -> Inactive
    | Inactive, 3 -> Active
    | Inactive, _ -> Inactive


let parse input =
    input
    |> lines
    |> Array.indexed
    |> Array.collect (fun (row, line) ->
        line
        |> Seq.indexed
        |> Seq.choose (fun (col, charr) -> if charr = '#' then Some(row, col, 0, 0) else None)
        |> Seq.toArray)
    |> Set.ofArray

let adjacenciesAux (row, col, zed, w) =
    let offsets = [ -1; 0; 1 ]
    let mutable ret = []
    for rowDelt in offsets do
        for colDelt in offsets do
            for zedDelt in offsets do
                for wDelt in offsets do
                    if rowDelt = 0
                       && colDelt = 0
                       && zedDelt = 0
                       && wDelt = 0 then
                        ()
                    else
                        ret <-
                            (row + rowDelt, col + colDelt, zed + zedDelt, w + wDelt)
                            :: ret
    ret

let adjacancies = memoize adjacenciesAux

let possibleNeighbors actives =
    actives
    |> Seq.fold (fun neighbors active ->
        adjacancies active
        |> Seq.fold (fun neighbors coordinate ->
            Map.add
                coordinate
                ((Map.tryFind coordinate neighbors
                  |> Option.defaultValue 0)
                 + 1)
                neighbors) neighbors) Map.empty

let nextGen actives =
    let neighbors = actives |> possibleNeighbors

    neighbors
    |> Map.toSeq
    |> Seq.choose (fun (coordinates, count) ->
        let state =
            if Set.contains coordinates actives then Active else Inactive

        let nextState = next state count
        if nextState = Active then Some coordinates else None)
    |> Set.ofSeq

let part1 numCycles input =
    let parsed = input |> parse
    [ 1 .. numCycles ]
    |> Seq.fold (fun acc _cycle -> nextGen acc) parsed

let input = Inputs.day17

let sample = ".#.
..#
###"

part1 6 input |> Set.count
