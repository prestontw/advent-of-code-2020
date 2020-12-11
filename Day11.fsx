#load "Common.fsx"
#load "Inputs.fsx"
open Common

type Space =
    | Empty
    | Floor
    | Occupied

let charToSpace c =
    match c with
    | '.' -> Floor
    | 'L' -> Empty

let parse input =
    input
    |> lines
    |> Array.map (fun l -> l |> Seq.map charToSpace |> Seq.toArray)

let offsets =
    [ (-1, -1)
      (-1, 0)
      (-1, 1)
      (0, -1)
      (0, 1)
      (1, -1)
      (1, 0)
      (1, 1) ]

let nextGen row column (a: Space [] []) =
    let neighbor rowDiff colDiff =
        a
        |> Array.tryItem (row + rowDiff)
        |> Option.bind (Array.tryItem (column + colDiff))

    let neighbors =
        offsets |> Seq.choose (fun (r, c) -> neighbor r c)

    let numOccupied =
        neighbors
        |> Seq.filter (fun s -> s = Occupied)
        |> Seq.length

    match a.[row].[column] with
    | Empty when numOccupied = 0 -> Occupied
    | Occupied when numOccupied >= 4 -> Empty
    | current -> current

let next (a: Space [] []) =
    a
    |> Array.indexed
    |> Array.map (fun (i, row) ->
        row
        |> Array.indexed
        |> Array.map (fun (j, _) -> nextGen i j a))

let part1 input =
    let rec inner current =
        let temp = current |> next
        if temp = current then current else inner temp

    let final = input |> parse |> inner
    final
    |> Array.sumBy (fun row ->
        row
        |> Array.sumBy (fun space -> if space = Occupied then 1 else 0))

let input = Inputs.day11

let sample = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL"

part1 input
