let manhattanDistance (x, y) = abs x + abs y

let keysToSet m =
    m |> Map.toSeq |> Seq.map fst |> Set.ofSeq

let digits (i: int) =
    i |> string |> Seq.map (fun i -> int i - int '0')

let lines (i: string) = i.Split '\n'

let commas (i: string) = i.Split ','
