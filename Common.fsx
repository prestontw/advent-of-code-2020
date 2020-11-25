let manhattanDistance (x, y) = abs x + abs y

let keysToSet m =
    m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
