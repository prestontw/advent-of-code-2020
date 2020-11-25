let keysToSet m =
    m |> Map.toSeq |> Seq.map fst |> Set.ofSeq
