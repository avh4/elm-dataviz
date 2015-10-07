module MatrixExample where

import Graph


f x y = (x+y) * (2*x - y)

data =
    List.concatMap (\a -> List.map (\b -> ((a,b),f a b)) [-20..20]) [-20..20]


main =
    data
    |> Graph.matrixDataset "Example"
        ("A", fst >> fst)
        ("B", fst >> snd)
        ("Value", snd)
        0
    |> Graph.matrix
