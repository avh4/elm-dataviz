module MatrixExample where

import Graph


f x y = (x+y) * (2*x - y)

data =
    List.map (\a -> List.map (\b -> f a b) [-20..20]) [-20..20]


main =
    data
    |> Graph.matrix
