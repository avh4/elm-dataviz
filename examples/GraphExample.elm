module GraphExample exposing (..)

import Graph


data =
    [ ( 0.0, 0.0 ), ( 1.0, 1.0 ), ( 2.0, 2.0 ) ]


view =
    data
        |> Graph.xyDataset "Example" ( "A", fst ) ( "B", snd )
        |> List.repeat 1
        |> Graph.graph ( 300, 200 )


main =
    view
