module UnivariateDistributionExample exposing (..)

import Graph


data =
    [ ( 40, 1 )
    , ( 50, 2 )
    , ( 60, 5 )
    , ( 70, 12 )
    , ( 80, 10 )
    , ( 90, 19 )
    , ( 100, 27 )
    , ( 110, 13 )
    , ( 120, 7 )
    , ( 130, 4 )
    ]


view =
    data
        |> Graph.distplot ( 300, 200 )


main =
    view
