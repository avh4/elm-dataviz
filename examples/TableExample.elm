module TableExample where

import Table

data =
    [ [ 1, 2, 3, 4 ]
    , [ 10, 5, 24, 3, 7]
    , []
    , [ 2, 7, 23903, 4039 , 1024, 2 ]
    ]


avg : List number -> Maybe Float
avg items =
    case items of
        [] ->
            Nothing
        _ ->
            Just <|
                (toFloat <| List.sum items) / (toFloat <| List.length items)


maybeString : Maybe a -> String
maybeString =
    Maybe.map toString >> Maybe.withDefault "--"


main =
    data
    |> Table.table
        [ ("sum", List.sum >> toString)
        , ("min", List.minimum >> maybeString)
        , ("average", avg >> maybeString)
        , ("max", List.maximum >> maybeString)
        ]
