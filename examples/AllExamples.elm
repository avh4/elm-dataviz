import Html exposing (Html)

import GraphExample
import TableExample


demos =
    [ ("Tables", TableExample.main)
    , ("Graphs", GraphExample.main)
    ]


renderDemo (title, view) =
    [ Html.h2 [] [ Html.text title]
    , view
    ]


main =
    demos
    |> List.concatMap renderDemo
    |> Html.div []
