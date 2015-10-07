import Html exposing (Html)
import Html.Attributes as Html

import GraphExample
import TableExample
import MatrixExample


demos =
    [ ("Tables", TableExample.main)
    , ("Graphs", GraphExample.main)
    , ("Matrix", MatrixExample.main)
    ]


renderDemo (title, view) =
    [ Html.h2 [] [ Html.text title]
    , view
    ]


main =
    demos
    |> List.concatMap renderDemo
    |> Html.div
        [ Html.style [("padding", "48px")] ]
