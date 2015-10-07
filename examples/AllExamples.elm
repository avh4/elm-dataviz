import Html exposing (Html)

import GraphExample
import TableExample


main =
    Html.div []
        [ Html.h2 [] [ Html.text "Tables"]
        , TableExample.main
        , Html.h2 [] [ Html.text "Graphs"]
        , GraphExample.main
        ]
