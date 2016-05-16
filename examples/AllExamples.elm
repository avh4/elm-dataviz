module Main exposing (..)

import Html exposing (Html)
import Html.Attributes as Html
import Html.App
import GraphExample
import TableExample
import MatrixExample
import UnivariateDistributionExample


demos =
    [ ( "Tables", \_ -> TableExample.view )
    , ( "Graphs", \_ -> GraphExample.view )
    , ( "Matrix", MatrixExample.view )
    , ( "Univariate Distribution", \_ -> UnivariateDistributionExample.view )
    ]


renderDemo model ( title, view ) =
    [ Html.h2 [] [ Html.text title ]
    , view model
    ]


view model =
    demos
        |> List.concatMap (renderDemo model)
        |> Html.div [ Html.style [ ( "padding", "48px" ) ] ]


main =
    Html.App.program
        { init = MatrixExample.init
        , update = MatrixExample.update
        , view = view
        , subscriptions = \_ -> Sub.none
        }
