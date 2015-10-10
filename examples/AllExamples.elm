import Html exposing (Html)
import Html.Attributes as Html

import StartApp
import Effects
import GraphExample
import TableExample
import MatrixExample
import UnivariateDistributionExample


demos =
    [ ("Tables", \_ _ -> TableExample.main)
    , ("Graphs", \_ _ -> GraphExample.main)
    , ("Matrix", MatrixExample.view)
    , ("Univariate Distribution", \_ _ -> UnivariateDistributionExample.main)
    ]


renderDemo address model (title, view) =
    [ Html.h2 [] [ Html.text title]
    , view address model
    ]


view address model =
    demos
    |> List.concatMap (renderDemo address model)
    |> Html.div
        [ Html.style [("padding", "48px")] ]


app =
    StartApp.start
        { init = MatrixExample.init
        , update = MatrixExample.update
        , view = view
        , inputs = []
        }


main =
    app.html
