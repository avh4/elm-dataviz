module MatrixExample where

import Graph
import StartApp
import Effects
import MatrixTable
import Html


type alias Model =
    { hover : Maybe Float
    , data : Graph.DenseDataset
    }


f : Float -> Float -> Float
f x y = (x+y) * (2*x - y)

data : Graph.DenseDataset
data =
    List.concatMap (\a -> List.map (\b -> ((a,b),f a b)) [-20..20]) [-20..20]
    |> Graph.matrixDataset "Example"
        ("A", fst >> fst)
        ("B", fst >> snd)
        ("Value", snd)
        0


init = (Model Nothing data, Effects.none)


view address model =
    let
        graph =
            model.data
            |> Graph.matrix address
    in
        case model.hover of
            Nothing -> graph
            Just v ->
                Html.div []
                    [ graph
                    , Html.text <| toString v
                    ]


update action model =
    case action of
        MatrixTable.Hover v ->
            { model | hover <- Just v }
            |> (flip (,)) Effects.none

app =
    StartApp.start
        { init = init
        , update = update
        , view = view
        , inputs = []
        }


main = app.html
