module MatrixTable where

import Effects
import Html exposing (Html)
import Html.Attributes as Html
import Html.Events as Html


type alias Model a =
    List (List a)


type Action a
    = Hover a


matrix : (a -> Html) -> Signal.Address (Action a) -> Model a -> Html
matrix cellDefinition address data =
    let
        singleList a = [a]
        attrs a =
            [ Html.style
                [ ("padding", "0")
                , ("margin", "0")
                ]
            , Html.onMouseOver address (Hover a)
            ]
        cell a =
            Html.td (attrs a) [cellDefinition a]
        row data =
            data
            |> List.map cell
            |> Html.tr []
    in
        data
        |> List.map row
        |> Html.table [ Html.style [("border-collapse", "collapse")]]
