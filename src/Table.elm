module Table (table, matrix) where

{-|

This module provides functions to render tabular views of data.

@docs table

-}

import Html exposing (Html)
import Html.Attributes as Html

cell : a -> (a -> String) -> Html
cell data fn =
    Html.td [] [ Html.text (fn data) ]

row : List (a -> String) -> a -> Html
row fns row =
    Html.tr [] (List.map (cell row) fns)

{-| Renders a tabular view of a list of data, where each row represents a single item in the list.

You must provide column definitions which specify the header text and a function
to convert items into displayed text.

```
import Table

data =
    [ [ 1, 2, 3, 4 ]
    , [ 10, 5, 24, 3, 7]
    , [ 2, 7, 23903, 4039 , 1024, 2 ]
    ]

main =
    data
    |> Table.table
        [ ("sum", List.sum >> toString)
        , ("min", List.foldr min 9999 >> toString)
        , ("max", List.foldr max -9999 >> toString)
        ]
```

-}
table : List (String, (a -> String)) -> List a -> Html
table columnDefinitions data =
    let
        headers = (List.map fst columnDefinitions)
        cellFns = (List.map snd columnDefinitions)
        singleList a = [a]
        headerRow =
            headers
            |> List.map (Html.text >> singleList >> Html.th [])
            |> Html.tr []
    in
        data
        |> List.map (row cellFns)
        |> (::) headerRow
        |> Html.table []


matrix : (a -> Html) -> List (List a) -> Html
matrix cellDefinition data =
    let
        singleList a = [a]
        attrs = [ Html.style
          [ ("padding", "0")
          , ("margin", "0")
          ] ]
        row data =
            data
            |> List.map (cellDefinition >> singleList >> Html.td attrs)
            |> Html.tr []
    in
        data
        |> List.map row
        |> Html.table [ Html.style [("border-collapse", "collapse")]]
