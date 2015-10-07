This package provides a simple way to render tabular views of data.

```elm
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

## Running the examples from source

```bash
(cd examples; elm-package install; elm-reactor)
open http://localhost:8080/Examples.elm
```
