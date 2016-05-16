module Graph
    exposing
        ( DenseDataset
        , xyDataset
        , matrixDataset
        , graph
        , distplot
        , matrix
        )

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Svg.Attributes as Svg
import Html exposing (Html)
import Html.Attributes as Html
import Numeral
import Array exposing (Array)
import Set exposing (Set)
import Dict exposing (Dict)
import MatrixTable


type alias Point =
    ( Float, Float )


type alias Normalize a =
    a -> a


type alias Range =
    ( Float, Float )


type alias Dataset =
    { name : String
    , values : List Point
    , x : String
    , y : String
    }


xyDataset : String -> ( String, a -> Float ) -> ( String, a -> Float ) -> List a -> Dataset
xyDataset name ( xl, xf ) ( yl, yf ) values =
    { name = name
    , values = List.map2 (,) (List.map xf values) (List.map yf values)
    , x = xl
    , y = yl
    }


type alias DenseDataset =
    { name : String
    , values : Array (Array Float)
    , x : String
    , y : String
    , v : String
    }


matrixDataset :
    String
    -> ( String, a -> Float )
    -> ( String, a -> Float )
    -> ( String, a -> Float )
    -> Float
    -> List a
    -> DenseDataset
matrixDataset name ( xl, xf ) ( yl, yf ) ( vl, vf ) default values =
    let
        step a ( xs, ys, vs ) =
            ( Set.insert (xf a) xs
            , Set.insert (yf a) ys
            , Dict.insert ( xf a, yf a ) (vf a) vs
            )

        ( xs, ys, vs ) =
            List.foldl step ( Set.empty, Set.empty, Dict.empty ) values
    in
        { name = name
        , values =
            Array.map (\x -> Array.map (\y -> Dict.get ( x, y ) vs |> Maybe.withDefault default) (Array.fromList <| Set.toList ys)) (Array.fromList <| Set.toList xs)
        , x = xl
        , y = yl
        , v = vl
        }


scatterPlot : Normalize Point -> String -> Dataset -> Svg msg
scatterPlot normalize color data =
    data.values
        |> List.map normalize
        |> List.map (\( x, y ) -> Svg.circle [ Svg.cx <| toString x, Svg.cy <| toString y, r "2", fill color ] [])
        |> Svg.g []



{- --         <text className="label-backing" x={p.x} y={p.y} dy="0.3em">{d.name}</text>
   --<text x={p.x} y={p.y} dy="0.3em">{d.name}</text>
-}


barPlot : Normalize Point -> String -> List ( Float, Float ) -> Svg msg
barPlot normalize color data =
    let
        w =
            1

        bar ( x, y ) =
            let
                ( x1, y1 ) =
                    normalize ( x - w / 2, y )

                ( x2, y2 ) =
                    normalize ( x + w / 2, 0 )
            in
                Svg.rect
                    [ Svg.x <| toString x1
                    , Svg.y <| toString y1
                    , Svg.width <| toString (x2 - x1)
                    , Svg.height <| toString (y2 - y1)
                    , Svg.fill color
                    ]
                    []
    in
        data
            |> List.map bar
            |> Svg.g []


niceNum : Float -> Bool -> Float
niceNum range round =
    let
        exponent =
            logBase 10 range |> floor |> toFloat

        fraction =
            range / (10 ^ exponent)
    in
        (*) (10.0 ^ exponent)
            <| case round of
                True ->
                    if fraction < 1.5 then
                        1.0
                    else if fraction < 3.0 then
                        2.0
                    else if fraction < 7.0 then
                        2.0
                    else
                        10.0

                False ->
                    if fraction <= 1 then
                        1.0
                    else if fraction <= 2 then
                        2.0
                    else if fraction <= 5 then
                        5.0
                    else
                        10.0


ticks2 : Float -> Float -> Float -> Float -> List Float -> List Float
ticks2 min max spacing i acc =
    if i > max then
        acc
    else if i < min + spacing * 0.2 then
        ticks2 min max spacing (i + spacing) acc
    else if i > max - spacing * 0.2 then
        ticks2 min max spacing (i + spacing) acc
    else
        ticks2 min max spacing (i + spacing) (i :: acc)


ticks : Range -> List Float
ticks ( min, max ) =
    let
        maxTicks =
            10

        range =
            niceNum (max - min) False

        spacing =
            niceNum (range / (maxTicks - 1)) True

        niceMin =
            floor (min / spacing) |> toFloat |> (*) spacing

        niceMax =
            ceiling (max / spacing) |> toFloat |> (*) spacing
    in
        ticks2 niceMin niceMax spacing niceMin []


tickMarkX : ( Float, Float ) -> Normalize Point -> Float -> List (Svg msg)
tickMarkX ( w, h ) normalize t =
    let
        x' =
            normalize ( t, 0 ) |> fst |> toString
    in
        [ Svg.line [ y2 (toString -h), x1 x', x2 x', stroke "rgba(80,80,80,0.4)" ] []
        , Svg.text' [ x x', dy ".71em", y "10" ] [ Html.text <| toString t ]
        ]


tickMarkY : ( Float, Float ) -> Normalize Point -> Float -> List (Svg msg)
tickMarkY ( w, h ) normalize t =
    let
        y' =
            normalize ( 0, t ) |> snd |> toString
    in
        [ Svg.line [ x2 (toString w), y1 y', y2 y', stroke "rgba(80,80,80,0.4)" ] []
        , Svg.text' [ textAnchor "end", x "-10", dy ".32em", y y' ] [ Html.text <| Numeral.format "0,0" <| toFloat <| round t ]
        ]


yAxis : ( Float, Float ) -> String -> Range -> Normalize Point -> Svg msg
yAxis ( w, h ) label ( min, max ) normalize =
    ticks ( min, max )
        |> List.map (tickMarkY ( w, h ) normalize)
        |> List.concat
        |> List.append
            [ Svg.text' [ transform "rotate(-90)", y "-80", x (toString <| -h / 2) ] [ Html.text label ]
              --, Svg.text' [dy ".32em", y (toString h), x "-10"] [Html.text <| toString min]
              --, Svg.text' [x "-10", dy ".32em", y "0"] [Html.text <| toString max]
            , Svg.line [ y2 <| toString h, stroke "rgb(30,30,30)" ] []
            ]
        |> Svg.g []


xAxis : ( Float, Float ) -> String -> Range -> Normalize Point -> Svg msg
xAxis ( w, h ) label ( min, max ) normalize =
    ticks ( min, max )
        |> List.map (tickMarkX ( w, h ) normalize)
        |> List.concat
        |> List.append
            [ Svg.text' [ textAnchor "middle", y "50", x (toString <| w / 2) ] [ Html.text label ]
              --, Svg.text' [dy ".71em", y "20"] [Html.text <| toString min]
              --, Svg.text' [x (toString w), dy ".71em", y "20"] [Html.text <| toString max]
            , Svg.line [ x2 <| toString w, stroke "rgb(30,30,30)" ] []
            ]
        |> Svg.g [ transform <| "translate(0," ++ (toString h) ++ ")" ]


determineRange : List Float -> Range
determineRange data =
    let
        min =
            List.minimum data |> Maybe.withDefault 0

        max =
            List.maximum data |> Maybe.withDefault 100
    in
        if min /= max then
            ( min, max )
        else if min < 0 then
            ( min, 0 )
        else if max > 0 then
            ( 0, max )
        else
            ( 0, 1 )


type alias ScalingFn =
    Range -> Float -> Float -> Float


linearScale : ScalingFn
linearScale ( min, max ) scale v =
    let
        percent =
            (v - min) / (max - min)
    in
        percent * scale


logScale : ScalingFn
logScale ( min, max ) scale v =
    let
        lv =
            logBase 10 v

        r1 =
            logBase 10 max

        r0 =
            logBase 10 min

        percent =
            (lv - r0) / (r1 - r0)
    in
        percent * scale



-- Dark Tango colors following http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/


colors : List String
colors =
    [ "#204a87", "#ce5c00", "#4e9a06", "#a40000", "#2e3436", "#5c3566", "#8f5902", "#c4a000" ]


graph : ( Float, Float ) -> List Dataset -> Svg msg
graph ( w, h ) dataSets =
    let
        svgW =
            w + 110 + (145 - 90) |> toString

        svgH =
            h + 80 |> toString

        ( xmin, xmax ) =
            dataSets |> List.map .values |> List.concat |> List.map fst |> determineRange

        --(ymin, ymax) = (0.0, 50000000.0)
        ( ymin, ymax ) =
            dataSets |> List.map .values |> List.concat |> List.map snd |> determineRange

        normalize =
            (\( x, y ) -> ( linearScale ( xmin, xmax ) w x, linearScale ( ymax, ymin ) h y ))

        xl =
            dataSets |> List.head |> Maybe.map .x |> Maybe.withDefault ""

        yl =
            dataSets |> List.head |> Maybe.map .y |> Maybe.withDefault ""
    in
        Svg.svg [ width (svgW ++ "px"), height (svgH ++ "px"), viewBox ("-110 0 " ++ svgW ++ " " ++ svgH) ]
            [ Svg.g [ transform "translate(0,30)" ]
                [ xAxis ( w, h ) xl ( xmin, xmax ) normalize
                , yAxis ( w, h ) yl ( ymin, ymax ) normalize
                , dataSets
                    |> List.map2 (,) colors
                    |> List.map (uncurry <| scatterPlot normalize)
                    |> List.reverse
                    |> Svg.g []
                  --, legend data
                ]
            ]


distplot : ( Float, Float ) -> List ( Float, Float ) -> Svg msg
distplot ( w, h ) data =
    let
        svgW =
            w + 110 + (145 - 90) |> toString

        svgH =
            h + 80 |> toString

        ( xmin, xmax ) =
            data |> List.map fst |> determineRange

        --(ymin, ymax) = (0.0, 50000000.0)
        ( ymin, ymax ) =
            ( 0, data |> List.map snd |> determineRange |> snd )

        normalize =
            (\( x, y ) -> ( linearScale ( xmin, xmax ) w x, linearScale ( ymax, ymin ) h y ))

        xl =
            Nothing |> Maybe.withDefault ""

        yl =
            Nothing |> Maybe.withDefault ""
    in
        Svg.svg [ width (svgW ++ "px"), height (svgH ++ "px"), viewBox ("-110 0 " ++ svgW ++ " " ++ svgH) ]
            [ Svg.g [ transform "translate(0,30)" ]
                [ xAxis ( w, h ) xl ( xmin, xmax ) normalize
                , yAxis ( w, h ) yl ( ymin, ymax ) normalize
                , barPlot normalize "rgba(32,74,136,0.4)" data
                ]
            ]


compareGraph : ( Float, Float ) -> ( String, a -> Float ) -> ( String, a -> Float ) -> List (List a) -> Svg msg
compareGraph ( w, h ) xdef ydef data =
    data
        |> List.map (xyDataset "???" xdef ydef)
        |> graph ( w, h )


color' : Float -> Float -> Float -> Html msg
color' min max x =
    let
        p =
            (x - min) / (max - min)

        c =
            if p <= 1 / 6 then
                "#dae8f5"
            else if p <= 2 / 6 then
                "#bad6ea"
            else if p <= 3 / 6 then
                "#88bedc"
            else if p <= 4 / 6 then
                "#539dcc"
            else if p <= 5 / 6 then
                "#297ab9"
            else
                "#09559f"
    in
        Html.div [ Html.style [ ( "background", c ), ( "width", "5px" ), ( "height", "5px" ) ] ] []


matrix : DenseDataset -> Html (MatrixTable.Msg Float)
matrix model =
    let
        asList =
            model.values |> Array.map (Array.toList) |> Array.toList

        min =
            asList |> List.concat |> List.minimum |> Maybe.withDefault 0

        max =
            asList |> List.concat |> List.maximum |> Maybe.withDefault 1
    in
        MatrixTable.view (color' min max) asList
