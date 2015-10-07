module Graph where


import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Html
import Number.Format


type alias Point = (Float, Float)
type alias Normalize = (Point -> Point)
type alias Range = (Float, Float)

type alias Dataset =
    { name:String
    , values:List Point
    , x:String
    , y:String
    }

xyDataset : String -> (String, a->Float) -> (String, a->Float) -> List a -> Dataset
xyDataset name (xl,xf) (yl,yf) values =
    { name = name
    , values = List.map2 (,) (List.map xf values) (List.map yf values)
    , x = xl
    , y = yl
    }

scatterPlot : Normalize -> String -> Dataset -> Svg
scatterPlot normalize color data =
    data.values
    |> List.map normalize
    |> List.map (\(x,y) -> Svg.circle [ cx <| toString x, cy <| toString y, r "2", fill color] [])
    |> Svg.g []
{-    --         <text className="label-backing" x={p.x} y={p.y} dy="0.3em">{d.name}</text>
    --<text x={p.x} y={p.y} dy="0.3em">{d.name}</text>
-}


niceNum : Float -> Bool -> Float
niceNum range round =
    let
        exponent = logBase 10 range |> floor |> toFloat
        fraction = range / (10^exponent)
    in
        (*) (10.0^exponent) <| case round of
            True ->
                if | fraction < 1.5 -> 1.0
                   | fraction < 3.0 -> 2.0
                   | fraction < 7.0 -> 2.0
                   | otherwise -> 10.0
            False ->
                if | fraction <= 1 -> 1.0
                   | fraction <= 2 -> 2.0
                   | fraction <= 5 -> 5.0
                   | otherwise -> 10.0

ticks' : Float -> Float -> Float -> Float -> List Float -> List Float
ticks' min max spacing i acc =
    if | i > max -> acc
       | i < min + spacing*0.2 -> ticks' min max spacing (i+spacing) acc
       | i > max - spacing*0.2 -> ticks' min max spacing (i+spacing) acc
       | otherwise -> ticks' min max spacing (i+spacing) (i::acc)


ticks : Range -> List Float
ticks (min,max) =
    let
        maxTicks = 10
        range = niceNum (max-min) False
        spacing = niceNum (range / (maxTicks - 1)) True
        niceMin = floor (min/spacing) |> toFloat |> (*) spacing
        niceMax = ceiling (max/spacing) |> toFloat |> (*) spacing
    in
        ticks' niceMin niceMax spacing niceMin []


tickMarkX : (Float, Float) -> Normalize -> Float -> List Svg
tickMarkX (w,h) normalize t =
    let
        x' = normalize (t,0) |> fst |> toString
    in
        [ Svg.line [y2 (toString -h), x1 x', x2 x', stroke "rgb(80,80,80)"] []
        , Svg.text' [x x', dy ".71em", y "10"] [Html.text <| toString t]
        ]


tickMarkY : (Float, Float) -> Normalize -> Float -> List Svg
tickMarkY (w,h) normalize t =
    let
        y' = normalize (0,t) |> snd |> toString
    in
        [ Svg.line [x2 (toString w), y1 y', y2 y', stroke "rgb(80,80,80)"] []
        , Svg.text' [textAnchor "end", x "-10", dy ".32em", y y'] [Html.text <| Number.Format.prettyInt ',' <| round t]
        ]


yAxis : (Float, Float) -> String -> Range -> Normalize -> Svg
yAxis (w,h) label (min,max) normalize =
    ticks (min,max)
    |> List.map (tickMarkY (w,h) normalize)
    |> List.concat
    |> List.append
        [ Svg.text' [transform "rotate(-90)", y "-80", x (toString <| -h/2)] [Html.text label]
        --, Svg.text' [dy ".32em", y (toString h), x "-10"] [Html.text <| toString min]
        --, Svg.text' [x "-10", dy ".32em", y "0"] [Html.text <| toString max]
        , Svg.line [y2 <| toString h, stroke "rgb(30,30,30)"] []
        ]
    |> Svg.g []


xAxis : (Float, Float) -> String -> Range -> Normalize -> Svg
xAxis (w,h) label (min,max) normalize =
    ticks (min,max)
    |> List.map (tickMarkX (w,h) normalize)
    |> List.concat
    |> List.append
        [ Svg.text' [textAnchor "middle", y "50", x (toString <| w/2)] [Html.text label]
        --, Svg.text' [dy ".71em", y "20"] [Html.text <| toString min]
        --, Svg.text' [x (toString w), dy ".71em", y "20"] [Html.text <| toString max]
        , Svg.line [x2 <| toString w, stroke "rgb(30,30,30)"] []
        ]
    |> Svg.g [transform <| "translate(0," ++ (toString h) ++ ")"]

determineRange : List Float -> Range
determineRange data =
    let
        min = List.minimum data |> Maybe.withDefault 0
        max = List.maximum data |> Maybe.withDefault 100
    in
        if  | min /= max -> (min,max)
            | min < 0 -> (min,0)
            | max > 0 -> (0, max)
            | otherwise -> (0, 1)

type alias ScalingFn = (Range -> Float -> Float -> Float)

linearScale : ScalingFn
linearScale (min,max) scale v =
    let
        percent = (v-min) / (max-min)
    in
        percent * scale

logScale : ScalingFn
logScale (min,max) scale v =
    let
        lv = logBase 10 v
        r1 = logBase 10 max
        r0 = logBase 10 min
        percent = (lv - r0) / (r1-r0)
    in
        percent * scale

-- Dark Tango colors following http://ksrowell.com/blog-visualizing-data/2012/02/02/optimal-colors-for-graphs/
colors : List String
colors = [ "#204a87", "#ce5c00", "#4e9a06", "#a40000", "#2e3436", "#5c3566", "#8f5902", "#c4a000" ]

graph : (Float, Float) -> List Dataset -> Svg
graph (w,h) dataSets =
    let
        svgW = w + 110 + (145-90) |> toString
        svgH = h + 80 |> toString
        (xmin, xmax) = dataSets |> List.map .values |> List.concat |> List.map fst |> determineRange
        --(ymin, ymax) = (0.0, 50000000.0)
        (ymin, ymax) = dataSets |> List.map .values |> List.concat |> List.map snd |> determineRange
        normalize = (\(x, y) -> (linearScale (xmin,xmax) w x, linearScale (ymax,ymin) h y))
        xl = dataSets |> List.head |> Maybe.map .x |> Maybe.withDefault ""
        yl = dataSets |> List.head |> Maybe.map .y |> Maybe.withDefault ""
    in
        Svg.svg [width (svgW ++ "px"), height (svgH ++ "px"), viewBox ("-110 0 " ++ svgW ++ " " ++ svgH) ]
        [ Svg.g [ transform "translate(0,30)" ]
            [ xAxis (w,h) xl (xmin, xmax) normalize
            , yAxis (w,h) yl (ymin, ymax) normalize
            , dataSets
                |> List.map2 (,) colors
                |> List.map (uncurry <| scatterPlot normalize)
                |> List.reverse
                |> Svg.g []
            --, legend data
            ]
        ]

compareGraph : (Float,Float) -> (String,a->Float) -> (String,a->Float) -> List (List a) -> Svg
compareGraph (w,h) xdef ydef data =
    data
    |> List.map (xyDataset "???" xdef ydef)
    |> graph (w,h)
