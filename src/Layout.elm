module Layout exposing (Layout, Point, encode, render)

import Hex exposing (Hex)
import Svg exposing (Svg)
import Svg.Attributes as Attributes


type alias Point =
    ( Float, Float )


type alias Matrix2 =
    { f0 : Float
    , f1 : Float
    , f2 : Float
    , f3 : Float
    }


type alias Orientation =
    { forwardMatrix : Matrix2
    , inverseMatrix : Matrix2
    , startAngle : Float
    }


type Layout
    = Layout
        { orientation : Orientation
        , size : Point
        , origin : Point
        }


encode : Point -> Point -> Layout
encode size origin =
    Layout
        { orientation = orientationFlat
        , size = size
        , origin = origin
        }


floatRound : Int -> Float -> Float
floatRound precision number =
    let
        k : Float
        k =
            toFloat <| 10 ^ precision
    in
    (toFloat << round) (number * k) / k


orientationFlat : Orientation
orientationFlat =
    { forwardMatrix =
        { f0 = 3.0 / 2.0
        , f1 = 0.0
        , f2 = sqrt 3.0 / 2
        , f3 = sqrt 3
        }
    , inverseMatrix =
        { f0 = 2.0 / 3.0
        , f1 = 0.0
        , f2 = -1.0 / 3.0
        , f3 = 1.0 / sqrt 3
        }
    , startAngle = 0
    }


centerPoint : Layout -> Hex -> Point
centerPoint (Layout layout) hex =
    let
        { f0, f1, f2, f3 } =
            layout.orientation.forwardMatrix

        ( sX, sY ) =
            layout.size

        ( oX, oY ) =
            layout.origin

        { q, r } =
            Hex.decodeFloat hex

        x : Float
        x =
            floatRound 2 <|
                (((f0 * q) + (f1 * r)) * sX)
                    + oX

        y : Float
        y =
            floatRound 2 <|
                (((f2 * q) + (f3 * r)) * sY)
                    + oY
    in
    ( x, y )


cornerOffset : Layout -> Int -> Point
cornerOffset (Layout layout) corner =
    let
        ( sX, sY ) =
            layout.size

        startAngle : Float
        startAngle =
            layout.orientation.startAngle

        angle : Float
        angle =
            ((2.0 * pi)
                * (toFloat corner + startAngle)
            )
                / 6

        x : Float
        x =
            floatRound 2 (sX * cos angle)

        y : Float
        y =
            floatRound 2 (sY * sin angle)
    in
    ( x, y )


corners : Hex -> Layout -> List Point
corners hex layout =
    let
        ( cX, cY ) =
            centerPoint layout hex

        offsets : List Point
        offsets =
            List.map
                (cornerOffset layout)
                (List.range 0 5)

        toScreenSpace : Point -> Point
        toScreenSpace ( x, y ) =
            ( floatRound 2 (x + cX)
            , floatRound 2 (y + cY)
            )
    in
    List.map toScreenSpace offsets


render : Hex -> String -> String -> Layout -> Svg msg
render hex color outline layout =
    let
        pointToString : Point -> String
        pointToString ( x, y ) =
            String.fromFloat x ++ "," ++ String.fromFloat y

        path : String
        path =
            corners hex layout
                |> List.map pointToString
                |> String.join " "
    in
    Svg.polygon
        [ Attributes.points path
        , Attributes.stroke outline
        , Attributes.fill color
        ]
        [ Svg.text <| Hex.toString hex ]
