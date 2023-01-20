module Hex exposing
    ( Hex
    , Layout(..)
    , Point
    , add
    , decode
    , decodeToFloat
    , distance
    , encode
    , eq
    , neighbor
    , neighborhood
    , neighbors
    , render
    , scale
    , size1
    , size2
    , subtract
    , toString
    )

import Svg exposing (Svg)
import Svg.Attributes as Attributes


type Hex
    = Hex Int Int


encode : Int -> Int -> Hex
encode q r =
    Hex q r


decode : Hex -> { q : Int, r : Int, s : Int }
decode (Hex q r) =
    { q = q
    , r = r
    , s = -q - r
    }


decodeToFloat : Hex -> { q : Float, r : Float, s : Float }
decodeToFloat (Hex q r) =
    { q = toFloat q
    , r = toFloat r
    , s = toFloat (-q - r)
    }


toString : Hex -> String
toString hex =
    let
        { q, r, s } =
            decode hex
    in
    List.foldl (\n acc -> acc ++ n) "" <|
        List.intersperse ", " <|
            List.map String.fromInt [ q, r, s ]


eq : Hex -> Hex -> Bool
eq (Hex q1 r1) (Hex q2 r2) =
    q1 == q2 && r1 == r2


add : Hex -> Hex -> Hex
add (Hex q1 r1) (Hex q2 r2) =
    Hex (q1 + q2) (r1 + r2)


subtract : Hex -> Hex -> Hex
subtract (Hex q1 r1) (Hex q2 r2) =
    Hex (q1 - q2) (r1 - r2)


scale : Int -> Hex -> Hex
scale scalar (Hex q r) =
    Hex (scalar * q) (scalar * r)


length : Hex -> Int
length hex =
    let
        { q, r, s } =
            decode hex
    in
    List.foldr (\n acc -> acc + abs n) 0 [ q, r, s ]
        // 2


distance : Hex -> Hex -> Int
distance hex1 hex2 =
    length <|
        subtract hex1 hex2


directions : Int -> Hex
directions dir =
    case dir of
        1 ->
            Hex 1 0

        2 ->
            Hex 1 -1

        3 ->
            Hex 0 -1

        4 ->
            Hex -1 0

        5 ->
            Hex -1 1

        6 ->
            Hex 0 1

        n ->
            modBy n 6
                |> directions


neighbor : Hex -> Int -> Hex
neighbor hex dir =
    add hex (directions dir)


neighbors : Hex -> List Hex
neighbors hex =
    List.map (neighbor hex) (List.range 1 6)


neighborhood : Hex -> Int -> List Hex
neighborhood hex radius =
    let
        qs : List Int
        qs =
            List.range -radius radius

        rs : Int -> List Int
        rs dq =
            List.range
                (max -radius (-dq - radius))
                (min radius (-dq + radius))
    in
    List.concatMap
        (\dq ->
            List.map
                (\dr -> add hex (Hex dq dr))
                (rs dq)
        )
        qs


type Layout
    = PointyTops { size : Point, origin : Point }
    | FlatTops { size : Point, origin : Point }


renderParameters : Layout -> ( Orientation, Point, Point )
renderParameters layout =
    case layout of
        PointyTops { size, origin } ->
            ( pointy, size, origin )

        FlatTops { size, origin } ->
            ( flat, size, origin )


type alias Orientation =
    { f0 : Float
    , f1 : Float
    , f2 : Float
    , f3 : Float
    , b1 : Float
    , b2 : Float
    , b3 : Float
    , b4 : Float
    , start_angle : Float
    }


pointy : Orientation
pointy =
    Orientation
        (sqrt 3.0)
        (sqrt 3.0 / 2)
        0.0
        (3.0 / 2.0)
        (sqrt 3.0 / 3.0)
        (-1.0 / 3.0)
        0.0
        (2.0 / 3.0)
        0.5


flat : Orientation
flat =
    Orientation
        (3.0 / 2.0)
        0.0
        (sqrt 3.0 / 2.0)
        (sqrt 3.0)
        (2.0 / 3.0)
        0.0
        (-1.0 / 3.0)
        (sqrt 3.0 / 3.0)
        0.0


type alias Point =
    { x : Float, y : Float }


size1 : Float -> Point
size1 size =
    Point size size


size2 : Float -> Maybe Float -> Point
size2 x maybeY =
    case maybeY of
        Just y ->
            Point x y

        Nothing ->
            size1 x


toPixel : Hex -> Layout -> Point
toPixel hex layout =
    let
        { q, r } =
            decodeToFloat hex

        ( orientation, size, origin ) =
            renderParameters layout

        x : Float
        x =
            (orientation.f0 * q + orientation.f1 * r)
                * size.x
                + origin.x

        y : Float
        y =
            (orientation.f2 * q + orientation.f3 * r)
                * size.y
                + origin.y
    in
    Point x y


cornerOffset : Int -> Layout -> Point
cornerOffset corner layout =
    let
        ( orientation, size, _ ) =
            renderParameters layout

        angle : Float
        angle =
            2.0
                * pi
                * (orientation.start_angle + toFloat corner)
                / 6
    in
    Point (size.x * cos angle) (size.y * sin angle)


corners : Hex -> Layout -> List Point
corners hex layout =
    let
        center : Point
        center =
            toPixel hex layout

        offsets : List Point
        offsets =
            List.map
                (\corner -> cornerOffset corner layout)
                (List.range 0 5)
    in
    List.map
        (\offset -> Point (center.x + offset.x) (center.y + offset.y))
        offsets


render : Hex -> Layout -> Svg msg
render hex layout =
    let
        pointToString : Point -> String
        pointToString { x, y } =
            String.fromFloat x ++ "," ++ String.fromFloat y

        path : String
        path =
            corners hex layout
                |> List.map pointToString
                |> String.join " "
    in
    Svg.polygon
        [ Attributes.points path
        , Attributes.stroke "black"
        , Attributes.fill "lightgrey"
        ]
        [ Svg.text <| toString hex ]
