module Hex exposing
    ( Hex
    , add
    , decode
    , distance
    , encode
    , eq
    , neighbor
    , neighbors
    , scale
    , subtract
    , toString
    )


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


toString : Hex -> String
toString hex =
    let
        { q, r, s } =
            decode hex
    in
    List.foldl (\n acc -> acc ++ " " ++ String.fromInt n) "" [ q, r, s ]


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
