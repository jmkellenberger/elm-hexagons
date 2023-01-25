module Hex exposing
    ( Hex
    , add
    , decodeFloat
    , encode
    , eq
    , neighborhood
    , neighbors
    , rectangle
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


decodeFloat : Hex -> { q : Float, r : Float, s : Float }
decodeFloat (Hex q r) =
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
    length
        (subtract hex1 hex2)


directions : Int -> Hex
directions dir =
    case dir of
        0 ->
            Hex 1 0

        1 ->
            Hex 1 -1

        2 ->
            Hex 0 -1

        3 ->
            Hex -1 0

        4 ->
            Hex -1 1

        5 ->
            Hex 0 1

        n ->
            modBy n 6
                |> directions


neighbor : Hex -> Int -> Hex
neighbor hex dir =
    add hex (directions dir)


neighbors : Hex -> List Hex
neighbors hex =
    List.map (neighbor hex) (List.range 0 5)



-- SHAPE GENERATION


cartesian : List Int -> List Int -> List ( Int, Int )
cartesian xs ys =
    List.concatMap (\x -> List.map (\y -> ( x, y )) ys) xs


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


rectangle : Int -> Int -> Int -> Int -> List Hex
rectangle x1 x2 y1 y2 =
    let
        xs : List Int
        xs =
            List.range x1 x2

        qOffset : Int -> Int
        qOffset q_ =
            floor (toFloat q_ / 2.0)
    in
    List.concatMap
        (\x ->
            List.map
                (\y ->
                    encode x
                        y
                )
                (List.range
                    (y1
                        - qOffset x
                    )
                    (y2 - qOffset x)
                )
        )
        xs
