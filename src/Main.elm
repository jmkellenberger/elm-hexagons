module Main exposing (Msg, main)

import Browser
import Hex exposing (Hex)
import Html exposing (Html, div, p, text)
import Html.Events exposing (onClick)


main : Program () Hex Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Hex


init : Model
init =
    Hex.encode 0 0


type Msg
    = IncrementQ
    | IncrementR


update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementQ ->
            Hex.add model (Hex.encode 1 0)

        IncrementR ->
            Hex.add model (Hex.encode 0 1)


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text <| viewHex model ]
        , p [] [ text "Neigbors:" ]
        , Html.ol [] (List.map (\hex -> Html.li [] [ text <| viewHex hex ]) (Hex.neighbors model))
        , Html.button [ onClick IncrementQ ] [ text "+ Q" ]
        , Html.button [ onClick IncrementR ] [ text "+ R" ]
        ]


viewHex : Hex -> String
viewHex hex =
    Hex.toString hex
