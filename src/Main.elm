module Main exposing (main)

import Browser
import Html exposing (Html)


main : Program () Int msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    Int


init : Model
init =
    0


update : msg -> Model -> Model
update msg model =
    case msg of
        _ ->
            model


view : Model -> Html msg
view _ =
    Html.div [] [ Html.text "Hello World" ]
