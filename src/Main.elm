module Main exposing (Model, Msg, main)

import Browser
import Hex exposing (Hex)
import Html exposing (Html, div, text)
import Html.Events exposing (onClick)
import Layout
import Noise
import Random
import Svg exposing (Svg)
import Svg.Attributes


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { hex : Hex
    , radius : Int
    , permTable : Noise.PermutationTable
    }


init : Model
init =
    let
        ( permTable, _ ) =
            Noise.permutationTable (Random.initialSeed 1)
    in
    { hex = Hex.encode 0 0
    , radius = 27
    , permTable = permTable
    }


type Param
    = Q
    | R
    | Radius


type Msg
    = Increment Param
    | Decrement Param


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment param ->
            case param of
                Q ->
                    { model | hex = Hex.add model.hex (Hex.encode 1 0) }

                R ->
                    { model | hex = Hex.add model.hex (Hex.encode 0 1) }

                Radius ->
                    { model | radius = model.radius + 1 }

        Decrement param ->
            case param of
                Q ->
                    { model | hex = Hex.add model.hex (Hex.encode -1 0) }

                R ->
                    { model | hex = Hex.add model.hex (Hex.encode 0 -1) }

                Radius ->
                    if model.radius == 0 then
                        model

                    else
                        { model | radius = model.radius - 1 }



-- Checked isChecked ->


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , viewNeighborhood model
        , viewCurrentHex model
        ]


viewCurrentHex : Model -> Html Msg
viewCurrentHex model =
    div []
        [ text ("Current Hex: " ++ viewCoords model.hex)
        , div []
            [ Html.button [ onClick (Increment Q) ] [ text "+ Q" ]
            , Html.button [ onClick (Decrement Q) ] [ text "- Q" ]
            , Html.button [ onClick (Increment R) ] [ text "+ R" ]
            , Html.button [ onClick (Decrement R) ] [ text "- R" ]
            ]
        ]


viewNeighborhood : Model -> Html Msg
viewNeighborhood model =
    div []
        [ text ("Map Radius: " ++ String.fromInt model.radius ++ " hexes")
        , div []
            [ Html.button [ onClick (Increment Radius) ]
                [ text "+ Radius" ]
            , Html.button [ onClick (Decrement Radius) ]
                [ text "- Radius" ]
            ]
        , Svg.svg
            [ Svg.Attributes.width "1000"
            , Svg.Attributes.height "1000"
            , Svg.Attributes.viewBox "0 0 1000 1000"
            ]
            [ Svg.g [] (List.map (\hex -> viewHex hex ( 500.0, 500.0 ) model) <| Hex.neighborhood (Hex.encode 0 0) model.radius) ]
        ]


viewCoords : Hex -> String
viewCoords hex =
    Hex.toString hex


viewHex : Hex -> ( Float, Float ) -> Model -> Svg msg
viewHex hex origin model =
    let
        { q, r } =
            Hex.decodeFloat hex

        noise : Float
        noise =
            Noise.noise2d model.permTable (q * 0.15) (r * 0.18)

        color : String
        color =
            if noise < 0 then
                "black"

            else if noise < 0.25 then
                "darkgray"

            else if noise < 0.5 then
                "gray"

            else if noise < 0.75 then
                "lightgrey"

            else
                "white"

        outline : String
        outline =
            if Hex.eq hex model.hex then
                "orange"

            else if List.member hex (Hex.neighbors model.hex) then
                "lightgreen"

            else
                "lightblue"

        layout : Layout.Layout
        layout =
            Layout.encode
                True
                ( 10, 10 )
                origin
    in
    Layout.render hex color outline layout
