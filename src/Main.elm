module Main exposing (Model, Msg, Orientation, main)

import Browser
import Hex exposing (Hex)
import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events exposing (onClick, onInput, targetChecked)
import Svg exposing (Svg)
import Svg.Attributes exposing (orientation)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type Orientation
    = Flat
    | Pointy


type alias Model =
    { hex : Hex
    , radius : Int
    , orientation : Orientation
    }


pointy : Hex.Layout
pointy =
    Hex.PointyTops { size = Hex.Point 15 15, origin = Hex.Point 0 0 }


flat : Hex.Layout
flat =
    Hex.FlatTops { size = Hex.Point 15 15, origin = Hex.Point 0 0 }


init : Model
init =
    { hex = Hex.encode 0 0
    , radius = 1
    , orientation = Flat
    }


type Param
    = Q
    | R
    | Radius


type Msg
    = Increment Param
    | Decrement Param
    | SwitchTo Orientation


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

        SwitchTo orientation ->
            case orientation of
                Pointy ->
                    { model | orientation = Pointy }

                Flat ->
                    { model | orientation = Flat }



-- Checked isChecked ->


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , p [] [ text "Orientation:" ]
        , div []
            [ radio "Flat Tops" (model.orientation == Flat) (SwitchTo Flat)
            , radio "Pointy Tops" (model.orientation == Pointy) (SwitchTo Pointy)
            ]
        , p [] [ text <| "Current Hex: " ++ viewHex model.hex ]
        , div []
            [ button [ onClick <| Increment Q ] [ text "+ Q" ]
            , button [ onClick <| Decrement Q ] [ text "- Q" ]
            , button [ onClick <| Increment R ] [ text "+ R" ]
            , button [ onClick <| Decrement R ] [ text "- R" ]
            ]
        , Svg.svg []
            [ Svg.g [] [ renderHex model.hex model ] ]
        , p [] [ text "Neighbors:" ]
        , Svg.svg []
            [ Svg.g [] (List.map (\hex -> renderHex hex model) <| Hex.neighbors model.hex) ]
        , ul [] (List.map (\hex -> Html.li [] [ text <| viewHex hex ]) (Hex.neighbors model.hex))
        , p [] [ text ("Within " ++ String.fromInt model.radius ++ " hexes:") ]
        , div []
            [ button [ onClick <| Increment Radius ] [ text "+ Radius" ]
            , button [ onClick <| Decrement Radius ] [ text "- Radius" ]
            ]
        , Svg.svg []
            [ Svg.g [] (List.map (\hex -> renderHex hex model) <| Hex.neighborhood model.hex model.radius) ]
        , ul [] (List.map (\hex -> li [] [ text <| viewHex hex ]) (Hex.neighborhood model.hex model.radius))
        ]


viewHex : Hex -> String
viewHex hex =
    Hex.toString hex


renderHex : Hex -> Model -> Svg msg
renderHex hex model =
    case model.orientation of
        Pointy ->
            Hex.render hex pointy

        Flat ->
            Hex.render hex flat


radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    label
        []
        [ input [ Attributes.type_ "radio", Attributes.checked isChecked, Attributes.name "font-size", onClick msg ] []
        , text value
        ]
