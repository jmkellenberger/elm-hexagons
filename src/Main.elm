module Main exposing (Model, Msg, main)

import Browser
import Hex exposing (Hex)
import Html exposing (Html, div, p, text)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Svg


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
    , layout : Hex.Layout
    }


init : Model
init =
    { hex = Hex.encode 0 0
    , radius = 1
    , layout = Hex.FlatTops { size = Hex.Point 15 15, origin = Hex.Point 0 0 }
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


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , p [] [ text <| "Current Hex: " ++ viewHex model.hex ]
        , div []
            [ Html.button [ onClick <| Increment Q ] [ text "+ Q" ]
            , Html.button [ onClick <| Decrement Q ] [ text "- Q" ]
            , Html.button [ onClick <| Increment R ] [ text "+ R" ]
            , Html.button [ onClick <| Decrement R ] [ text "- R" ]
            ]
        , Svg.svg []
            [ Svg.g [] [ Hex.render model.hex model.layout ] ]
        , p [] [ text "Neighbors:" ]
        , Svg.svg []
            [ Svg.g [] (List.map (\hex -> Hex.render hex model.layout) <| Hex.neighbors model.hex) ]
        , Html.ul [] (List.map (\hex -> Html.li [] [ text <| viewHex hex ]) (Hex.neighbors model.hex))
        , p [] [ text ("Within " ++ String.fromInt model.radius ++ " hexes:") ]
        , div []
            [ Html.button [ onClick <| Increment Radius ] [ text "+ Radius" ]
            , Html.button [ onClick <| Decrement Radius ] [ text "- Radius" ]
            ]
        , Svg.svg []
            [ Svg.g [] (List.map (\hex -> Hex.render hex model.layout) <| Hex.neighborhood model.hex model.radius) ]
        , Html.ul [] (List.map (\hex -> Html.li [] [ text <| viewHex hex ]) (Hex.neighborhood model.hex model.radius))
        ]


viewHex : Hex -> String
viewHex hex =
    Hex.toString hex
