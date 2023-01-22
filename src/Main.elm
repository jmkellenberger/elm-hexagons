module Main exposing (Model, Msg, main)

import Browser
import Hex exposing (Hex)
import Html exposing (Html, div, text)
import Html.Attributes as Attributes
import Html.Events exposing (onClick)
import Layout
import Svg exposing (Svg)


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
    , flatTops : Bool
    , size : ( Float, Float )
    , origin : ( Float, Float )
    }


init : Model
init =
    { hex = Hex.encode 0 0
    , radius = 1
    , flatTops = True
    , size = ( 25, 25 )
    , origin = ( 0, 0 )
    }


type Param
    = Q
    | R
    | Radius


type Msg
    = Increment Param
    | Decrement Param
    | SwitchOrientation


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

        SwitchOrientation ->
            { model | flatTops = not model.flatTops }



-- Checked isChecked ->


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , viewOrientation model
        , viewCurrentHex model
        , viewNeighborhood model
        ]


viewOrientation : Model -> Html Msg
viewOrientation model =
    div []
        [ text "Orientation:"
        , div []
            [ radio "Flat Tops" model.flatTops SwitchOrientation
            , radio "Pointy Tops" (not model.flatTops) SwitchOrientation
            ]
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
        , Svg.svg
            []
            [ Svg.g [] [ viewHex model.hex model ] ]
        ]


viewNeighborhood : Model -> Html Msg
viewNeighborhood model =
    div []
        [ text ("Within " ++ String.fromInt model.radius ++ " hexes:")
        , div []
            [ Html.button [ onClick (Increment Radius) ]
                [ text "+ Radius" ]
            , Html.button [ onClick (Decrement Radius) ]
                [ text "- Radius" ]
            ]
        , Svg.svg []
            [ Svg.g [] (List.map (\hex -> viewHex hex model) <| Hex.neighborhood model.hex model.radius) ]
        , Html.ul [] (List.map (\hex -> Html.li [] [ text <| viewCoords hex ]) (Hex.neighborhood model.hex model.radius))
        ]


viewCoords : Hex -> String
viewCoords hex =
    Hex.toString hex


viewHex : Hex -> Model -> Svg msg
viewHex hex model =
    let
        layout : Layout.Layout
        layout =
            Layout.encode
                model.flatTops
                model.size
                model.origin
    in
    Layout.render hex layout


radio : String -> Bool -> msg -> Html msg
radio value isChecked msg =
    Html.label
        []
        [ Html.input
            [ Attributes.type_ "radio"
            , Attributes.checked isChecked
            , Attributes.name "font-size"
            , onClick msg
            ]
            []
        , text value
        ]
