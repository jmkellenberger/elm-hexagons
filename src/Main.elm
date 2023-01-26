module Main exposing (Model, Msg, main)

import Browser
import Hex exposing (Hex)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attributes
import Html.Events as Events exposing (onClick)
import Layout
import Simplex
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
    , seed : Int
    , permTable : Simplex.PermutationTable
    , fractalConfig : Simplex.FractalConfig
    }


init : Model
init =
    let
        config : Simplex.FractalConfig
        config =
            { steps = 2
            , stepSize = 2
            , persistence = 2
            , scale = 4
            }

        seed : number
        seed =
            1

        permTable : Simplex.PermutationTable
        permTable =
            Simplex.permutationTableFromInt seed
    in
    { hex = Hex.encode 0 0
    , radius = 27
    , seed = seed
    , permTable = permTable
    , fractalConfig = config
    }


type Param
    = Q
    | R
    | Radius


type Msg
    = Increment Param
    | Decrement Param
    | SeedChanged Int
    | ScaleChanged Float
    | StepsChanged Int
    | StepSizeChanged Float
    | PersistenceChanged Float


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

        SeedChanged seed ->
            { model | seed = seed, permTable = Simplex.permutationTableFromInt seed }

        ScaleChanged scale ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | scale = scale }
            in
            { model | fractalConfig = newConf }

        StepsChanged steps ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | steps = steps }
            in
            { model | fractalConfig = newConf }

        StepSizeChanged stepSize ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | stepSize = stepSize }
            in
            { model | fractalConfig = newConf }

        PersistenceChanged persistance ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | persistence = persistance }
            in
            { model | fractalConfig = newConf }


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , viewMap model
        , viewCurrentHex model
        , viewNoiseControls model
        ]


viewMap : Model -> Html Msg
viewMap model =
    div []
        [ text ("Map Radius: " ++ String.fromInt model.radius ++ " hexes")
        , div []
            [ button [ onClick (Increment Radius) ]
                [ text "+ Radius" ]
            , button [ onClick (Decrement Radius) ]
                [ text "- Radius" ]
            ]
        , Svg.svg
            [ Svg.Attributes.width "1000"
            , Svg.Attributes.height "1000"
            , Svg.Attributes.viewBox "0 0 1000 1000"
            ]
            [ Svg.g [] (List.map (\hex -> viewHex hex ( 500.0, 500.0 ) model) <| Hex.neighborhood (Hex.encode 0 0) model.radius) ]
        ]


viewCurrentHex : Model -> Html Msg
viewCurrentHex model =
    div []
        [ text ("Current Hex: " ++ viewCoords model.hex)
        , div []
            [ button [ onClick (Increment Q) ] [ text "+ Q" ]
            , button [ onClick (Decrement Q) ] [ text "- Q" ]
            , button [ onClick (Increment R) ] [ text "+ R" ]
            , button [ onClick (Decrement R) ] [ text "- R" ]
            ]
        ]


viewNoiseControls : Model -> Html Msg
viewNoiseControls model =
    div []
        [ div []
            [ Html.label [ Attributes.for "seed" ] [ text "Seed: " ]
            , input
                [ Events.onInput (\val -> SeedChanged (String.toInt val |> Maybe.withDefault 0))
                , Attributes.type_ "number"
                , Attributes.id "seed"
                , Attributes.value (String.fromInt model.seed)
                ]
                []
            ]
        , div []
            [ Html.label [ Attributes.for "scale" ] [ text "Scale: " ]
            , input
                [ Attributes.type_ "number"
                , Events.onInput (\val -> ScaleChanged (String.toFloat val |> Maybe.withDefault 0))
                , Attributes.id "scale"
                , Attributes.value (String.fromFloat model.fractalConfig.scale)
                ]
                []
            ]
        , div []
            [ Html.label [ Attributes.for "steps" ] [ text "Steps: " ]
            , input
                [ Events.onInput (\val -> StepsChanged (String.toInt val |> Maybe.withDefault 0))
                , Attributes.type_ "number"
                , Attributes.id "steps"
                , Attributes.value (String.fromInt model.fractalConfig.steps)
                ]
                []
            ]
        , div []
            [ Html.label [ Attributes.for "stepSize" ] [ text "Step Size: " ]
            , input
                [ Events.onInput (\val -> StepSizeChanged (String.toFloat val |> Maybe.withDefault 0))
                , Attributes.type_ "number"
                , Attributes.id "stepSize"
                , Attributes.value (String.fromFloat model.fractalConfig.stepSize)
                ]
                []
            ]
        , div []
            [ Html.label [ Attributes.for "persistence" ] [ text "Persistence" ]
            , input
                [ Events.onInput (\val -> PersistenceChanged (String.toFloat val |> Maybe.withDefault 0))
                , Attributes.type_ "number"
                , Attributes.id "persistance"
                , Attributes.value (String.fromFloat model.fractalConfig.persistence)
                ]
                []
            ]
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
            (Simplex.fractal2d model.fractalConfig model.permTable q r
                + 1
            )
                / 2

        greyScale : String
        greyScale =
            String.fromFloat (abs noise * 255)

        color =
            "rgba(" ++ greyScale ++ ", " ++ greyScale ++ ", " ++ greyScale ++ ")"

        outline : String
        outline =
            if Hex.eq hex model.hex then
                "orange"
                -- else if List.member hex (Hex.neighborhood model.hex 4) then
                --     "yellow"

            else
                "black"

        layout : Layout.Layout
        layout =
            Layout.encode
                True
                ( 10, 10 )
                origin
    in
    Layout.render hex color outline layout
