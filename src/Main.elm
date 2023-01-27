module Main exposing (Model, Msg, main)

import Bitwise
import Browser
import Browser.Events
import Hex exposing (Hex)
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attributes
import Html.Events as Events exposing (onClick)
import Json.Decode as Decode
import Layout
import Random exposing (Generator)
import Simplex
import Svg exposing (Svg)
import Svg.Attributes


main : Program Int Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { hex : Hex
    , radius : Int
    , seed : Int
    , showStars : Bool
    , permTable : Simplex.PermutationTable
    , fractalConfig : Simplex.FractalConfig
    }


init : Int -> ( Model, Cmd Msg )
init time =
    let
        config : Simplex.FractalConfig
        config =
            { steps = 2
            , stepSize = 2
            , persistence = 2
            , scale = 4
            }

        permTable : Simplex.PermutationTable
        permTable =
            Simplex.permutationTableFromInt time
    in
    ( { hex = Hex.encode 0 0
      , radius = 27
      , seed = time
      , showStars = False
      , permTable = permTable
      , fractalConfig = config
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement
    | SeedChanged Int
    | ScaleChanged Float
    | StepsChanged Int
    | StepSizeChanged Float
    | PersistenceChanged Float
    | HexMoved Direction
    | ShowNoise
    | ShowStars


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowNoise ->
            ( { model | showStars = False }, Cmd.none )

        ShowStars ->
            ( { model | showStars = True }, Cmd.none )

        Increment ->
            ( { model | radius = model.radius + 1 }, Cmd.none )

        Decrement ->
            if model.radius == 0 then
                ( model, Cmd.none )

            else
                ( { model | radius = model.radius - 1 }, Cmd.none )

        SeedChanged seed ->
            ( { model | seed = seed, permTable = Simplex.permutationTableFromInt seed }, Cmd.none )

        ScaleChanged scale ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | scale = scale }
            in
            ( { model | fractalConfig = newConf }, Cmd.none )

        StepsChanged steps ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | steps = steps }
            in
            ( { model | fractalConfig = newConf }, Cmd.none )

        StepSizeChanged stepSize ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | stepSize = stepSize }
            in
            ( { model | fractalConfig = newConf }, Cmd.none )

        PersistenceChanged persistance ->
            let
                conf =
                    model.fractalConfig

                newConf =
                    { conf | persistence = persistance }
            in
            ( { model | fractalConfig = newConf }, Cmd.none )

        HexMoved dir ->
            let
                newHex =
                    case dir of
                        NW ->
                            Hex.neighbor model.hex 3

                        N ->
                            Hex.neighbor model.hex 2

                        NE ->
                            Hex.neighbor model.hex 1

                        SE ->
                            Hex.neighbor model.hex 0

                        S ->
                            Hex.neighbor model.hex 5

                        SW ->
                            Hex.neighbor model.hex 4

                        Other ->
                            model.hex

                inBounds : Bool
                inBounds =
                    List.member newHex <| Hex.neighborhood (Hex.encode 0 0) model.radius
            in
            if inBounds then
                ( { model | hex = newHex }, Cmd.none )

            else
                ( model, Cmd.none )


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


type Direction
    = NW
    | N
    | NE
    | SW
    | S
    | SE
    | Other


toDirection : String -> Msg
toDirection string =
    case String.toLower string of
        "q" ->
            HexMoved NW

        "w" ->
            HexMoved N

        "e" ->
            HexMoved NE

        "a" ->
            HexMoved SW

        "s" ->
            HexMoved S

        "d" ->
            HexMoved SE

        _ ->
            HexMoved Other


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onKeyPress keyDecoder


view : Model -> Html Msg
view model =
    div []
        [ Html.h1 [] [ text "Hexagons" ]
        , viewMap model
        , div []
            [ text ("Current Hex: " ++ viewCoords model.hex)
            , Html.br [] []
            , text "(Move with QWEASD)"
            ]
        , viewNoiseControls model
        ]


viewMap : Model -> Html Msg
viewMap model =
    div []
        [ text ("Map Radius: " ++ String.fromInt model.radius ++ " hexes")
        , div []
            [ button [ onClick Increment ]
                [ text "+ Radius" ]
            , button [ onClick Decrement ]
                [ text "- Radius" ]
            ]
        , div []
            [ button [ onClick ShowNoise ]
                [ text "Show Noise" ]
            , button [ onClick ShowStars ]
                [ text "Show Stars" ]
            ]
        , Svg.svg
            [ Svg.Attributes.width "1000"
            , Svg.Attributes.height "1000"
            , Svg.Attributes.viewBox "0 0 1000 1000"
            ]
            [ Svg.g [] (List.map (\hex -> viewHex hex ( 500.0, 500.0 ) model) <| Hex.neighborhood (Hex.encode 0 0) model.radius), viewCurrentHex ( 500.0, 500.0 ) model ]
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

        newNoise : Float
        newNoise =
            noise ^ 1

        greyScale : String
        greyScale =
            String.fromFloat (abs newNoise * 255)

        color : String
        color =
            "rgba(" ++ greyScale ++ ", " ++ greyScale ++ ", " ++ greyScale ++ ")"

        fill : String
        fill =
            if model.showStars then
                let
                    localSeed : Random.Seed
                    localSeed =
                        Random.initialSeed <|
                            round (toFloat model.seed * noise)

                    ( starRoll, _ ) =
                        Random.step (Random.float 0 1) localSeed
                in
                if (starRoll + 0.1) <= newNoise then
                    "white"

                else
                    "black"

            else
                color

        layout : Layout.Layout
        layout =
            Layout.encode
                ( 10, 10 )
                origin
    in
    Layout.render hex fill color layout


viewCurrentHex : ( Float, Float ) -> Model -> Svg msg
viewCurrentHex origin model =
    Layout.render model.hex
        "none"
        "red"
        (Layout.encode
            ( 10, 10 )
            origin
        )
