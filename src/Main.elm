module Main exposing (main)

import Browser
import Dict exposing (Dict)
import Element as E
import Element.Border as Border
import Element.Events exposing (onClick)
import Element.Input as Input
import Html exposing (Html, button, div, text)



type alias Model =
    { tablero : Tablero, turno : Mark, ganador : Maybe Mark }


type alias Tablero =
    Dict ( Int, Int ) Mark


type Mark
    = X
    | O


initialModel : Model
initialModel =
    { tablero = Dict.empty, turno = X, ganador = Nothing }


type Msg
    = Marcar Int Int Mark
    | Reset


siguienteMarca : Mark -> Mark
siguienteMarca mark =
    case mark of
        X ->
            O

        O ->
            X


update : Msg -> Model -> Model
update msg model =
    case msg of
        Marcar row col mark ->
            let
                actualizarTablero =
                    Dict.insert ( row, col ) mark model.tablero
            in
            if Dict.member ( row, col ) model.tablero then
                model

            else
                { model
                    | tablero = actualizarTablero
                    , turno = siguienteMarca model.turno
                }
        Reset ->
            initialModel





view : Model -> Html Msg
view model =
    let

        tablero =
            E.column [ E.spacing 5, E.centerX, E.centerY ]
                [ cellRow model 0
                , cellRow model 1
                , cellRow model 2
                ]

        info =
            E.row [ E.spaceEvenly ]
                [ E.text "turno de "
                , ponerMarca model.turno
                , Input.button [] { onPress = Just Reset, label = E.text " Reset" }
                ]
    in
    E.layout [] <|
        E.column [ E.spacing 15, E.centerX, E.centerY ]
            [ tablero, info ]


cellRow : Model -> Int -> E.Element Msg
cellRow model row =
    E.row [ E.spacing 5 ]
        [ cell model row 0
        , cell model row 1
        , cell model row 2
        ]


ponerMarca : Mark -> E.Element Msg
ponerMarca mark =
    case mark of
        X ->
            E.text "X"

        o ->
            E.text "O"


cell : Model -> Int -> Int -> E.Element Msg
cell model row col =
    let
        valor =
            Dict.get ( row, col ) model.tablero
                |> Maybe.map (\mark -> ponerMarca mark)
                |> Maybe.withDefault E.none
    in
    E.el
        [ E.width <| E.px 100
        , E.height <| E.px 100
        , Border.solid
        , Border.width 1
        , onClick <| Marcar row col model.turno
        ]
        (E.el [ E.centerX, E.centerY ] valor)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
