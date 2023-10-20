module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Dict exposing (update)
import Html exposing (mark)
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Border as Border

type alias Model = { tablero : Tablero }
type alias Tablero = Dict (Int, Int) Mark

type Mark = X | O

initialModel : Model
initialModel = 
            { tablero = Dict.empty 
                |> Dict.insert (1,1) X 
                |> Dict.insert (0,2) O 
                |> Dict.insert (1,3) X }

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    model

viewGrid : Tablero -> Element Msg
viewGrid tablero =
    let
        cellContent coords = 
            Dict.get coords tablero
            |> Maybe.map (\mark ->
                case mark of X -> "X"
                            O -> "O"
                            )
            |> Maybe.withDefault ""

        cell (row,column)  = Element.el [
            Element.width <| Element.px 50,
            Element.height <| Element.px 50,
            Border.width 1
            ] 
            <| 
               Element.el [Element.centerX, Element.centerY] (Element.text content)    
    in

    Element.column [Element.spacing 5] [
        Element.row [Element.spacing 5] [cell (0,0), cell (0,1), cell (0,2)]
        , Element.row [Element.spacing 5] [cell (1,0), cell (1,1), cell (1,2)]
        , Element.row [Element.spacing 5] [cell (2,0), cell (2,1), cell (2,2)]
    ]

view : Model -> Html Msg
view model =
    Element.layout [] 
    (Element.column [Element.centerX, Element.centerY] 
        [ Element.el [Element.centerX, Element.centerY ] <| 
            Element.text "tres en raya"
            , viewGrid model.tablero
        ]
    )

main : Program () Model Msg
main = 
    Browser.sandbox
    {
        init = initialModel
        , view = view
        , update = update
    }