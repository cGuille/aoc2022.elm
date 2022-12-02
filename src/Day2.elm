module Day2 exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)



-- SOLUTION


part1 : String -> Int
part1 input =
    input
        |> String.lines
        |> List.map score
        |> List.sum

score : String -> Int
score line =
    case line of
        "A X" -> 3 + 1
        "B X" -> 0 + 1
        "C X" -> 6 + 1
        "A Y" -> 6 + 2
        "B Y" -> 3 + 2
        "C Y" -> 0 + 2
        "A Z" -> 0 + 3
        "B Z" -> 6 + 3
        "C Z" -> 3 + 3
        _ -> 0



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    "A Y\nB X\nC Z\n"



-- UPDATE


type Msg
    = InputChange String


update : Msg -> Model -> Model
update msg model =
    case msg of
        InputChange newInput ->
            newInput



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text (String.fromInt (part1 model)) ]
        , textarea [ cols 20, rows 20, onInput InputChange ] [ text model ]
        ]
