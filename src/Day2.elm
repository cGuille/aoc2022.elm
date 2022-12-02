module Day2 exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)



-- SOLUTION


part1 : String -> Int
part1 input =
    solve score1 input


part2 : String -> Int
part2 input =
    solve score2 input


solve : (String -> Int) -> String -> Int
solve scoreFunc input =
    input
        |> String.lines
        |> List.map scoreFunc
        |> List.sum



-- A: they pick rock
-- B: they pick paper
-- C: they pick scissors
-- X: I pick rock (+1pt)
-- Y: I pick paper (+1pt)
-- Z: I pick scissors (+1pt)
--
-- Win: 6pts
-- Draw: 3pts
-- Loss: 0pt
score1 : String -> Int
score1 line =
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

-- A: they pick rock
-- B: they pick paper
-- C: they pick scissors
-- X: I need to lose
-- Y: I need to draw
-- Z: I pick win
--
-- Win: 6pts
-- Draw: 3pts
-- Loss: 0pt
-- Picking rock: +1pt
-- Picking paper: +2pt
-- Picking scissors: +3pt
score2 : String -> Int
score2 line =
    case line of
        "A X" -> 3 + 0 -- Rock, so I pick scissors to lose
        "B X" -> 1 + 0 -- Paper, so I pick rock to lose
        "C X" -> 2 + 0 -- Scissors, so I pick paper to lose
        "A Y" -> 1 + 3 -- Rock, so I pick rock to draw
        "B Y" -> 2 + 3 -- Paper, so I pick paper to draw
        "C Y" -> 3 + 3 -- Scissors, so I pick scissors to draw
        "A Z" -> 2 + 6 -- Rock, so I pick paper to win
        "B Z" -> 3 + 6 -- Paper, so I pick scissors
        "C Z" -> 1 + 6 -- Scissors, so I pick rock
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
        , div [] [ text (String.fromInt (part2 model)) ]
        , textarea [ cols 20, rows 20, onInput InputChange ] [ text model ]
        ]
