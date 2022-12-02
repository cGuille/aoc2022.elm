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



score1 : String -> Int
score1 line =
    case line of
        "A X" -> 3 + 1 -- They pick rock, I draw (3pts) with rock (+1pt)
        "B X" -> 0 + 1 -- They pick paper, I lose (0pt) with rock (+1pt)
        "C X" -> 6 + 1 -- They pick scissors, I win (6pt) with rock (+1pt)
        "A Y" -> 6 + 2 -- They pick rock, I win (6pt) with paper (+2pt)
        "B Y" -> 3 + 2 -- They pick paper, I draw (3pts) with paper (+2pt)
        "C Y" -> 0 + 2 -- They pick scissors, I lose (0pt) with paper (+2pt)
        "A Z" -> 0 + 3 -- They pick rock, I lose (0pt) with scissors (+3pt)
        "B Z" -> 6 + 3 -- They pick paper, I win (6pt) with scissors (+3pt)
        "C Z" -> 3 + 3 -- They pick scissors, I draw (3pts) with scissors (+3pt)
        _ -> 0


score2 : String -> Int
score2 line =
    case line of
        "A X" -> 3 + 0 -- They pick rock, so I pick scissors (3pts) to lose (+0pt)
        "B X" -> 1 + 0 -- They pick paper, so I pick rock (1pt) to lose (+0pt)
        "C X" -> 2 + 0 -- They pick scissors, so I pick paper (2pts) to lose (+0pt)
        "A Y" -> 1 + 3 -- They pick rock, so I pick rock (1pt) to draw (+3pts)
        "B Y" -> 2 + 3 -- They pick paper, so I pick paper (2pts) to draw (+3pts)
        "C Y" -> 3 + 3 -- They pick scissors, so I pick scissors (3pts) to draw (+3pts)
        "A Z" -> 2 + 6 -- They pick rock, so I pick paper (2pts) to win (+6pts)
        "B Z" -> 3 + 6 -- They pick paper, so I pick scissors (3pts) to win (+6pts)
        "C Z" -> 1 + 6 -- They pick scissors, so I pick rock (1pt) to win (+6pts)
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
