module Day1 exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (cols, rows)
import Html.Events exposing (onInput)



-- SOLUTION


solution : String -> Int
solution input =
    Maybe.withDefault 0
        (input
            |> String.lines
            |> parse
            |> List.map List.sum
            |> List.maximum
        )


type alias ParsingState =
    { parsed : List (List Int)
    , current : List Int
    , remaining : List String
    }


parse : List String -> List (List Int)
parse lines =
    let
        result =
            parseRec (ParsingState [] [] lines)
    in
    result.parsed


parseRec : ParsingState -> ParsingState
parseRec state =
    case state.remaining of
        head :: tail ->
            if String.isEmpty head then
                parseRec (ParsingState (state.parsed ++ [ state.current ]) [] tail)

            else
                parseRec (ParsingState state.parsed (state.current ++ [ Maybe.withDefault 0 (String.toInt head) ]) tail)

        [] ->
            if List.isEmpty state.current then
                state

            else
                ParsingState (state.parsed ++ [ state.current ]) [] []



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    """1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"""



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
        [ div [] [ text (String.fromInt (solution model)) ]
        , textarea [ cols 20, rows 20, onInput InputChange ] [ text model ]
        ]
