module Day1 exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onInput)



-- SOLUTION


part1 : String -> Int
part1 input =
    Maybe.withDefault 0
        (input
            |> String.lines
            |> parse
            |> List.map List.sum
            |> List.maximum
        )

part2 : String -> Int
part2 input =
    input
        |> String.lines
        |> parse
        |> List.map List.sum
        |> List.sortWith reverseCompare
        |> List.take 3
        |> List.sum

reverseCompare a b =
    case compare a b of
      LT -> GT
      EQ -> EQ
      GT -> LT

type alias ParsingState =
    { result : List (List Int)
    , current : List Int
    , remaining : List String
    }


parse : List String -> List (List Int)
parse lines =
    let
        finalState =
            parseRec (ParsingState [] [] lines)
    in
    finalState.result


parseRec : ParsingState -> ParsingState
parseRec state =
    case state.remaining of
        head :: tail ->
            if String.isEmpty head then
                parseRec (ParsingState (state.result ++ [ state.current ]) [] tail)

            else
                parseRec (ParsingState state.result (state.current ++ [ Maybe.withDefault 0 (String.toInt head) ]) tail)

        [] ->
            if List.isEmpty state.current then
                state

            else
                ParsingState (state.result ++ [ state.current ]) [] []



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
        [ div [ class "solution", class "part1" ] [ text (String.fromInt (part1 model)) ]
        , div [ class "solution", class "part2" ] [ text (String.fromInt (part2 model)) ]
        , textarea [ class "input", cols 20, rows 20, onInput InputChange ] [ text model ]
        ]
