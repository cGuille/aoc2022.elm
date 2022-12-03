module Day3 exposing (..)

import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onInput)
import Set exposing (Set)



-- SOLUTION


part1 : String -> Int
part1 input =
    String.lines input
        |> List.map parseCompartments
        |> List.map compartmentsIntersect
        |> List.map itemsPriority
        |> List.sum


parseCompartments : String -> ( Set Char, Set Char )
parseCompartments rucksack =
    let
        half_len =
            String.length rucksack // 2
    in
    ( String.left half_len rucksack
        |> String.toList
        |> Set.fromList
    , String.right half_len rucksack
        |> String.toList
        |> Set.fromList
    )


compartmentsIntersect : ( Set Char, Set Char ) -> Set Char
compartmentsIntersect compartments =
    Set.intersect (Tuple.first compartments) (Tuple.second compartments)


itemsPriority : Set Char -> Int
itemsPriority items =
    List.map itemPriority (Set.toList items)
        |> List.sum


itemPriority : Char -> Int
itemPriority item =
    if item >= 'a' && item <= 'z' then
        Char.toCode item - 96

    else if item >= 'A' && item <= 'Z' then
        Char.toCode item - (64 - 26)

    else
        0



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    """vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""



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
        , textarea [ class "input", cols 40, rows 20, onInput InputChange ] [ text model ]
        ]
