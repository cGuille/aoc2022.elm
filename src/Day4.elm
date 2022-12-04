module Day4 exposing (..)

import Basics.Extra exposing (uncurry)
import Browser
import Html exposing (Html, div, text, textarea)
import Html.Attributes exposing (class, cols, rows)
import Html.Events exposing (onInput)



-- SOLUTION


part1 : String -> Int
part1 input =
    String.lines input
        |> List.filterMap parseLine
        |> List.filter (uncurry eitherFullyContains)
        |> List.length


parseLine : String -> Maybe ( Range, Range )
parseLine line =
    case String.split "," line of
        [ str1, str2 ] ->
            case ( rangeFromString str1, rangeFromString str2 ) of
                ( Just r1, Just r2 ) ->
                    Just ( r1, r2 )

                _ ->
                    Nothing

        _ ->
            Nothing


eitherFullyContains : Range -> Range -> Bool
eitherFullyContains r1 r2 =
    fullyContains r1 r2 || fullyContains r2 r1


fullyContains : Range -> Range -> Bool
fullyContains r1 r2 =
    let
        s1 =
            r1.start

        e1 =
            r1.end

        s2 =
            r2.start

        e2 =
            r2.end
    in
    (s1 <= s2 && s2 <= e1) && (s1 <= e2 && e2 <= e1)


type alias Range =
    { start : Int, end : Int }


rangeFromString : String -> Maybe Range
rangeFromString str =
    case String.split "-" str of
        [ a, b ] ->
            case ( String.toInt a, String.toInt b ) of
                ( Just i1, Just i2 ) ->
                    Just (Range i1 i2)

                _ ->
                    Nothing

        _ ->
            Nothing



-- MAIN


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    String


init : Model
init =
    """2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"""



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
        , textarea [ class "input", cols 20, rows 20, onInput InputChange ] [ text model ]
        ]
