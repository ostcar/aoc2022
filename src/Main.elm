module Main exposing (main)

import Array exposing (Array)
import Browser
import Days.Day1
import Days.Day2
import Days.Day3
import Days.Day4
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program () Model Msg
main =
    Browser.sandbox
        { init = ShowList
        , update = update
        , view = view
        }


type Model
    = ShowList
    | ShowDay Day


type alias Day =
    ( () -> String, () -> String )


type Msg
    = ClickedDay Int
    | ClickedBack


days : Array ( String, Day )
days =
    Array.fromList
        [ ( "Day 1", Days.Day1.solution )
        , ( "Day 2", Days.Day2.solution )
        , ( "Day 3", Days.Day3.solution )
        , ( "Day 4", Days.Day4.solution )
        ]


dayFromIndex : Int -> Day
dayFromIndex index =
    Array.get index days
        |> Maybe.andThen (Tuple.second >> Just)
        |> Maybe.withDefault ( \() -> "Unknown", \() -> "day" )


update : Msg -> Model -> Model
update msg _ =
    case msg of
        ClickedDay index ->
            dayFromIndex index |> ShowDay

        ClickedBack ->
            ShowList


view : Model -> Html Msg
view model =
    div []
        [ viewIntroduction
        , viewBody model
        , viewFooter model
        ]


viewIntroduction : Html msg
viewIntroduction =
    div []
        [ h1 []
            [ text "Welcome to my solution of "
            , a [ href "https://adventofcode.com/2022" ] [ text "Advent of code 2022" ]
            ]
        ]


viewBody : Model -> Html Msg
viewBody model =
    case model of
        ShowList ->
            viewShowList

        ShowDay ( soltuon1, solution2 ) ->
            div []
                [ p [] [ text <| soltuon1 () ]
                , p [] [ text <| solution2 () ]
                ]


viewShowList : Html Msg
viewShowList =
    ul []
        (Array.toIndexedList days
            |> List.map
                (\( index, ( name, _ ) ) ->
                    li []
                        [ a [ href "#", onClick (ClickedDay index) ] [ text name ] ]
                )
        )


viewFooter : Model -> Html Msg
viewFooter model =
    case model of
        ShowList ->
            text ""

        ShowDay _ ->
            div [] [ a [ href "#", onClick ClickedBack ] [ text "back" ] ]
