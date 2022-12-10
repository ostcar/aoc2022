module Main exposing (main)

import Array exposing (Array)
import Browser
import Days.Day01
import Days.Day02
import Days.Day03
import Days.Day04
import Days.Day05
import Days.Day06
import Days.Day07
import Days.Day08
import Days.Day09
import Days.Day10
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
    | ShowRope String Int Int


type alias Day =
    ( String, String -> ( String, String ) )


type Msg
    = ClickedDay Int
    | ClickedBack
    | InputedRopeInput String
    | InputedRopeLength String
    | ClickedRopeAddFrame
    | ClickedRopeRemoveFrame


days : Array ( String, Day )
days =
    Array.fromList
        [ ( "Day 01", ( Days.Day01.puzzleInput, Days.Day01.solution ) )
        , ( "Day 02", ( Days.Day02.puzzleInput, Days.Day02.solution ) )
        , ( "Day 03", ( Days.Day03.puzzleInput, Days.Day03.solution ) )
        , ( "Day 04", ( Days.Day04.puzzleInput, Days.Day04.solution ) )
        , ( "Day 05", ( Days.Day05.puzzleInput, Days.Day05.solution ) )
        , ( "Day 06", ( Days.Day06.puzzleInput, Days.Day06.solution ) )
        , ( "Day 07", ( Days.Day07.puzzleInput, Days.Day07.solution ) )
        , ( "Day 08", ( Days.Day08.puzzleInput, Days.Day08.solution ) )
        , ( "Day 09", ( Days.Day09.puzzleInput, Days.Day09.solution ) )
        , ( "Day 10", ( Days.Day10.puzzleInput, Days.Day10.solution ) )
        ]


dayFromIndex : Int -> Day
dayFromIndex index =
    Array.get index days
        |> Maybe.andThen (Tuple.second >> Just)
        |> Maybe.withDefault ( "Unknown", \_ -> ( "Unknown", "Day" ) )


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedDay index ->
            dayFromIndex index |> ShowDay

        ClickedBack ->
            ShowList

        InputedRopeInput input ->
            case model of
                ShowRope _ length frame ->
                    ShowRope input length frame

                _ ->
                    ShowRope input 2 0

        InputedRopeLength length ->
            let
                maybeIntLength =
                    String.toInt length
            in
            case maybeIntLength of
                Nothing ->
                    model

                Just intLength ->
                    case model of
                        ShowRope input _ frame ->
                            ShowRope input intLength frame

                        _ ->
                            ShowRope "" intLength 0

        ClickedRopeAddFrame ->
            case model of
                ShowRope input length oldFrame ->
                    ShowRope input length (oldFrame + 1)

                _ ->
                    ShowRope "" 2 1

        ClickedRopeRemoveFrame ->
            case model of
                ShowRope input length oldFrame ->
                    Basics.max (oldFrame - 1) 0 |> ShowRope input length

                _ ->
                    ShowRope "" 2 1


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

        ShowDay ( input, solver ) ->
            let
                ( s1, s2 ) =
                    solver input
            in
            div []
                [ p [] [ multilineText s1 ]
                , p [] [ multilineText s2 ]
                ]

        ShowRope input length frame ->
            viewRope input length frame


viewRope : String -> Int -> Int -> Html Msg
viewRope input length frame =
    div []
        [ p [] [ Html.textarea [ onInput InputedRopeInput, placeholder "input" ] [] ]

        --, p [] [ text <| (Days.Day9.frameCount input |> String.fromInt) ++ " Frames" ]
        , p [] [ Html.input [ onInput InputedRopeLength, placeholder "rope length" ] [] ]
        , p []
            [ button [ onClick ClickedRopeAddFrame ] [ text "+" ]
            , button [ onClick ClickedRopeRemoveFrame ] [ text "-" ]
            , text <| String.fromInt frame
            ]
        , p [] [ multilineText <| Days.Day09.run input length frame ]
        ]


multilineText : String -> Html msg
multilineText input =
    pre []
        [ text input ]


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
    div []
        [ case model of
            ShowList ->
                text ""

            _ ->
                div [] [ a [ href "#", onClick ClickedBack ] [ text "back" ] ]
        , div [] [ a [ href "#", onClick (InputedRopeInput "") ] [ text "robe" ] ]
        ]
