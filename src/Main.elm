module Main exposing (main)

import Browser
import Days.Day1
import Days.Day2
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


defaultDay : Day
defaultDay =
    Days.Day1.solution



-- TODO Maybe use Array?


days : List ( String, Day )
days =
    [ ( "Day 1", Days.Day1.solution )
    , ( "Day 2", Days.Day2.solution )
    ]


dayFromIndex : Int -> Day
dayFromIndex index =
    case List.drop index days of
        ( _, day ) :: _ ->
            day

        [] ->
            defaultDay


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
                [ p [] [ text <|soltuon1 () ]
                , p [] [ text <| solution2 () ]
                ]


viewShowList : Html Msg
viewShowList =
    let
        lis =
            List.indexedMap
                (\index ( name, _ ) ->
                    li []
                        [ a [ href "#", onClick (ClickedDay index) ] [ text name ]
                        ]
                )
                days
    in
    ul [] lis


viewFooter : Model -> Html Msg
viewFooter model =
    case model of
        ShowList ->
            text ""

        ShowDay _ ->
            div [] [ a [ href "#", onClick ClickedBack ] [ text "back" ] ]
