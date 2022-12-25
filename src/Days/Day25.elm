module Days.Day25 exposing (puzzleInput, solution, testSolution, testallInputNumbersToSelf)

import Days.Day20 exposing (puzzleInput)
import Expect
import Test


solution : String -> ( String, String )
solution input =
    case String.trim input |> String.lines |> List.map snafuToInt |> allJust of
        Just ints ->
            ( ints |> List.sum |> intToSnafu
            , "TODO"
            )

        Nothing ->
            ( "invalid", "input" )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "2=-1=0", "TODO" )


testallInputNumbersToSelf : Test.Test
testallInputNumbersToSelf =
    Test.describe "test to its self" <|
        List.map
            (\snafu ->
                Test.test
                    snafu
                    (\() -> snafuToInt snafu |> Maybe.map (\n -> intToSnafu n) |> Expect.equal (Just snafu))
            )
            (puzzleInput |> String.trim |> String.lines)


allJust : List (Maybe a) -> Maybe (List a)
allJust list =
    case list of
        (Just v) :: rest ->
            allJust rest
                |> Maybe.map (\r -> v :: r)

        Nothing :: _ ->
            Nothing

        [] ->
            Just []


snafuToInt : String -> Maybe Int
snafuToInt snafu =
    String.toList snafu
        |> List.map snafuCharToInt
        |> allJust
        |> Maybe.map fiverPlaces


intToSnafu : Int -> String
intToSnafu number =
    case number of
        0 ->
            ""

        _ ->
            intToSnafu (saveIDiv (number + 2) 5) ++ (number + 2 |> modBy 5 |> (+) -2 |> oneDigitToString)


saveIDiv : Int -> Int -> Int
saveIDiv n d =
    floor <| (toFloat n / toFloat d)


oneDigitToString : Int -> String
oneDigitToString n =
    case n + 2 of
        0 ->
            "="

        1 ->
            "-"

        2 ->
            "0"

        3 ->
            "1"

        4 ->
            "2"

        _ ->
            "ERROR"


fiverPlaces : List Int -> Int
fiverPlaces list =
    List.reverse list
        |> fiverPlacesHelper 0


fiverPlacesHelper : Int -> List Int -> Int
fiverPlacesHelper deep list =
    case list of
        head :: rest ->
            5 ^ deep * head + fiverPlacesHelper (deep + 1) rest

        [] ->
            0


snafuCharToInt : Char -> Maybe Int
snafuCharToInt c =
    case c of
        '=' ->
            Just -2

        '-' ->
            Just -1

        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        _ ->
            Nothing


testInput : String
testInput =
    """
1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122
"""


puzzleInput : String
puzzleInput =
    """
1-=022001=-1-
2-1020=0-01=2000
1==-2=122-=2-21102
1-0=1=-0-==0=--=-2
1-0-1=--22=
2=110---012022-21
22==0
2-0-10=112
2210=1-=-1-011=2-
1-=-=0212
1=100011=21101=
2-===02
1--120
1=--1=-000-=00=210
2220=210-01
1=011--1
100212002=0=0
2-=2=0=-00022
200-
1200=
1=1221-01-01-22
1=1=22==-1
1-1=201-
1=10
2
1--2=2=201020-=
1-2-==211=-221=-
100-20=-002=
1-021
20021--=1
2--2=0
1=010=2-=200-01-1
2=1
202102
100-0-0=0-1-
2-
22-12=0010=-221=-
1=1
10--120
1210-020=-1-210=1
102-=1
2=0012=1=10=22-
2=0
1=222
2=-0000=-=11-
1-11-21=02=10
11201---=1=202110
10-1=21202=1=10=0
220102=2110-1
21110101
2--0-0--01-01---2
1=10=1-1-1=0121-=2==
1---=0=10021011
111=1=21
110220==20=2
20--12==20
10-11=1-01
2=-==11=
2==2-
1-0
220=-=-102==
10-0-20
21201=-211001=0-11-
2=020-=20
12=-1---2-=2-02
10=-=20-121
12-
10-02--0=-==
11-
212-2--1-1-=122
1=2-
2020=-1-1002-==-01
2==11201==10-2
200-=-0-0-
1--=====12
12-=02-2-1
21-2=21==10-2=201-
102
111=00
2=-2-0--222=
11--=
2=0-102=2=
1=2-22-=20121-1-12
1-==1-0=--
100
1=0=220002
11-0-2
12
1-020120020
2-==
1020==-02
220=-1
2=-0==-2=021-000
112==
11=1-01=0-2=-10=2==
1=1001
2-=
10-1==1=-2111-=2
1=-0==
2112===0-202=1
102=-
1=12=
1=200=01=22-0
1-2-=2
211220=-01===-1==2
1101-12=--=2020
110001-001-==2=
"""
