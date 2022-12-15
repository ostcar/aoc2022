module Days.Day15 exposing (parseInput, puzzleInput, solution, testSolution)

import Expect
import Parser exposing ((|.), (|=), Parser)
import Test


solution : String -> ( String, String )
solution input =
    ( run1 input 2000000
    , run2 input 4000000
    )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> ( run1 testInput 10, run2 testInput 20 ) |> Expect.equal ( "26", "56000011" )


run1 : String -> Int -> String
run1 input testLine =
    parseInput input
        |> List.filterMap (rangeAtLine testLine)
        |> sortRanges
        |> mergeRanges
        |> sumRanges
        |> String.fromInt


type alias Point =
    ( Int, Int )


type alias Range =
    ( Int, Int )


parseInput : String -> List ( Point, Point )
parseInput input =
    Parser.run (listParser lineParser) input |> Result.withDefault []


pointParser : Parser Point
pointParser =
    Parser.succeed Tuple.pair
        |. Parser.symbol "x="
        |= posNegInt
        |. Parser.symbol ","
        |. Parser.spaces
        |. Parser.symbol "y="
        |= posNegInt


posNegInt : Parser Int
posNegInt =
    Parser.oneOf
        [ Parser.succeed negate
            |. Parser.symbol "-"
            |= Parser.int
        , Parser.int
        ]


lineParser : Parser ( Point, Point )
lineParser =
    Parser.succeed Tuple.pair
        |. Parser.chompUntil "x="
        |= pointParser
        |. Parser.chompUntil "x="
        |= pointParser
        |. Parser.chompUntilEndOr "\n"


listParser : Parser a -> Parser (List a)
listParser singleParser =
    -- TODO: This has a bug. If the singleParser fails, then this does not fail,
    -- but returns an empty list.
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ Parser.succeed (\single -> Parser.Loop (single :: list))
                    |= singleParser
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse list))
                ]
        )


rangeAtLine : Int -> ( Point, Point ) -> Maybe Range
rangeAtLine line ( ( x, y ) as point1, point2 ) =
    let
        distance =
            toDistance point1 point2

        distanceToLine =
            abs (y - line)

        oneSide =
            distance - distanceToLine
    in
    if oneSide < 0 then
        Nothing

    else
        Just ( x - oneSide, x + oneSide )


toDistance : Point -> Point -> Int
toDistance ( x1, y1 ) ( x2, y2 ) =
    abs (x1 - x2) + abs (y1 - y2)


sortRanges : List Range -> List Range
sortRanges list =
    List.sortWith
        (\( from1, to1 ) ( from2, to2 ) ->
            case compare from1 from2 of
                EQ ->
                    compare to1 to2

                a ->
                    a
        )
        list


mergeRanges : List Range -> List Range
mergeRanges list =
    case list of
        first :: second :: rest ->
            case mergeTwo first second of
                Just together ->
                    mergeRanges (together :: rest)

                Nothing ->
                    first :: mergeRanges (second :: rest)

        _ ->
            list


mergeTwo : Range -> Range -> Maybe Range
mergeTwo ( from1, to1 ) ( from2, to2 ) =
    if to1 < from2 then
        Nothing

    else
        Just ( from1, max to1 to2 )


sumRanges : List Range -> Int
sumRanges list =
    List.foldl (\( from, to ) acc -> acc + to - from) 0 list


run2 : String -> Int -> String
run2 input maxSize =
    parseInput input
        |> run2Helper 0 maxSize
        |> String.fromInt


run2Helper : Int -> Int -> List ( Point, Point ) -> Int
run2Helper current maxSize list =
    if current >= maxSize then
        -1

    else
        let
            ranges =
                List.filterMap (rangeAtLine current) list
                    |> sortRanges
                    |> mergeRanges
        in
        case ranges of
            [ ( _, to ), _ ] ->
                4000000 * (to + 1) + current

            _ ->
                run2Helper (current + 1) maxSize list


testInput : String
testInput =
    """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3"""


puzzleInput : String
puzzleInput =
    """Sensor at x=2662540, y=1992627: closest beacon is at x=1562171, y=2000000
Sensor at x=3577947, y=3994226: closest beacon is at x=3468220, y=3832344
Sensor at x=34015, y=3658022: closest beacon is at x=-48386, y=3887238
Sensor at x=3951270, y=2868430: closest beacon is at x=3499312, y=2620002
Sensor at x=3136779, y=3094333: closest beacon is at x=2731027, y=3076619
Sensor at x=3415109, y=2591103: closest beacon is at x=3499312, y=2620002
Sensor at x=277465, y=3971183: closest beacon is at x=-48386, y=3887238
Sensor at x=3697201, y=1834735: closest beacon is at x=3499312, y=2620002
Sensor at x=874397, y=1535447: closest beacon is at x=1562171, y=2000000
Sensor at x=2996230, y=3508199: closest beacon is at x=3251079, y=3709457
Sensor at x=2754388, y=3147571: closest beacon is at x=2731027, y=3076619
Sensor at x=524580, y=2640616: closest beacon is at x=-73189, y=1870650
Sensor at x=2718599, y=3106610: closest beacon is at x=2731027, y=3076619
Sensor at x=2708759, y=3688992: closest beacon is at x=3251079, y=3709457
Sensor at x=2413450, y=3994713: closest beacon is at x=3251079, y=3709457
Sensor at x=1881113, y=495129: closest beacon is at x=1562171, y=2000000
Sensor at x=3792459, y=3827590: closest beacon is at x=3468220, y=3832344
Sensor at x=3658528, y=641189: closest beacon is at x=4097969, y=-110334
Sensor at x=1379548, y=3381581: closest beacon is at x=1562171, y=2000000
Sensor at x=3480959, y=3069234: closest beacon is at x=3499312, y=2620002
Sensor at x=3871880, y=3531918: closest beacon is at x=3468220, y=3832344
Sensor at x=2825206, y=2606984: closest beacon is at x=2731027, y=3076619
Sensor at x=3645217, y=2312011: closest beacon is at x=3499312, y=2620002
Sensor at x=3485320, y=3509352: closest beacon is at x=3468220, y=3832344
Sensor at x=56145, y=3879324: closest beacon is at x=-48386, y=3887238
Sensor at x=148776, y=433043: closest beacon is at x=-73189, y=1870650
Sensor at x=3368682, y=3929248: closest beacon is at x=3468220, y=3832344
Sensor at x=3330787, y=2481990: closest beacon is at x=3499312, y=2620002
Sensor at x=2802875, y=3209067: closest beacon is at x=2731027, y=3076619
Sensor at x=2679788, y=3102108: closest beacon is at x=2731027, y=3076619
Sensor at x=3326846, y=3767097: closest beacon is at x=3251079, y=3709457
Sensor at x=3111518, y=1310720: closest beacon is at x=3499312, y=2620002"""
