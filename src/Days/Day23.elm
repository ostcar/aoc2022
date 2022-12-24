module Days.Day23 exposing (printElves, puzzleInput, smallInput, solution, testInput, testSolution)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Test


solution : String -> ( String, String )
solution input =
    case Parser.run elvesPositionParser input of
        Ok elvesPosition ->
            ( run1 elvesPosition
            , run2 elvesPosition
            )

        Err _ ->
            ( "invalid", "input" )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution puzzleInput |> Expect.equal ( "110", "20" )


run1 : ElvesPosition -> String
run1 elves =
    nTimes
        (\round acc ->
            nextPosition acc round
                |> moveToNext
        )
        10
        elves
        |> coundFree
        |> String.fromInt


run2 : ElvesPosition -> String
run2 elves =
    untilAllAreOk 0 elves
        |> String.fromInt


untilAllAreOk : Int -> ElvesPosition -> Int
untilAllAreOk deep elves =
    let
        next =
            nextPosition elves deep
    in
    if allFree next then
        deep + 1

    else
        moveToNext next
            |> untilAllAreOk (deep + 1)


printElves : ElvesPosition -> String
printElves elves =
    let
        ( ( mx, mX ), ( my, mY ) ) =
            minMax elves
    in
    List.range my mY
        |> List.foldl
            (\y accX ->
                List.range mx mX
                    |> List.foldl
                        (\x accY ->
                            if Set.member ( x, y ) elves then
                                accY ++ "#"

                            else
                                accY ++ "."
                        )
                        ""
                    |> (\s -> accX ++ s ++ "\n")
            )
            ""


coundFree : ElvesPosition -> Int
coundFree elves =
    let
        ( ( mx, mX ), ( my, mY ) ) =
            minMax elves
    in
    List.range my mY
        |> List.foldl
            (\y accX ->
                List.range mx mX
                    |> List.foldl
                        (\x accY ->
                            if Set.member ( x, y ) elves then
                                accY

                            else
                                accY + 1
                        )
                        accX
            )
            0


nTimes : (Int -> a -> a) -> Int -> a -> a
nTimes fn count acc =
    nTimesHelper fn count 0 acc


nTimesHelper : (Int -> a -> a) -> Int -> Int -> a -> a
nTimesHelper fn count deep acc =
    if count == deep then
        acc

    else
        nTimesHelper fn count (deep + 1) (fn deep acc)


minMax : ElvesPosition -> ( Position, Position )
minMax elves =
    Set.foldl
        (\( x, y ) ( ( minX, maxX ), ( minY, maxY ) ) ->
            ( ( min x minX
              , max x maxX
              )
            , ( min y minY
              , max y maxY
              )
            )
        )
        ( ( 100, 0 )
        , ( 100, 0 )
        )
        elves


nextPosition : ElvesPosition -> Int -> Dict Position (List Position)
nextPosition elves roundIdx =
    Set.foldl
        (\pos acc ->
            let
                next =
                    if isAllFree elves pos then
                        pos

                    else
                        case freeDirection elves roundIdx pos of
                            Just direction ->
                                toDirection direction pos

                            Nothing ->
                                pos
            in
            case Dict.get next acc of
                Just list ->
                    Dict.insert next (pos :: list) acc

                Nothing ->
                    Dict.insert next [ pos ] acc
        )
        Dict.empty
        elves


allFree : Dict Position (List Position) -> Bool
allFree dict =
    Dict.toList dict
        |> List.all
            (\( k, v ) ->
                [ k ] == v
            )


moveToNext : Dict Position (List Position) -> ElvesPosition
moveToNext dict =
    Dict.foldl
        (\nextPos currentElves acc ->
            if List.length currentElves == 1 then
                Set.insert nextPos acc

            else
                Set.union acc (Set.fromList currentElves)
        )
        Set.empty
        dict


isAllFree : ElvesPosition -> Position -> Bool
isAllFree elves ( x, y ) =
    [ ( x - 1, y - 1 )
    , ( x, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    , ( x - 1, y + 1 )
    , ( x, y + 1 )
    , ( x + 1, y + 1 )
    ]
        |> Set.fromList
        |> Set.intersect elves
        |> (\set -> set == Set.empty)


type Direction
    = North
    | South
    | West
    | East


toDirection : Direction -> Position -> Position
toDirection direction ( x, y ) =
    case direction of
        North ->
            ( x, y - 1 )

        South ->
            ( x, y + 1 )

        West ->
            ( x - 1, y )

        East ->
            ( x + 1, y )


allDirections : Array Direction
allDirections =
    Array.fromList [ North, South, West, East ]


directionAtIdx : Int -> Direction
directionAtIdx index =
    Array.get (modBy 4 index) allDirections
        |> Maybe.withDefault North


freeDirection : ElvesPosition -> Int -> Position -> Maybe Direction
freeDirection elves round pos =
    List.range 0 3
        |> List.filterMap
            (\idx ->
                let
                    direction =
                        directionAtIdx (round + idx)
                in
                if isFreeAtDirection elves direction pos then
                    Just direction

                else
                    Nothing
            )
        |> List.head


isFreeAtDirection : ElvesPosition -> Direction -> Position -> Bool
isFreeAtDirection elves direction ( x, y ) =
    let
        directionPositions =
            case direction of
                North ->
                    [ ( x - 1, y - 1 ), ( x, y - 1 ), ( x + 1, y - 1 ) ]

                South ->
                    [ ( x - 1, y + 1 ), ( x, y + 1 ), ( x + 1, y + 1 ) ]

                West ->
                    [ ( x - 1, y - 1 ), ( x - 1, y ), ( x - 1, y + 1 ) ]

                East ->
                    [ ( x + 1, y - 1 ), ( x + 1, y ), ( x + 1, y + 1 ) ]
    in
    directionPositions
        |> Set.fromList
        |> Set.intersect elves
        |> (\set -> set == Set.empty)


type alias ElvesPosition =
    Set Position


elvesPositionParser : Parser ElvesPosition
elvesPositionParser =
    Parser.loop Set.empty
        (\set ->
            Parser.oneOf
                [ Parser.oneOf [ Parser.symbol ".", Parser.symbol "\n" ]
                    |> Parser.map (\() -> Parser.Loop set)
                , Parser.succeed (\( line, column ) -> Parser.Loop (Set.insert ( column - 1, line - 1 ) set))
                    |= Parser.getPosition
                    |. Parser.symbol "#"
                , Parser.end |> Parser.map (\() -> Parser.Done set)
                ]
        )


type alias Position =
    ( Int, Int )


testInput : String
testInput =
    """....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..
"""


smallInput : String
smallInput =
    """.....
..##.
..#..
.....
..##.
.....
"""


puzzleInput : String
puzzleInput =
    """#.##......#...#.###......###.........####.##...#.###...####.#....#.#.#.#
..#.###..##.##.#.#.....#.###..###.#..........#..#...##..####.#.#..#..##.
#..#.###...##.....##.#.#####.#..##.....##...#.#.#....##.#..###.###....#.
#.###...###.#.##.#..#..#.##..##...##.#..##..#.##.##....#######...#..#...
....#.#.###...#.#..###.#.#..##....#####.#...##..#.#...##.##.....###..#.#
..###.#.#..#####..##.##....###......##.##..####.#.###....######.#.#.##..
#...###.##.##..###.####.####.#...###..##...#.#..#####.#...##...#..##..##
##.####....###.#####..#.#.###.##.#...#..##.##.###..#...##.#.#..###...#..
####...##..#..####...##.##..##.##..#.##.......#..#.##.#...###..######.##
.#.##.#.###.##.##........####.#...#.##...##..##.....#..##.#.###...######
#.......#...#####.#...#.###...####..####....##..###.##..##..######....#.
.##.###..#....#.#.#####.###..#.##......#...##..##...#..#.##.#...##..#..#
##.###......####.#..#####.....#..##.####.#.#....##...##.#.#.##...#.#.#..
.#.##..###.#.......#..##.##.#.#.........#.##.##.......##....##..#..#....
###..####.##..##..#....#..#..#.#.##..#........#...##.##.#.##...#.#...#.#
..#.#####.#..###.#.##..#..#...#.#...#...#..##.#.#..##..#.#...####..#...#
#...#####..#.####.#.##.##.#...##.#...#.....##.....#..#.###..#.#...#..#..
.##########...#..#..#......#.###.#......#..#.#..#..#.#..#..#.#.###.#...#
.##..#.##.###.#...##.#...#.##.#.##..##...#.#..#...#.....#.######..#.#.##
.#.#.....###......#.#.##.#..#...#....##.#..#.###...#.....###.##.####..##
..#..#.#...#.###..#..####....##.#........##..###..####....###..###.###..
#..##.###.#.#..#..#.##....#...#.#.###..#.#.....###..##..#####...##.#..#.
#.#......##.#.#.#.###.#.#...###..######.###.##.#.#....#####..#.#..###.##
........##.##.##..##.#.###.#..#.########.....#.#.##.##.#......########.#
#..#.#.#...#..#.#....###.....###.###.#.##....##...#...#..#.#..##.###.###
.#...####.#.#....#.####..#...##.#.##...#.#.####.#...#..##.###...#.###..#
......#...#..##.##.###.#..#.....######..##.####.#......#..##.......####.
...###.#..#.....#..#.####...#..#...#...##..#...##.##....####......###..#
#.###...#.##.######.#...#...##..#.#..##.#.#....#####.####.#.#......#..##
##.....#..####.#.#.###...#..#..#..#..#.#.#....###..##..#.#.#....#..#.##.
.#.#.....####.#.#...........#.#.##.####...#.#...#..#.#..####.#..#.#.##.#
...#..#...#.#.###..#.#..##...##.#.#..#.#####.#.##.###.#......#..##.#.###
...#....##########.######...#.....#..#.##...###..#.####.#.####..##..##..
#.##....#..#...####.......##.#..##.#.#..#######.....#.#.#.##..##..#.#...
###..##..#..#.#..###..#.#.#..###....##..#.#.....##..#..#....#....###.#.#
##.#.#..#.#######.......#..#.#.#...#..#.#..######..##..........##....#..
......##..#######.#..#...#.#####.#.#.#..#.####.####..##.##.#.##....#....
...#.###...######.#....#.#...###.##.#.#.###..#.#...####.#....#...#.#..#.
#.#...#.###..#####.###..##.#....#...#.##...#..#...####.#...#.##.#..#.#.#
##.#..##....##..#..######....##.##.....##..##....###....#####.#.##....##
#..#.#..#..###.##..#....#.##.#####.###.#.##.#..#.#..#####..###..###.#.##
.#.##...#.#..#....#.#.##.#.......#.....###.###.#.##.##...###.####......#
#.#####.#.##.#.#.##.#..#####......#...#...##.###.#..#.#.#..#.##.....###.
#....##.##....##.....#..#.#.#.#.#...###.#####....#..#.##.##.#.#.#.#.####
#....##.##....#...####....##..#.#.#.#.##.##.#..##....##....##..#...###..
##.#..##.#...#.##.#.#.##.#.#.#...#...#..##.###....#.##...###.#.#...####.
.############.#.#..#.###.###.######...#####.####.....#.#.###..##.#######
##...#.###..####..##...#...###...##..#.###...###.#.#..#.##..#######.###.
#..#.###..#..##..#..#.##.###..###....##..#.#..#....#.#.##..#.###.....#.#
#.#...#..#.##...###..##.##..###........#...##..#....###..##..#.#.###...#
##....#......##.####..##...###...##..#...#..#.#.##.#...####.#.#.####..#.
..#.#.###..##.##.#..####.###..####.....##.#...#.#.#...#.########.#.#....
#..#.##..#..##......##.##...#..#.#..#####..##.#.##..##.###.#.#..######.#
#..#..###.#..###.##..##....#..##..##...#.#...#..##..#####.###..###.####.
...#..##..##.#...#.....##..........#.##.#..#####..##..###..#...##.##..##
.#.#..##......#...#..#....#.######..#.#..##..#.......#.###.#.###..##....
..#.###.#.#.#.###.###..######..####...#..#.##.###..#....##..#.#.#.#..###
###.##.##.####.....####.##.##..##....###.#....###..###.##########.####..
#.#...#.###....##.#.#.##.#.#.#....#####...#.##..###.#.......#.##.#.###.#
.....##....#..#.....####.#.##..#.#..#....#.....#.######.##..#..###.#..#.
.........#...###..#....#...##.............###..###...#..##..####.###.##.
..#.#..#####.#.#..#.#...######.#.##..#...##.####...#...#.##..#.#.##.#...
#...#.#...##.#..##..#.####.#.#.######...###...#..#...#.##.#.#...#.###.##
.#####..#.#.##.#####...######.#..#.#.#.###...#...#.#.#.#.#..#..##.#...##
###.##.#.#.##...##..###....##.#.#..#..#...#.###.#.#.....#..##...#....#.#
##.#.##..#...###.#..#..###...#.##.#..#..####.#.#.##.#....#.#.#....#..###
.#.#.#...##.#.######.#.##.#.......#.#......#..#..#.#.####...###..#....#.
.#....####...#....#..#.##.######...####..#...#.####.####..#.##.#.#.###..
###.......#.###.##...##..#....#####..##.#...#..###..#......#...#.#.##..#
.#...##.#..##..#..###.##..#....#..#....##..#.##...###.##.#.####.##.#.###
..##.#.#.##.#.###.##.##...#.#....#.#..###..##....#.##...#...#.###.##.##.
#..#..##.##..#..#####..#.#.#.#.##....#####.#.#.....###.##....#.#...##..#
"""
