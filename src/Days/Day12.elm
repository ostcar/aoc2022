module Days.Day12 exposing (puzzleInput, solution, testSolution)

import Char
import Dict exposing (Dict)
import Expect
import Set exposing (Set)
import Test


solution : String -> ( String, String )
solution input =
    ( run Part1 input
    , run Part2 input
    )


type Part
    = Part1
    | Part2


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "31", "29" )


type alias Grid =
    Dict Position Location


type alias Position =
    ( Int, Int )


type Location
    = Hight Char
    | Start
    | Stop


run : Part -> String -> String
run part input =
    let
        grid =
            parseInput input

        startPositions =
            findStart part grid
    in
    if Set.size startPositions == 0 then
        "Could not find a place to start"

    else
        breathFirstSearch grid (findStop grid) startPositions 0
            |> String.fromInt


locationToInt : Location -> Int
locationToInt location =
    case location of
        Start ->
            locationToInt <| Hight 'a'

        Stop ->
            locationToInt <| Hight 'z'

        Hight a ->
            Char.toCode a - Char.toCode 'a'


parseInput : String -> Grid
parseInput input =
    input
        |> String.trim
        |> String.lines
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( y, line ) gridOut ->
                line
                    |> String.toList
                    |> List.indexedMap Tuple.pair
                    |> List.foldl
                        (\( x, c ) gridIn ->
                            Dict.insert ( x, y ) (charToLocation c) gridIn
                        )
                        gridOut
            )
            Dict.empty


charToLocation : Char -> Location
charToLocation c =
    case c of
        'S' ->
            Start

        'E' ->
            Stop

        _ ->
            Hight c


findStart : Part -> Grid -> Set Position
findStart part grid =
    grid
        |> Dict.toList
        |> List.filter
            (\( _, loc ) ->
                case part of
                    Part1 ->
                        case loc of
                            Start ->
                                True

                            _ ->
                                False

                    Part2 ->
                        locationToInt loc == 0
            )
        |> List.map Tuple.first
        |> Set.fromList


findStop : Grid -> Position
findStop grid =
    grid
        |> Dict.toList
        |> List.filter
            (\( _, loc ) ->
                case loc of
                    Stop ->
                        True

                    _ ->
                        False
            )
        |> List.head
        |> Maybe.andThen (Tuple.first >> Just)
        |> Maybe.withDefault ( 0, 0 )


breathFirstSearch : Grid -> Position -> Set Position -> Int -> Int
breathFirstSearch grid stop positions depth =
    -- Thank you @normanjaeckel for the hint to use a breath first search.
    if Set.member stop positions then
        depth

    else
        let
            gridWithoutPositions =
                dictRemoveMany (Set.toList positions) grid

            nextPositions : Set Position
            nextPositions =
                Set.foldl
                    (\p before ->
                        Set.union (possiblePositions grid p) before
                    )
                    Set.empty
                    positions
        in
        breathFirstSearch gridWithoutPositions stop nextPositions (depth + 1)


possiblePositions : Grid -> Position -> Set Position
possiblePositions grid (( x, y ) as myPos) =
    case Dict.get myPos grid of
        Nothing ->
            Set.empty

        Just myLocation ->
            let
                myHight =
                    locationToInt myLocation

                positionsAroundMe =
                    [ ( x - 1, y )
                    , ( x + 1, y )
                    , ( x, y - 1 )
                    , ( x, y + 1 )
                    ]
            in
            positionsAroundMe
                |> List.filterMap
                    (\possiblePosition ->
                        Dict.get possiblePosition grid
                            |> Maybe.andThen
                                (\possibleLocation ->
                                    let
                                        possibleHight =
                                            locationToInt possibleLocation
                                    in
                                    if (myHight + 1) >= possibleHight then
                                        Just possiblePosition

                                    else
                                        Nothing
                                )
                    )
                |> Set.fromList


dictRemoveMany : List comparable -> Dict comparable a -> Dict comparable a
dictRemoveMany indexes dict =
    List.foldl
        Dict.remove
        dict
        indexes


testInput : String
testInput =
    """
Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi
"""


puzzleInput : String
puzzleInput =
    """
abcccaaaaaaccccccccaaaaaccccccaaaaaaccccccaaaaaaaacccaaaaaaaccaaaacccccccccccccccccccccccccaaaaaacccccccccccccccccccccccccccccaaaaaa
abcccaaaaaacccccccaaaaaaccccaaaaaaaacccccccaaaaaaaaaaaaaaaaccaaaaacccccccccccccccccccccccccaaaaaacccccccccccccccccccccccccccccaaaaaa
abccccaaaaacaaaccaaaaaaaacccaaaaaaaaacccccccaaaaaaaaaaaaaaaacaaaaaacccccccccaaacccccccccccaaaaaaaaccccccccccaaccccccccccccccccaaaaaa
abccccaaaaccaaaaaaaaaaaaacccaaaaaaaaaacccccaaaaaaaaaaaaaaaaaaacaaaacccccccccaaaacccccccccaaaaaaaaaacccccccccaaaccccccccccccccccccaaa
abcccccccccaaaaaacccaacccccccccaaacaaaccccccaacccccccaaaaaaaaacaacccccccccccaaaacccccccccaaaaaaaaaacccccccccaaaccacaaccccccccccccaaa
abcccccccccaaaaaacccaacccccccccaaacccccccccccccccccccaaaacaaaacccccccaacaaccaaaccccccccccaccaaaaacacccccccccaaaacaaaaccccccccccccaac
abccccccccccaaaaacccccccccccccccacccaaaacccccccccccccaaaacccccccccccccaaaacccccccccccaacccccaaaaccccccccjjjjaaaaaaaaaccccccccccccccc
abccccccccccaaaacccccccccccccccccccaaaaacccccccccccccaaaccccccccccccccaaaaacccccccccaaaaaacccaaccccccccjjjjjjkkaaaacccccccccaacccccc
abcccccaaccccccccccccccccccccccccccaaaaaacccccccccccccaacccccccccccccaaaaaaccccccccccaaaaaccccccccccccjjjjjjjkkkkaacccccaacaaacccccc
abccaaaacccccccccccccccccccccccccccaaaaaaccccccccccccccccccccccccccccaaaacaccccccccaaaaaaaccccaacccccjjjjoooookkkkkkkklllaaaaaaacccc
abccaaaaaacccccccccccccccccccccccccaaaaacccccccccccccccccccccccccccccccaaccccccccccaaaaaaaaccaaaaccccjjjoooooookkkkkkkllllaaaaaacccc
abcccaaaaacccccccccccccccccccccccccccaaaccccccccaaaacccccccccccccccccccccccccccccccaaaaaaaaccaaaaccccjjooooooooppkkppplllllaccaacccc
abccaaaaaccccccccccccaccccccccccccccccccccccccccaaaacccccccccccccccccccccccccccccccccaaacacccaaaacccijjooouuuuoppppppppplllccccccccc
abcccccaacccccccccccaaaaaaaaccccccccccccccccccccaaaaccccaaccccccccaaacccccccccccccaacaaccccccccccccciijoouuuuuuppppppppplllcccaccccc
abcccccccccccccccccccaaaaaaccccccccccccccccccccccaaccccaaaacccccccaaaaccccccccccaaaaaaccccccccccccciiiiootuuuuuupuuuvvpppllccccccccc
abcccccccccccccccccccaaaaaaccaaaaacccccccccccccccccccccaaaacccccccaaaaccccccccccaaaaaaccccccccccccciiinnotuuxxxuuuuvvvpppllccccccccc
abccccccccccccccacccaaaaaaaacaaaaaaacccccccccccccccccccaaaacccccccaaacccccaaaaccaaaaaccccaaccccccciiiinnnttxxxxuuyyyvvqqqllccccccccc
abcccccccccccaaaaccaaaaaaaaaaaaaaaaaaccaacccccccccccccccccccccccccccccccccaaaacccaaaaaccaaacccccciiinnnnnttxxxxxyyyyvvqqqllccccccccc
abaaaacccccccaaaaaaaaaaaaaaaaaaaaaaaaaaaacccccccccccccccccccccccccccccccccaaaacccaaaaaacaaaccccciiinnnnttttxxxxxyyyyvvqqmmmccccccccc
abaaaaccccccccaaaaacccaaaaacaaaaaacaaaaaaccccccccccccccccaaccccccccccccccccaacccccccaaaaaaaaaaciiinnnnttttxxxxxyyyyvvqqqmmmccccccccc
SbaaaacccccccaaaaaccccaaaaaccaaaaaaaaaaaccccccccccccccccaaacaacccccccccccccccccccccccaaaaaaaaachhhnnntttxxxEzzzzyyvvvqqqmmmccccccccc
abaaaacccccccaacaacccccaaaaaaaacaaaaaaaaaccccccccccccccccaaaaaccccccccccccccccccccccccaaaaaaacchhhnnntttxxxxxyyyyyyvvvqqmmmdddcccccc
abaaaacccccccccccccccccccaaaaaacaaaaaaaaaacccccccccccccaaaaaaccccccccaaaccccccccccccccaaaaaaccchhhnnntttxxxxywyyyyyyvvvqqmmmdddccccc
abaacccccccccccccccccccaaaaaaacccccaaaaaaacccccccccccccaaaaaaaacccccaaaacccccccccccccaaaaaaacaahhhmmmttttxxwwyyyyyyyvvvqqmmmdddccccc
abcccccccccccccccccccccaaaaaaacaaccaaacccccccccccccccccaacaaaaacccccaaaacccccccccccccaaacaaaaaahhhmmmmtsssswwyywwwwvvvvqqqmmdddccccc
abcccccccccccccccaaaccccaaaaaaaaaacaaccaaccccccccccccccccaaacaccccccaaaacccccccccccccccccaaaaacahhhmmmmmsssswwywwwwwvvrrqqmmdddccccc
abcccccccccccccaaaaaaccccaaaaaaaaaccaaaacccccccccccccccccaacccccccccccccccccccccccaaaccccaaaaaaahhhhhmmmmssswwwwwrrrrrrrrmmmmddccccc
abcccccccccccccaaaaaaccccaaaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccaaaaaacccccaaaaachhhhhmmmmsswwwwrrrrrrrrrkkmdddccccc
abccccccccccccccaaaaaccccccaaaaaaaaaaaaaaaccccccccccccccccccccccccccccccccccccccaaaaaaccccaaaaacccchhggmmmssswwrrrrrkkkkkkkkdddacccc
abccaaaacccccccaaaaacccccccccaaaaaacaaaaacccccccccccccccccccccccccccccccccccccccaaaaaaccccaacaaaccccggggmmsssssrrlkkkkkkkkkdddaccccc
abccaaaacccccccaaaaacccccccccaaaaaaccccaacccccccccccccccccccccccccccccccccccccccaaaaaccccccccaaccccccgggmllssssrllkkkkkkkeeeddaccccc
abccaaaacccccccaaacccccccccccaaaaaacccccccccccccccccccaacccccccccccccccccccccccaaaaaacccccccccccccccccggllllssslllkkeeeeeeeeeaaacccc
abcccaaccccccccaaacaaaccccccaaaaaaaaaaacccccccccccccaaaaaacccccccccccccccccccccaaacaaacccccaacccccccccggglllllllllfeeeeeeeeaaaaacccc
abccccccccccaaaaaaaaaaccccccccccccaccaaaccacccccccccaaaaaaccccaaccaacccaaccccccaaaaaaacccccaaccccccccccggglllllllfffeeecccaaaaaacccc
abccccccccccaaaaaaaaacccccccccccccccaaaaaaaccccccccccaaaaaccccaaaaaacccaaaaaaccaaaaaacccaaaaaaaacccccccggggllllfffffccccccaacccccccc
abcccccccccccaaaaaaacccccccccccccccccaaaaaaccaacccccaaaaaccccccaaaaacccaaaaaacaaaaaaacccaaaaaaaaccccccccgggffffffffccccccccccccccccc
abccccccccccccaaaaaaacccccccccccccaaaaaaaaacaaaaccccaaaaacaaaaaaaaaacaaaaaaacaaaaaaaaaccccaaaacccccccccccggffffffacccccccccccccccaaa
abccccccccccccaaaaaaacaaccccccccccaaaaaaaaacaaaacccccaaaaaaaaaaaaaaaaaaaaaaacaaaaaaaaaacccaaaaacccccccccccaffffaaaaccccccccccccccaaa
abccccccccccccaaacaaaaaacccccccccccaaaaaaaacaaaaaaaaaaaaaaaaaaaaaaaaacaaaaaaacccaaacaaaccaaaaaacccccccccccccccccaaaccccccccccccccaaa
abccccccccccccaaccaaaaaccccccccccccccaaaaaaaccccaaaaaaaaaaaaccccaacccccaaaaaacccaaaccccccaaccaacccccccccccccccccaaacccccccccccaaaaaa
abcccccccccccccccaaaaaaaaccccccccccccaacccacccccccaaaaaaaaaaccccaacccccaaccccccccaccccccccccccccccccccccccccccccccccccccccccccaaaaaa
"""
