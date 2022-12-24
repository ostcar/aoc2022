module Days.Day24 exposing (puzzleInput, solution, testSolution)

import Array exposing (Array)
import Dict exposing (Dict)
import Expect
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Test


solution : String -> ( String, String )
solution input =
    case Parser.run mapParser input of
        Ok map ->
            ( run1 map |> String.fromInt
            , run2 map |> String.fromInt
            )

        Err _ ->
            ( "invalid", "input" )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "18", "54" )


run1 : Map -> Int
run1 map =
    go map ( 0, -1 ) (mapGoal map) 0


run2 : Map -> Int
run2 map =
    let
        start =
            ( 0, -1 )

        goal =
            mapGoal map
    in
    go map start goal 0
        |> go map goal start
        |> go map start goal


go : Map -> Position -> Position -> Int -> Int
go map start stop minute =
    breathFirstSearch map stop (Set.singleton start) minute


breathFirstSearch : Map -> Position -> Set Position -> Int -> Int
breathFirstSearch map goal queue deep =
    let
        neighbors =
            neighborsPositions queue
    in
    if Set.member goal neighbors then
        deep + 1

    else
        let
            newQueue =
                Set.filter (inBox map.max) neighbors
                    |> Set.union queue
                    |> Set.filter (noBlizzard map (deep + 1))
        in
        breathFirstSearch map goal newQueue (deep + 1)


mapGoal : Map -> Position
mapGoal { max } =
    let
        ( x, y ) =
            max
    in
    ( x, y + 1 )


neighborsPositions : Set Position -> Set Position
neighborsPositions poses =
    Set.foldl
        (\( x, y ) acc ->
            Set.fromList [ ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
                |> Set.union acc
        )
        Set.empty
        poses


inBox : Position -> Position -> Bool
inBox ( maxX, maxY ) ( x, y ) =
    x <= maxX && y <= maxY && x >= 0 && y >= 0


noBlizzard : Map -> Int -> Position -> Bool
noBlizzard map step ( x, y ) =
    let
        lineBlizzard =
            Array.get y map.lines
                |> Maybe.withDefault []
                |> List.any (\f -> f step == x)

        columnBlizzard =
            Array.get x map.columns
                |> Maybe.withDefault []
                |> List.any (\f -> f step == y)
    in
    not <| (lineBlizzard || columnBlizzard)


type alias Position =
    ( Int, Int )


type alias Map =
    { lines : Array (List (Int -> Int))
    , columns : Array (List (Int -> Int))
    , max : Position
    }


type alias MapHelper =
    { lines : Dict Int (List (Int -> Int -> Int))
    , columns : Dict Int (List (Int -> Int -> Int))
    , maxLines : Int
    , maxColumns : Int
    }


mapParser : Parser Map
mapParser =
    Parser.loop { lines = Dict.empty, columns = Dict.empty, maxLines = 0, maxColumns = 0 }
        (\map ->
            Parser.oneOf
                [ Parser.succeed (Parser.Loop map)
                    |. Parser.oneOf
                        [ Parser.symbol "\n"
                        , Parser.symbol "#"
                        , Parser.symbol "."
                        ]
                , Parser.succeed (mapParserHelper map)
                    |= (Parser.getPosition |> Parser.map (\( x, y ) -> ( x - 2, y - 2 )))
                    |= directionParser
                , Parser.succeed (map |> mapHelperToMap |> Parser.Done)
                    |. Parser.end
                ]
        )


upwards : Int -> Int -> Int -> Int
upwards start maxLines round =
    (start - round) |> modBy maxLines


downwards : Int -> Int -> Int -> Int
downwards start maxLines round =
    (start + round) |> modBy maxLines


leftwards : Int -> Int -> Int -> Int
leftwards start maxColumns round =
    (start - round) |> modBy maxColumns


rightwards : Int -> Int -> Int -> Int
rightwards start maxColumns round =
    (start + round) |> modBy maxColumns


mapParserHelper : MapHelper -> ( Int, Int ) -> Direction -> Parser.Step MapHelper Map
mapParserHelper map ( lineNr, columnNr ) direction =
    let
        line =
            Dict.get lineNr map.lines |> Maybe.withDefault []

        column =
            Dict.get columnNr map.columns |> Maybe.withDefault []

        ( newLine, newColumn ) =
            case direction of
                Up ->
                    ( line
                    , upwards lineNr :: column
                    )

                Down ->
                    ( line
                    , downwards lineNr :: column
                    )

                Left ->
                    ( leftwards columnNr :: line
                    , column
                    )

                Right ->
                    ( rightwards columnNr :: line
                    , column
                    )
    in
    { map
        | lines = map.lines |> Dict.insert lineNr newLine
        , columns = map.columns |> Dict.insert columnNr newColumn
        , maxColumns = max map.maxColumns columnNr
        , maxLines = max map.maxLines lineNr
    }
        |> Parser.Loop


mapHelperToMap : MapHelper -> Map
mapHelperToMap { lines, columns, maxLines, maxColumns } =
    let
        addMax m =
            Dict.map (\_ v -> v |> List.map (\f -> f (m + 1)))
    in
    { lines = lines |> addMax maxColumns |> arrayFromDict [] maxLines
    , columns = columns |> addMax maxLines |> arrayFromDict [] maxColumns
    , max = ( maxColumns, maxLines )
    }


arrayFromDict : a -> Int -> Dict Int a -> Array a
arrayFromDict default max dict =
    let
        fn : Int -> a
        fn n =
            Dict.get n dict |> Maybe.withDefault default
    in
    Array.initialize (max + 1) fn


type Direction
    = Up
    | Down
    | Left
    | Right


directionParser : Parser Direction
directionParser =
    Parser.oneOf
        [ Parser.succeed Up |. Parser.symbol "^"
        , Parser.succeed Down |. Parser.symbol "v"
        , Parser.succeed Left |. Parser.symbol "<"
        , Parser.succeed Right |. Parser.symbol ">"
        ]


testInput : String
testInput =
    """#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#
"""


puzzleInput : String
puzzleInput =
    """#.####################################################################################################
#>>^<^v>v<>^>>v<<.<<<^<vv^<^>v><^<v>><^>.<><^^>v<^>v>^^>v><v.^<.^>>>^>v^^^<vv^<><vvv>v>^vv<<>v^^<v><>#
#>^>^><^v<>.v><><<<>><v><>vv^.><v^<^v^>v<<<v..<v.v^v.v<<v<>>^^<v^v.<^.^v><^v^.^^<v>..v<^>^<<^>.v.>v.>#
#.>^>v<v^><v<v^><>^v>><>v<>^><>v^<^v<>v<>^<v<><^^v.v.>^^.v<<^v<>>.v>>v<^^.v<v^<^>>..>><<v.^>.<<<^v.^>#
#<>^^^..v^^v^<<><<<^<>.>.vvv.v>^^.><.^>>>v^^>^^vv>v<^v>>^^<v>v^<<^>..^v^<>>>>>.>>><v^^^vv<>v^v<>^^.<>#
#>v^vvv><>>v^v.v><v<<^.>.>>^<.^<^<>><^<><^>v.>v<>v><<>>><^v<.>v<^v<v^..<vv><><.^^^^<^.<v<<^^>>v^>^<><#
#>^>^>^<<>^><<v^^^^^^^v^<<>>v^^>v>v<.><><^v^<v>>>>^<..>v<.^v><^<^v>v^^vvv^v^<v>>.^<<<.^^vv>vvv>^..<v>#
#><vv.vv^v.>^>>v<<<v^>>^^>><v<<.^>>v>v^.<v<vv<<<.vv^<>^.<v>vv<.>^^v><><^<v<^><.^vv^^<><<.vv^<v<<v.v^>#
#<^<v>.>^v^<v^^><vv.>>.<^>.^..><v^<<v^<^<<.<v<^v^v<^<.<<v<^v>^<<^v<>^<^v>vv^>vv>^<v.>^><v..>><v^<^^^>#
#>^><v><<<>><^v^>v<><>v^><.v>>.>^v<.^><vvv<.>v<v>^>>v><^^^v^.v>^.^^>^v>>v^v<^v>.^.<^>^^v<.<<v^..<<.v>#
#>vv>^.<<>><^>.<^><v<^<>>v><v>^.>v^<><.v.>^>>^><<v>^v..<^^^<vv^..^^<<<.v<<v<vv^v^<^><><v>.<<><^<v<^<<#
#>^>v<^v^.<<v<<^^>^<>vvv<v^>^<v<.^v^>><>vv<>v<.^^>^<>>><>v<<>.>^<<>vvv^v.v<><<>^.v.>^><^^>>^>>><>v.^<#
#>^<.^.<>v<.<^v>v<^^<>^>vvv..v.^><<^<.><...<.^>^>^vv<>v^^><<v>v^^<^^><<^>>vvv^<^^v>>vv<v<><v^<^v<^v^>#
#.>>^.v<>v>><<..vv>^.>>^>^^.v>v.<^><>v<v^^><<v<.>>^<v<<><>.^<<.>>^<v<>v^>><v>^>..v<^<^.vv<>^^>>>v^>v<#
#<>^^^<v><^v^vvv>^v^v><<>>^<vv^><<v.>>v>.v>^>>v<^vv.<><<>><<v<>>^.<<>v.v^^vv<.>^<^v.<^<vvv^v.vv><vv.>#
#>vv.vv^><v>v>.<v^><v<vv^v<.v<<>^<^>^><v^>v>^<v><>v>>v>>>>v^.<v<vv>^v<^.^.^>>><>^<^<^.<v<.><v.^<^.><.#
#>^vvv<>^v^^>^<^v<^>v<<^<vv.vvv<<<v<v^..^.<<v<<<>>^>v^v^>^><v.^<v<<v>^^v<v.>>>v^<v^<v>^<^^.>v<^<>^.v<#
#<>^<<<<v<>^v>v^<vv<^v>v^^<>.^>^^<.>>v>v<<^<<<>v<v^v><>>.<<.^<>><<<v>v^>v^^<v^^.><vv>^^<v.v>^<v<.vv><#
#<>vvv<^^.v^^.v.<^<^>.^^v>^v>>.vv.>.v<<^<^^>vvv^<<<vv^<vv<>>v^^<^.<^.>v.^>v><^^>>^>^<>.>.^<<<.v.<>^>>#
#><>v<>>>><.^v>v><v>^<<><>^vv.^.<.>>vvvv><>^^><>v>v.^^<^<^v>v<^<.<^<>^^^^vvv>.>>><^vv<^^^>.^<<v^>>><>#
#<<><^>>^>^^>><.v^^^.v^^^>.^v<vvv.^^<^^>^<>>><<>>><<.v<^>><<v<v^>vv^<>vv.<<>v<v^><<vv^^^>vvvv<<v<><<<#
#<^<><>><<>^^>v<<^v<v>^^^>^^vvvv<^^vv<>><.<^^.vv^>>.<>^.v<><^^vv>^^>>^<^^v>.v<>>^v.v^>.^v<.<v.<>>>>v<#
#<^<<^^^vv><^<>^^>^v<v^v^.<v.>v>><<^^>v^>v>><v<<><>^^.vv^><.^>vv^v<<>^><^>^>^v^<><>^.^<.^.^>>^<.>^><<#
#>^.<<v>^.<<<^>v>>>^<.^>>>.^^<<^.v^<<>v^<<<^<><>^vvvv^>vv^^>.>>^>><>^^^^>v<^^>^^<^vv.v.^<<^<<^>v><.>>#
#>vv^.^vv>^<>^v.^^<v>v>><^^^v<<>>^>>^^>^<^<<<^.v><^>v<<v^<^^^>vv><<v<.<v><^vv>^>^v^<^v>^<^vv<<^v<.^^>#
#<v>vv<^>v^^<<^^<<<v>v><<<.v^>^<><vvv^>.^.v>v<^^vvvv>>^v^^v^^^<>>>>v>>^>v^.<>.>.^vvv^<^<.<^>^<.v<><.>#
#.<.<^v<v><^>.^.v<>>^<>.<<^.vv^>v^v<..>^.^^vv^><.<>^<><^vv>^<.<>v..<^^<v^^v<<<v<><<>>vv<>^^<>^vvvvv><#
#<.^v^>vvv<.v<.vv><^vv^>>.^>><><<v>v<^<<vv.<>v<vv^v>^>^^<v>^>^<<>^<<^>.v^^^><>.^^v>^...^<^>>>.>v^^vv<#
#.<^v>^>^^<^v.v>.^<><<<>>vv>.^v<>.<<>>v^v<v>>^.^<^^<.v^<><vvv>><^.>.vv<.vv^>^><<.<.>><^>vv^^>>v^>^>.<#
#.>^<>><v<.>vv<>>^<v.>^.<v^v.v<><>^<v^<v.<<^v<>v<<v>v^>^>v.>>v^.><^^.>v<v.v^.>^^<>^<.^<v^^><<>^vvv>><#
#.^.^vv^..^>.v.<<v>^^^>^<^v<^>>v<>v>.<><>>.<v^<^>.<v.^^v^<v><>^>v.<^<<><>v.<<<^>.<<.^>^>^<^.>^v>>>.>>#
#.^v>><>v<<.<^>^^<<>^>v>vv<>>^>>^.v^>.>^<v^v>>v^^><.v.<v>.>vv<<^v<v<v^v>v<v<.^<.^<<v>^^v.^v.<^>.v>^>>#
#>^v>^<<>>vv>^<.<v^v<^>>^v>^v^<<><<^^^^v>^^^^^^^vv<^..<vvv><^.<..^>^^<^>v^>>>>vv>v<<<<>.>.>^.^<^.v<v>#
#.^v.^>><.><<v^>^^.>><^v<..>..><.><><<v>>>v<>^.>v<vv^^<<.^.^^v<v..^<v^>.<vvv>^>v>^<^><>>>>v<>^>>vv^.>#
#<v<vv^vvv><v^<^>>.v^v^>><.>><>>v^^<v>>>>.v^.^.<vv.>>>^v>^<>v^.^>v<vv.>>vvv^v^<vv<>v<^<^.<<^..v^.^v<<#
#<.<v>^>v<^<.<><>.v<<^v<^<>^.vv<v<<v>v<^<^v>v<v^^vv<><..v<<<<.^^v<>>^<v>.^>>v<^>v>^><vvv<^>>.^v>^<.><#
####################################################################################################.#
"""
