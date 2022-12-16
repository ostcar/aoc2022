module Days.Day10 exposing (puzzleInput, solution, testSolution)

import Expect
import Parser exposing ((|.), (|=))
import Test


solution : String -> ( String, String )
solution input =
    ( run1 input
    , run2 input
    )


testSolution : Test.Test
testSolution =
    let
        s2Expect =
            """##..##..##..##..##..##..##..##..##..##..
###...###...###...###...###...###...###.
####....####....####....####....####....
#####.....#####.....#####.....#####.....
######......######......######......####
#######.......#######.......#######....."""
    in
    Test.test "test input" <|
        \_ -> ( "13140", s2Expect ) |> Expect.equal (solution testInput)


run1 : String -> String
run1 =
    parseInput
        >> combine20AndThen40
        >> List.foldl
            (\instr { reg, multiplier, sum } ->
                let
                    newReg =
                        callInstruction instr reg
                in
                { reg = newReg, multiplier = multiplier + 40, sum = sum + multiplier * newReg }
            )
            { reg = 1, multiplier = 20, sum = 0 }
        >> .sum
        >> String.fromInt


run2 : String -> String
run2 input =
    parseInput input
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( idx, instr ) ( reg, output ) ->
                let
                    add40 =
                        if idx > 0 && modBy 40 (idx + 1) == 0 then
                            40

                        else
                            0

                    newReg =
                        callInstruction instr reg + add40
                in
                if idx == reg || idx == reg + 1 || idx == reg - 1 then
                    ( newReg, output ++ "#" )

                else
                    ( newReg, output ++ "." )
            )
            ( 1, "" )
        |> Tuple.second
        |> String.toList
        |> listWithNElements 40
        |> List.map String.fromList
        |> String.join "\n"


combine20AndThen40 : List Instruction -> List Instruction
combine20AndThen40 list =
    let
        ( combined, rest ) =
            -- TODO, rewrite combineN so it takes 20 instead of 19
            combineN 19 list
    in
    combined :: combine40 rest


combine40 : List Instruction -> List Instruction
combine40 list =
    if List.length list < 40 then
        []

    else
        let
            ( combined, rest ) =
                combineN 40 list
        in
        combined :: combine40 rest


combineN : Int -> List Instruction -> ( Instruction, List Instruction )
combineN n list =
    let
        ( take, rest ) =
            listSplitAfter n list
    in
    ( combineInstruction take, rest )


listWithNElements : Int -> List a -> List (List a)
listWithNElements n list =
    let
        firstToSecond ( first, second ) =
            List.reverse first :: second
    in
    List.foldl
        (\e ( tmp, full ) ->
            if List.length tmp < n then
                ( e :: tmp, full )

            else
                ( [ e ], firstToSecond ( tmp, full ) )
        )
        ( [], [] )
        list
        |> firstToSecond
        |> List.reverse


listSplitAfter : Int -> List a -> ( List a, List a )
listSplitAfter n list =
    ( List.take n list
    , List.drop n list
    )


combineInstruction : List Instruction -> Instruction
combineInstruction =
    List.foldl
        (\a b ->
            case a of
                Noop ->
                    b

                Add x ->
                    case b of
                        Noop ->
                            Add x

                        Add y ->
                            Add (x + y)
        )
        Noop


type Instruction
    = Add Int
    | Noop


callInstruction : Instruction -> Int -> Int
callInstruction instr a =
    case instr of
        Add x ->
            a + x

        Noop ->
            a


parseInput : String -> List Instruction
parseInput =
    String.lines
        >> List.foldl
            (\line instructions ->
                case parseAddX line of
                    Nothing ->
                        Noop :: instructions

                    Just x ->
                        Add x :: Noop :: instructions
            )
            []
        >> List.reverse


parseAddX : String -> Maybe Int
parseAddX =
    let
        parser =
            Parser.succeed identity
                |. Parser.keyword "addx"
                |. Parser.spaces
                |= Parser.oneOf
                    [ Parser.succeed negate
                        |. Parser.symbol "-"
                        |= Parser.int
                    , Parser.int
                    ]
    in
    Parser.run parser
        >> Result.toMaybe


testInput : String
testInput =
    """addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop"""


puzzleInput : String
puzzleInput =
    """addx 1
addx 4
addx 21
addx -20
addx 4
noop
noop
addx 5
addx 3
noop
addx 2
addx 1
noop
noop
addx 4
noop
noop
noop
addx 3
addx 5
addx 2
addx 1
noop
addx -37
addx 22
addx -4
addx -14
addx 2
addx 5
addx 3
addx -2
addx 2
addx 5
addx 2
addx -15
addx 32
addx -14
addx 5
addx 2
addx 3
noop
addx -13
addx -2
addx 18
addx -36
noop
addx 11
addx -7
noop
noop
addx 6
addx 22
addx -21
addx 3
addx 2
addx 4
noop
noop
noop
addx 5
addx -16
addx 17
addx 2
addx 5
addx -11
addx 15
addx -15
addx -24
noop
noop
addx 7
addx 2
addx -6
addx 9
noop
addx 5
noop
addx -3
addx 4
addx 2
noop
noop
addx 7
noop
noop
noop
addx 5
addx -28
addx 29
noop
addx 3
addx -7
addx -29
noop
addx 7
addx -2
addx 2
addx 5
addx 2
addx -3
addx 4
addx 5
addx 2
addx 8
addx -30
addx 25
addx 7
noop
noop
addx 3
addx -2
addx 2
addx -10
addx -24
addx 2
noop
noop
addx 2
noop
addx 3
addx 2
noop
addx 3
addx 2
addx 5
addx 2
noop
addx 1
noop
addx 2
addx 8
noop
noop
addx -1
addx -9
addx 14
noop
addx 1
noop
noop
"""
