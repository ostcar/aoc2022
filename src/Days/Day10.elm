module Days.Day10 exposing (puzzleInput, solution, testSolution)

import Expect
import Parser exposing ((|.), (|=))
import Test


solution : String -> ( String, String )
solution input =
    ( run1 input
    , "TODO"
    )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> ( "13140", "TODO" ) |> Expect.equal (solution testInput)


run1 : String -> String
run1 input =
    parseInput input
        |> combine20AndThen40
        |> List.foldl
            (\instr { reg, multiplier, sum } ->
                let
                    newReg =
                        instr reg
                in
                { reg = newReg, multiplier = multiplier + 40, sum = sum + multiplier * newReg }
            )
            { reg = 1, multiplier = 20, sum = 0 }
        |> .sum
        |> String.fromInt


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
            listSplitAfter n  list
    in
    ( combineInstruction take, rest )


listSplitAfter : Int -> List a -> ( List a, List a )
listSplitAfter n list =
    ( List.take n list
    , List.drop n list
    )


combineInstruction : List Instruction -> Instruction
combineInstruction =
    List.foldl
        (\i combined ->
            combined >> i
        )
        instructionNoop


type alias Instruction =
    Int -> Int


instructionAddX : Int -> Int -> Int
instructionAddX =
    (+)


instructionNoop : Int -> Int
instructionNoop =
    identity


parseInput : String -> List Instruction
parseInput =
    String.lines
        >> List.foldl
            (\line instructions ->
                case parseAddX line of
                    Nothing ->
                        instructionNoop :: instructions

                    Just x ->
                        instructionAddX x :: instructionNoop :: instructions
            )
            []
        >> List.reverse


parseAddX : String -> Maybe Int
parseAddX input =
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
    Parser.run parser input
        |> Result.toMaybe


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
