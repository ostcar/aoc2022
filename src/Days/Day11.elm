module Days.Day11 exposing (puzzleInput, solution, testSolution)

import Array exposing (Array)
import Expect
import Test


solution : String -> ( String, String )
solution input =
    ( run WorryReductionSolution1 20 input
    , run WorryReductionSolution2 10000 input
    )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "10605", "2713310158" )


run : WorryReduction -> Int -> String -> String
run wr rounds input =
    parseInput input
        |> callNTimes rounds (playRound wr)
        |> Array.toList
        |> List.sortBy .business
        |> List.reverse
        |> List.take 2
        |> List.foldl (\m r -> m.business * r) 1
        |> String.fromInt


type WorryReduction
    = WorryReductionSolution1
    | WorryReductionSolution2


type alias Monkey =
    { items : List Int
    , operation : Int -> Int
    , testDivisible : Int
    , trueTo : Int
    , falseTo : Int
    , business : Int
    }


parseInput : String -> Array Monkey
parseInput input =
    input
        |> String.split "\n\n"
        |> List.map parseMonkey
        |> filterJust
        |> Array.fromList


parseMonkey : String -> Maybe Monkey
parseMonkey input =
    case String.lines input of
        _ :: item :: operation :: divisible :: trueTo :: falseTo :: _ ->
            Just
                { items = parseItemsLine item
                , operation = parsePperationLine operation
                , testDivisible = parseToMonkeyLine divisible
                , trueTo = parseToMonkeyLine trueTo
                , falseTo = parseToMonkeyLine falseTo
                , business = 0
                }

        _ ->
            Nothing


parseItemsLine : String -> List Int
parseItemsLine =
    String.dropLeft 18
        >> String.split ","
        >> List.map String.trim
        >> List.map String.toInt
        >> filterJust


parsePperationLine : String -> (Int -> Int)
parsePperationLine input =
    if input == "  Operation: new = old * old" then
        \a -> a * a

    else
        let
            by =
                String.dropLeft 25 input
                    |> String.toInt
                    |> Maybe.withDefault 0
        in
        case String.slice 23 24 input of
            "+" ->
                (+) by

            "*" ->
                (*) by

            _ ->
                identity


parseToMonkeyLine : String -> Int
parseToMonkeyLine line =
    line
        |> String.split " "
        |> List.reverse
        |> List.head
        |> Maybe.withDefault ""
        |> String.toInt
        |> Maybe.withDefault 0


playRound : WorryReduction -> Int -> Array Monkey -> Array Monkey
playRound wr _ monkeys =
    monkeys
        |> callNTimes
            (Array.length monkeys)
            (\idx ms ->
                processMonkey wr idx ms
            )


processMonkey : WorryReduction -> Int -> Array Monkey -> Array Monkey
processMonkey wr idx monkeys =
    monkeys
        |> arrayUpdate idx processMonkeyBusiness
        |> processMonkeyItems wr idx


processMonkeyBusiness : Monkey -> Monkey
processMonkeyBusiness ({ business, items } as monkey) =
    { monkey | business = business + List.length items }


processMonkeyItems : WorryReduction -> Int -> Array Monkey -> Array Monkey
processMonkeyItems worryReduction idx monkeys =
    case Array.get idx monkeys of
        Nothing ->
            monkeys

        Just monkey ->
            case monkey.items of
                [] ->
                    monkeys

                worryLevel :: rest ->
                    let
                        newWarryLevel =
                            worryLevel
                                |> monkey.operation
                                |> reduceWorry worryReduction monkeys

                        toMonkey =
                            if modBy monkey.testDivisible newWarryLevel == 0 then
                                monkey.trueTo

                            else
                                monkey.falseTo
                    in
                    monkeys
                        |> arrayUpdate
                            toMonkey
                            (\m -> { m | items = newWarryLevel :: m.items })
                        |> arrayUpdate
                            idx
                            (\m -> { m | items = rest })
                        |> processMonkeyItems worryReduction idx


reduceWorry : WorryReduction -> Array Monkey -> Int -> Int
reduceWorry worryReduction monkeys level =
    case worryReduction of
        WorryReductionSolution1 ->
            level // 3

        WorryReductionSolution2 ->
            let
                modder =
                    Array.toList monkeys |> List.map .testDivisible |> List.foldl (*) 1
            in
            modBy modder level


filterJust : List (Maybe a) -> List a
filterJust =
    List.filterMap identity


callNTimes : Int -> (Int -> a -> a) -> a -> a
callNTimes n fn list =
    List.range 0 (n - 1)
        |> List.foldl (\idx e -> fn idx e) list


arrayUpdate : Int -> (value -> value) -> Array value -> Array value
arrayUpdate idx fn array =
    case Array.get idx array of
        Nothing ->
            array

        Just v ->
            Array.set idx (fn v) array


testInput : String
testInput =
    """Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1"""


puzzleInput : String
puzzleInput =
    """Monkey 0:
  Starting items: 56, 52, 58, 96, 70, 75, 72
  Operation: new = old * 17
  Test: divisible by 11
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 75, 58, 86, 80, 55, 81
  Operation: new = old + 7
  Test: divisible by 3
    If true: throw to monkey 6
    If false: throw to monkey 5

Monkey 2:
  Starting items: 73, 68, 73, 90
  Operation: new = old * old
  Test: divisible by 5
    If true: throw to monkey 1
    If false: throw to monkey 7

Monkey 3:
  Starting items: 72, 89, 55, 51, 59
  Operation: new = old + 1
  Test: divisible by 7
    If true: throw to monkey 2
    If false: throw to monkey 7

Monkey 4:
  Starting items: 76, 76, 91
  Operation: new = old * 3
  Test: divisible by 19
    If true: throw to monkey 0
    If false: throw to monkey 3

Monkey 5:
  Starting items: 88
  Operation: new = old + 4
  Test: divisible by 2
    If true: throw to monkey 6
    If false: throw to monkey 4

Monkey 6:
  Starting items: 64, 63, 56, 50, 77, 55, 55, 86
  Operation: new = old + 8
  Test: divisible by 13
    If true: throw to monkey 4
    If false: throw to monkey 0

Monkey 7:
  Starting items: 79, 58
  Operation: new = old + 6
  Test: divisible by 17
    If true: throw to monkey 1
    If false: throw to monkey 5"""
