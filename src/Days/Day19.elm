module Days.Day19 exposing (puzzleInput, runOnly, solution, testPlayRounds, testSolution)

import Expect
import Html.Attributes exposing (action)
import Parser exposing ((|.), (|=), Parser)
import Test


solution : String -> ( String, String )
solution input =
    case Parser.run blueprintListParser input of
        Ok blueprints ->
            ( run1 blueprints |> String.fromInt
            , run2 blueprints |> String.fromInt
            )

        Err _ ->
            ( "Invalid", "Input" )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "33", "3472" )


run1 : List Blueprint -> Int
run1 blueprints =
    List.foldl
        (\blueprint acc ->
            acc + (blueprint.id * playRounds blueprint 23 startInventory 0 [])
        )
        0
        blueprints


run2 : List Blueprint -> Int
run2 blueprints =
    blueprints
        |> List.take 3
        |> List.map (\b -> playRounds b 31 startInventory 0 [])
        |> List.product


runOnly : Int -> Int -> Int
runOnly minutes id =
    case Parser.run blueprintListParser puzzleInput of
        Ok blueprints ->
            case List.drop (id - 1) blueprints |> List.head of
                Just blueprint ->
                    id * playRounds blueprint minutes startInventory 0 []

                Nothing ->
                    -1

        Err _ ->
            -1


type alias Inventory =
    { wallet : Resources
    , robots : Resources
    }


startInventory : Inventory
startInventory =
    { wallet = { ore = 0, clay = 0, obsidian = 0, geode = 0 }
    , robots = { ore = 1, clay = 0, obsidian = 0, geode = 0 }
    }


type alias Resources =
    { ore : Int
    , clay : Int
    , obsidian : Int
    , geode : Int
    }


oreCost : Int -> Resources
oreCost i =
    { ore = i
    , clay = 0
    , obsidian = 0
    , geode = 0
    }


oreClayCost : Int -> Int -> Resources
oreClayCost i j =
    { ore = i
    , clay = j
    , obsidian = 0
    , geode = 0
    }


oreObsidianCost : Int -> Int -> Resources
oreObsidianCost i j =
    { ore = i
    , clay = 0
    , obsidian = j
    , geode = 0
    }


type alias Blueprint =
    { id : Int
    , oreRobot : Resources
    , clayRobot : Resources
    , obsidianRobot : Resources
    , geodeRobot : Resources
    }


buildBlueprint : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Blueprint
buildBlueprint id oreRobot clayRobot obsidianRobotA obsidianRobotB geodeRobotA geodeRobotB =
    { id = id
    , oreRobot = oreCost oreRobot
    , clayRobot = oreCost clayRobot
    , obsidianRobot = oreClayCost obsidianRobotA obsidianRobotB
    , geodeRobot = oreObsidianCost geodeRobotA geodeRobotB
    }


blueprintParser : Parser Blueprint
blueprintParser =
    Parser.succeed buildBlueprint
        |. Parser.token "Blueprint "
        |= Parser.int
        |. Parser.token ": Each ore robot costs "
        |= Parser.int
        |. Parser.token " ore. Each clay robot costs "
        |= Parser.int
        |. Parser.token " ore. Each obsidian robot costs "
        |= Parser.int
        |. Parser.token " ore and "
        |= Parser.int
        |. Parser.token " clay. Each geode robot costs "
        |= Parser.int
        |. Parser.token " ore and "
        |= Parser.int
        |. Parser.token " obsidian."


blueprintListParser : Parser (List Blueprint)
blueprintListParser =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ Parser.symbol "\n" |> Parser.map (\_ -> Parser.Loop list)
                , Parser.succeed (\single -> Parser.Loop (single :: list))
                    |= blueprintParser
                    |. Parser.symbol "\n"
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse list))
                ]
        )


type Action
    = DoNothing
    | BuyOreRobot
    | BuyClayRobot
    | BuyObsidianRobot
    | BuyGeodeRobot


testPlayRounds : Test.Test
testPlayRounds =
    let
        blueprint =
            { clayRobot = { clay = 0, geode = 0, obsidian = 0, ore = 2 }, geodeRobot = { clay = 0, geode = 0, obsidian = 7, ore = 2 }, id = 1, obsidianRobot = { clay = 14, geode = 0, obsidian = 0, ore = 3 }, oreRobot = { clay = 0, geode = 0, obsidian = 0, ore = 4 } }

        emptyResources =
            { ore = 0, clay = 0, obsidian = 0, geode = 0 }
    in
    Test.describe "playRound examples" <|
        [ Test.test "start Late" <|
            \_ ->
                playRounds blueprint 1 { robots = emptyResources, wallet = emptyResources } 0 [] |> Expect.equal 0
        , Test.test "start Late with mony" <|
            \_ ->
                playRounds blueprint 1 { robots = emptyResources, wallet = { emptyResources | ore = 1000, obsidian = 1000 } } 0 [] |> Expect.equal 1
        , Test.test "two Rounds with mony" <|
            \_ ->
                playRounds blueprint 2 { robots = emptyResources, wallet = { emptyResources | ore = 1000, obsidian = 1000 } } 0 [] |> Expect.equal 3
        , Test.test "two Rounds with mony have better" <|
            \_ ->
                playRounds blueprint 2 { robots = emptyResources, wallet = { emptyResources | ore = 1000, clay = 1000 } } 4 [] |> Expect.equal 4
        , Test.test "first buy obsidian, then geode" <|
            \_ ->
                playRounds
                    { blueprint | geodeRobot = { emptyResources | obsidian = 1 } }
                    3
                    { robots = emptyResources, wallet = { emptyResources | ore = 1000, clay = 1000 } }
                    0
                    []
                    |> Expect.equal 1
        ]


playRounds : Blueprint -> Int -> Inventory -> Int -> List Action -> Int
playRounds blueprint minute inventory bestCase blocked =
    if minute == 0 then
        inventory.wallet.geode + inventory.robots.geode

    else if maxHarvest inventory.robots.geode minute + inventory.wallet.geode <= bestCase then
        bestCase

    else
        let
            nextOptions =
                nextActions blueprint inventory.wallet blocked
        in
        nextOptions
            |> List.foldl
                (\( action, afterPaying ) acc ->
                    let
                        newInventory =
                            { wallet = harvestResources inventory.robots afterPaying
                            , robots = addRobot action inventory.robots
                            }

                        blockedActions =
                            case action of
                                DoNothing ->
                                    blocked ++ List.map Tuple.first nextOptions

                                _ ->
                                    []
                    in
                    max acc <| playRounds blueprint (minute - 1) newInventory acc blockedActions
                )
                bestCase


maxHarvest : Int -> Int -> Int
maxHarvest robots minutes =
    -- (robots+0) + (robots+1) + (robots+2) ... + (robots+minutes)
    robots * (minutes + 1) + minutes * (minutes + 1) // 2


addRobot : Action -> Resources -> Resources
addRobot action robots =
    case action of
        BuyOreRobot ->
            { robots | ore = robots.ore + 1 }

        BuyClayRobot ->
            { robots | clay = robots.clay + 1 }

        BuyObsidianRobot ->
            { robots | obsidian = robots.obsidian + 1 }

        BuyGeodeRobot ->
            { robots | geode = robots.geode + 1 }

        _ ->
            robots


harvestResources : Resources -> Resources -> Resources
harvestResources robots wallet =
    { ore = wallet.ore + robots.ore
    , clay = wallet.clay + robots.clay
    , obsidian = wallet.obsidian + robots.obsidian
    , geode = wallet.geode + robots.geode
    }


nextActions : Blueprint -> Resources -> List Action -> List ( Action, Resources )
nextActions blueprint wallet blocked =
    case payResources wallet blueprint.geodeRobot of
        Just payedInventory ->
            [ ( BuyGeodeRobot, payedInventory ) ]

        Nothing ->
            [ BuyOreRobot, BuyClayRobot, BuyObsidianRobot ]
                |> filterBlocked blocked
                |> List.foldl
                    (\action acc ->
                        case payResources wallet (actionBlueprint action blueprint) of
                            Just payed ->
                                ( action, payed ) :: acc

                            Nothing ->
                                acc
                    )
                    [ ( DoNothing, wallet ) ]


filterBlocked : List Action -> List Action -> List Action
filterBlocked blocked actions =
    case actions of
        first :: rest ->
            if List.member first blocked then
                filterBlocked blocked rest

            else
                first :: filterBlocked blocked rest

        [] ->
            []


actionBlueprint : Action -> Blueprint -> Resources
actionBlueprint action blueprint =
    case action of
        BuyOreRobot ->
            blueprint.oreRobot

        BuyClayRobot ->
            blueprint.clayRobot

        BuyObsidianRobot ->
            blueprint.obsidianRobot

        BuyGeodeRobot ->
            blueprint.geodeRobot

        _ ->
            { ore = 0, clay = 0, obsidian = 0, geode = 0 }


payResources : Resources -> Resources -> Maybe Resources
payResources have need =
    let
        payed =
            { ore = have.ore - need.ore
            , clay = have.clay - need.clay
            , obsidian = have.obsidian - need.obsidian
            , geode = have.geode
            }
    in
    if payed.ore < 0 || payed.clay < 0 || payed.obsidian < 0 then
        Nothing

    else
        Just payed


testInput : String
testInput =
    """
Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.
"""


puzzleInput : String
puzzleInput =
    """
Blueprint 1: Each ore robot costs 2 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 20 clay. Each geode robot costs 2 ore and 17 obsidian.
Blueprint 2: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 11 clay. Each geode robot costs 4 ore and 12 obsidian.
Blueprint 3: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 19 clay. Each geode robot costs 4 ore and 15 obsidian.
Blueprint 4: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 20 clay. Each geode robot costs 2 ore and 10 obsidian.
Blueprint 5: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 18 clay. Each geode robot costs 2 ore and 19 obsidian.
Blueprint 6: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 11 clay. Each geode robot costs 2 ore and 16 obsidian.
Blueprint 7: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 8 clay. Each geode robot costs 3 ore and 7 obsidian.
Blueprint 8: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 17 clay. Each geode robot costs 2 ore and 13 obsidian.
Blueprint 9: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 12 clay. Each geode robot costs 3 ore and 17 obsidian.
Blueprint 10: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 15 clay. Each geode robot costs 3 ore and 9 obsidian.
Blueprint 11: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 16 clay. Each geode robot costs 2 ore and 18 obsidian.
Blueprint 12: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 15 clay. Each geode robot costs 2 ore and 8 obsidian.
Blueprint 13: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 11 clay. Each geode robot costs 3 ore and 15 obsidian.
Blueprint 14: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 9 clay. Each geode robot costs 3 ore and 7 obsidian.
Blueprint 15: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 2 ore and 16 clay. Each geode robot costs 2 ore and 8 obsidian.
Blueprint 16: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 16 clay. Each geode robot costs 3 ore and 15 obsidian.
Blueprint 17: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 20 clay. Each geode robot costs 2 ore and 8 obsidian.
Blueprint 18: Each ore robot costs 2 ore. Each clay robot costs 4 ore. Each obsidian robot costs 4 ore and 13 clay. Each geode robot costs 3 ore and 11 obsidian.
Blueprint 19: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 12 clay. Each geode robot costs 2 ore and 10 obsidian.
Blueprint 20: Each ore robot costs 3 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 19 clay. Each geode robot costs 3 ore and 19 obsidian.
Blueprint 21: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 19 clay. Each geode robot costs 3 ore and 10 obsidian.
Blueprint 22: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 16 clay. Each geode robot costs 2 ore and 11 obsidian.
Blueprint 23: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 10 clay. Each geode robot costs 3 ore and 10 obsidian.
Blueprint 24: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 15 clay. Each geode robot costs 4 ore and 16 obsidian.
Blueprint 25: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 2 ore and 5 clay. Each geode robot costs 2 ore and 10 obsidian.
Blueprint 26: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 18 clay. Each geode robot costs 4 ore and 16 obsidian.
Blueprint 27: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 11 clay. Each geode robot costs 3 ore and 14 obsidian.
Blueprint 28: Each ore robot costs 4 ore. Each clay robot costs 4 ore. Each obsidian robot costs 2 ore and 7 clay. Each geode robot costs 4 ore and 13 obsidian.
Blueprint 29: Each ore robot costs 3 ore. Each clay robot costs 4 ore. Each obsidian robot costs 3 ore and 20 clay. Each geode robot costs 3 ore and 14 obsidian.
Blueprint 30: Each ore robot costs 4 ore. Each clay robot costs 3 ore. Each obsidian robot costs 4 ore and 8 clay. Each geode robot costs 2 ore and 8 obsidian.
"""
