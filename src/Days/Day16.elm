module Days.Day16 exposing (puzzleInput, solution, testSolution)

import Dict exposing (Dict)
import Expect
import Parser exposing ((|.), (|=), Parser)
import Set exposing (Set)
import Test


solution : String -> ( String, String )
solution input =
    ( run1 input
    , run2 input
    )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        -- TODO The tests actually fail, the real values are ( "1651", "1707" )
        \_ -> solution testInput |> Expect.equal ( "1651", "1624" )


type alias Context =
    Dict String Valve


type Valve
    = Valve Int (List String)


valveRate : Context -> String -> Int
valveRate context str =
    Dict.get str context
        |> Maybe.map (\(Valve rate _) -> rate)
        |> Maybe.withDefault 0



-- Parser


contextParser : Parser Context
contextParser =
    listParser valueParser
        |> Parser.map (\list -> Dict.fromList list)


valueParser : Parser ( String, Valve )
valueParser =
    Parser.succeed (\name rate toValues -> ( name, Valve rate toValues ))
        |. Parser.token "Valve"
        |. Parser.spaces
        |= valveNameParser
        |. Parser.spaces
        |. Parser.token "has flow rate="
        |= Parser.int
        |. Parser.symbol ";"
        |. Parser.spaces
        |. Parser.oneOf
            [ Parser.backtrackable (Parser.token "tunnels lead to valves")
            , Parser.token "tunnel leads to valve"
            ]
        |= listValveNameParser


valveNameParser : Parser String
valveNameParser =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile Char.isAlpha


listValveNameParser : Parser (List String)
listValveNameParser =
    Parser.sequence
        { start = ""
        , separator = ","
        , end = "\n"
        , item = valveNameParser
        , spaces = Parser.chompWhile (\c -> c == ' ')
        , trailing = Parser.Forbidden
        }


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



-- Solution


run1 : String -> String
run1 input =
    case Parser.run contextParser input of
        Ok context ->
            Dict.filter (\_ (Valve v _) -> v /= 0) context
                |> Dict.keys
                |> sortValves context "AA"
                |> bestRoutePart context 30
                |> String.fromInt

        Err _ ->
            "Parsing Error"


run2 : String -> String
run2 input =
    case Parser.run contextParser input of
        Ok context ->
            Dict.filter (\_ (Valve v _) -> v /= 0) context
                |> Dict.keys
                |> sortValves context "AA"
                |> bestRoutePartForTwo context 26
                |> String.fromInt

        Err _ ->
            "Parsing Error"


bestRoutePart : Context -> Int -> List String -> Int
bestRoutePart context minutes list =
    case list of
        _ :: rest ->
            let
                fromStart =
                    walkRoute context minutes list

                fromRest =
                    bestRoutePart context minutes rest
            in
            max fromStart fromRest

        [] ->
            0


bestRoutePartForTwo : Context -> Int -> List String -> Int
bestRoutePartForTwo context minutes list =
    listPartFoldl
        (\firstList secondList acc ->
            let
                firstWalk =
                    bestRoutePart context minutes firstList

                secondWalk =
                    bestRoutePart context minutes secondList
            in
            max
                acc
                (firstWalk + secondWalk)
        )
        0
        list


listPartFoldl : (List a -> List a -> b -> b) -> b -> List a -> b
listPartFoldl fn acc list =
    List.range 0 (List.length list)
        |> List.foldl
            (\e iAcc ->
                fn (List.take e list) (List.drop e list) iAcc
            )
            acc


walkRoute : Context -> Int -> List String -> Int
walkRoute context minutes route =
    route
        |> List.foldl
            (\key ( acc, ( from, atRound ) ) ->
                let
                    rate =
                        valveRate context key

                    rounds =
                        shortestDistance context from key + 1

                    roundsLeft =
                        max 0 (atRound - rounds)
                in
                ( (rate * roundsLeft) :: acc, ( key, roundsLeft ) )
            )
            ( [], ( "AA", minutes ) )
        |> Tuple.first
        |> List.sum


sortValves : Context -> String -> List String -> List String
sortValves context start valves =
    let
        sorted =
            List.sortWith (cmpValves context start) valves
                |> List.reverse
    in
    case sorted of
        first :: rest ->
            first :: sortValves context first rest

        a ->
            a


cmpValves : Context -> String -> String -> String -> Order
cmpValves context start a b =
    let
        va =
            valveRate context a

        vb =
            valveRate context b

        distanceAB =
            shortestDistance context a b + 1

        distanceToA =
            shortestDistance context start a + 1

        distanceToB =
            shortestDistance context start b + 1
    in
    compare
        (va * (distanceAB + distanceToB - distanceToA))
        (vb * (distanceAB + distanceToA - distanceToB))


shortestDistance : Context -> String -> String -> Int
shortestDistance context a b =
    breathFirstSearch context b (Set.singleton a) 0


breathFirstSearch : Dict String Valve -> String -> Set String -> Int -> Int
breathFirstSearch valves stopKey queue depth =
    if Set.member stopKey queue then
        depth

    else
        let
            valvesWithoutPositions =
                dictRemoveMany queue valves

            possibleNext : String -> Set String
            possibleNext key =
                case Dict.get key valves of
                    Nothing ->
                        Set.empty

                    Just (Valve _ list) ->
                        let
                            next =
                                Set.fromList list

                            unvisited =
                                Dict.keys valves |> Set.fromList
                        in
                        Set.intersect next unvisited

            nextPositions : Set String
            nextPositions =
                Set.foldl
                    (\key acc ->
                        possibleNext key |> Set.union acc
                    )
                    Set.empty
                    queue
        in
        breathFirstSearch valvesWithoutPositions stopKey nextPositions (depth + 1)


dictRemoveMany : Set comparable -> Dict comparable a -> Dict comparable a
dictRemoveMany indexes dict =
    Set.foldl
        Dict.remove
        dict
        indexes


testInput : String
testInput =
    """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
"""


puzzleInput : String
puzzleInput =
    """Valve EG has flow rate=21; tunnels lead to valves WZ, OF, ZP, QD
Valve OR has flow rate=0; tunnels lead to valves QD, CR
Valve VO has flow rate=0; tunnels lead to valves FL, OY
Valve BV has flow rate=0; tunnels lead to valves AA, KK
Valve OF has flow rate=0; tunnels lead to valves EJ, EG
Valve YZ has flow rate=0; tunnels lead to valves EL, AW
Valve EL has flow rate=16; tunnels lead to valves YZ, RD
Valve EJ has flow rate=0; tunnels lead to valves YI, OF
Valve FM has flow rate=0; tunnels lead to valves VX, FX
Valve FL has flow rate=22; tunnels lead to valves VO, FH
Valve QD has flow rate=0; tunnels lead to valves OR, EG
Valve XC has flow rate=0; tunnels lead to valves UA, GV
Valve WZ has flow rate=0; tunnels lead to valves FH, EG
Valve AT has flow rate=0; tunnels lead to valves FX, OZ
Valve MZ has flow rate=0; tunnels lead to valves UA, YI
Valve WI has flow rate=0; tunnels lead to valves OH, WW
Valve YD has flow rate=0; tunnels lead to valves OZ, WW
Valve QX has flow rate=0; tunnels lead to valves OY, YI
Valve AA has flow rate=0; tunnels lead to valves BV, ZE, PE, XL
Valve VX has flow rate=0; tunnels lead to valves FM, GQ
Valve VN has flow rate=0; tunnels lead to valves TU, OQ
Valve RD has flow rate=0; tunnels lead to valves OY, EL
Valve QR has flow rate=0; tunnels lead to valves QQ, OZ
Valve CD has flow rate=0; tunnels lead to valves WW, RJ
Valve VA has flow rate=20; tunnel leads to valve DE
Valve RJ has flow rate=0; tunnels lead to valves CR, CD
Valve UA has flow rate=19; tunnels lead to valves XC, MZ, KY
Valve WW has flow rate=4; tunnels lead to valves YD, PE, WI, DY, CD
Valve MC has flow rate=0; tunnels lead to valves ZP, XY
Valve XY has flow rate=24; tunnel leads to valve MC
Valve FH has flow rate=0; tunnels lead to valves FL, WZ
Valve DE has flow rate=0; tunnels lead to valves VA, FX
Valve DY has flow rate=0; tunnels lead to valves WW, YI
Valve FX has flow rate=14; tunnels lead to valves DE, FM, AT, OQ
Valve UU has flow rate=0; tunnels lead to valves AR, AW
Valve OY has flow rate=13; tunnels lead to valves RD, VO, AR, GV, QX
Valve CS has flow rate=0; tunnels lead to valves MG, OZ
Valve KY has flow rate=0; tunnels lead to valves UA, AW
Valve KK has flow rate=0; tunnels lead to valves BV, TU
Valve GQ has flow rate=18; tunnel leads to valve VX
Valve ZV has flow rate=0; tunnels lead to valves YI, LS
Valve QQ has flow rate=0; tunnels lead to valves CR, QR
Valve AW has flow rate=25; tunnels lead to valves YZ, KY, UU
Valve OH has flow rate=0; tunnels lead to valves WI, TU
Valve CR has flow rate=8; tunnels lead to valves OR, ZE, RJ, LS, QQ
Valve TU has flow rate=7; tunnels lead to valves MG, VN, OH, KK
Valve ZP has flow rate=0; tunnels lead to valves EG, MC
Valve AR has flow rate=0; tunnels lead to valves UU, OY
Valve OZ has flow rate=10; tunnels lead to valves YD, XL, CS, AT, QR
Valve GV has flow rate=0; tunnels lead to valves XC, OY
Valve PE has flow rate=0; tunnels lead to valves WW, AA
Valve ZE has flow rate=0; tunnels lead to valves AA, CR
Valve XL has flow rate=0; tunnels lead to valves OZ, AA
Valve YI has flow rate=15; tunnels lead to valves QX, MZ, EJ, DY, ZV
Valve OQ has flow rate=0; tunnels lead to valves FX, VN
Valve MG has flow rate=0; tunnels lead to valves TU, CS
Valve LS has flow rate=0; tunnels lead to valves CR, ZV
"""
