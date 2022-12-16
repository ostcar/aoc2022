module Days.Day16 exposing (Context, Valve(..), contextParser, listParser, listValveNameParser, puzzleInput, run1, solution, testInput, testSolution, valueParser, valveNameParser)

-- (puzzleInput, solution, testSolution)

import Char exposing (isAlpha)
import Dict exposing (Dict)
import Expect
import Parser exposing ((|.), (|=), Parser, succeed)
import Set exposing (Set)
import Test


solution : String -> ( String, String )
solution input =
    ( run1 input
    , "TODO"
    )


testSolution : Test.Test
testSolution =
    Test.test "test input" <|
        \_ -> solution testInput |> Expect.equal ( "1651", "TODO" )


type alias Context =
    { valves : Dict String Valve
    }


type Valve
    = Valve Int (List String)


valveRate : Context -> String -> Int
valveRate context str =
    Dict.get str context.valves
        |> Maybe.andThen (\(Valve rate _) -> Just rate)
        |> Maybe.withDefault 0


nextValves : Context -> String -> List String
nextValves context str =
    Dict.get str context.valves
        |> Maybe.andThen (\(Valve _ next) -> Just next)
        |> Maybe.withDefault []



-- Parser


contextParser : Parser Context
contextParser =
    listParser valueParser
        |> Parser.map (\list -> { valves = Dict.fromList list })


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
        succeed ()
            |. Parser.chompWhile isAlpha


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



-- Part 1


run1 : String -> String
run1 input =
    case Parser.run contextParser input of
        Ok context ->
            walk context
                |> String.fromInt

        Err _ ->
            "Parsing Error"


type alias Step =
    ( Set String, String )


walk : Context -> Int
walk context =
    walkHelper context 30 ( Set.empty, "AA" ) ( Set.empty, "AA" ) [ ( Set.empty, "AA" ) ]


walkHelper : Context -> Int -> Step -> Step -> List Step -> Int
walkHelper context depth lastStep (( openValves, _ ) as currentStep) path =
    let
        next =
            nextSteps context lastStep currentStep
    in
    if depth == 0 || List.length next == 0 then
        0

    else
        List.foldl
            (\(( nextOpenValves, _ ) as nextStep) acc ->
                let
                    value =
                        Set.diff nextOpenValves openValves
                            |> Set.map (valveRate context)
                            |> Set.foldl (+) 0
                            |> (*) (depth - 1)
                in
                walkHelper context (depth - 1) currentStep nextStep (nextStep :: path)
                    |> (+) value
                    |> max acc
            )
            0
            next


nextSteps : Context -> Step -> Step -> List Step
nextSteps context lastStep ( openValves, currentPos ) =
    let
        openSelf =
            if valveRate context currentPos > 0 && (not <| Set.member currentPos openValves) then
                [ ( Set.insert currentPos openValves, currentPos ) ]

            else
                []
    in
    nextValves context currentPos
        |> List.filterMap
            (\next ->
                if lastStep == ( openValves, next ) then
                    Nothing

                else
                    Just ( openValves, next )
            )
        |> (++) openSelf


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
