module Days.Day6 exposing (solution, testSolution1, testSolution2)

import Expect
import Set
import Test


solution : ( () -> String, () -> String )
solution =
    ( \_ -> firstNUniquePosition 4 puzzleInput |> String.fromInt
    , \_ -> firstNUniquePosition 14 puzzleInput |> String.fromInt
    )


testSolution1 : Test.Test
testSolution1 =
    let
        testTable =
            [ ( "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7 )
            , ( "bvwbjplbgvbhsrlpgdmjqwftvncz", 5 )
            , ( "nppdvjthqldpwncqszvftbrmjlhg", 6 )
            , ( "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10 )
            , ( "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11 )
            ]
    in
    Test.describe "solution1" <|
        List.map
            (\( input, expect ) ->
                Test.test
                    (input ++ "==" ++ String.fromInt expect)
                    (\() -> firstNUniquePosition 4 input |> Expect.equal expect)
            )
            testTable


testSolution2 : Test.Test
testSolution2 =
    let
        testTable =
            [ ( "mjqjpqmgbljsphdztnvjfqwrcgsmlb", 19 )
            , ( "bvwbjplbgvbhsrlpgdmjqwftvncz", 23 )
            , ( "nppdvjthqldpwncqszvftbrmjlhg", 23 )
            , ( "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 29 )
            , ( "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 26 )
            ]
    in
    Test.describe "solution2" <|
        List.map
            (\( input, expect ) ->
                Test.test
                    (input ++ "==" ++ String.fromInt expect)
                    (\() -> firstNUniquePosition 14 input |> Expect.equal expect)
            )
            testTable


firstNUniquePosition : Int -> String -> Int
firstNUniquePosition n input =
    String.length input - String.length (findFirstNUnique n input) + n


findFirstNUnique : Int -> String -> String
findFirstNUnique n input =
    let
        first =
            String.left n input
    in
    if firstNUnique n first then
        input

    else
        String.dropLeft 1 input
            |> findFirstNUnique n


firstNUnique : Int -> String -> Bool
firstNUnique n input =
    if String.length input < n then
        False

    else
        String.left n input
            |> String.toList
            |> Set.fromList
            |> Set.size
            |> (==) n


puzzleInput : String
puzzleInput =
    """rhghwwsmsgmgsmmmlzljllrddvsvhvhhnvvrcccwhhvgghchwhvwvcvrrrgtgrgdggfdgffshsllvvslsppglgvgwgswwcbcwbwrrbjbwjjtgjjdzdfdhfddrmmhqhpqhhqghhzssqzqccbwwffzvzffvtftddrbrtrrcjrrmmbmrrrlrppplmplmppfzpfpvpqvpqqdndpndnmnwmwjmwmwbbrhhmzmffwfqwfqwffppvfpvffnwwwwcbwbhbccvmmplmplmmlmplpprbpbllmhlhzhffngfnntrrsrprrvlllvjvmjvvppsrrfnrfnrfrfwrffrsfrflfltfllpfprrthhhnhqnnfwwcttvzzddqsddwpddnrnttvjttjjdjvdjvjnvnrvvchhschhlffjggtqgqnnvrnrsnrsnsvspsnncppvwvvtmmcqmcmscmsmwmvmpvvlqqmbqqlwwtgtztgttpjjbfjjzhzthhzssffwqqdmdcdlclrclrccbmmqjqmqcmmnnsvnntpphhdpdhhtbtdtffhzfhhpwhppwbppfspphdhggzqgzzffdsdfsszcscpscsqcssffhjhnhhlqqdbqqdmdsmddrfddndldgglvglvlggqlggjngjnnbffthhjbjpjnjmnmgmqqbjqjzjqzzzjffwttcnnzffmllcncmnnthnncttpgpnnjdjfftmmvqmqsqggfrfjjqbjbvvshvhphfhhljlfjjvcclzclctcmmfwfvwfvwwjvwvjjplpbpqqdvqqbhbdbnnsndssgffgsglssmfmjjfrjfrfttljldjdffjjdnjjlbbnrntrtfrttmhhndnsdsffgqqrpplvvfwvfvsfszzfzhfzzlttlctllrzrggvwvhhgnnlmnlmlpppdldqqdpdlplrpppjspspzzhphccgjjpmmwvwbwssnpssrzrwrhhmzhmhsmhmttrqrvqvcclggptpjpzpccltctppflfjffrtrjrhjhffhjfjbfjfqfqzffdtthwwjwzjzggsgzgqqjdqjddvgvsvnnqwqgwgtttfvfrfwrfwfwmwmqwmmmbzmzvzfvfrfvvgvtvptpbpwbwrbwrwffzzfgzfgfcfzcffjdjjvrvnnfmmtztffsggjcjzcjcssscnscnssmmwgwbbljlnjndnrrqwqjjjbnnmbmcmbcmbbvvmtvmmfcmcjcdcnnzfzfzqfzfvvmbvmmndnbnsnnlddqvvsnngvnnrttfbtbqqnwqqzfzfzccfmmgrrsbbhvvgjjhfhssjmmsjjzzbqblqlfqllwhlwwvvstvtfvttnncjjzcjcjrrsvrssmfsfczbrzrvscvmcmrjpzwhcqfrrzbljnmqlzbzqtmhrshlrjjpvhnsvtlhqggqwppsjpszmqwfqmlwbqzwcrggrvfbvztnflwvbrqcrqbcllswvsvhwjzpldgphwptfdnlgdlnbttjfzrdcfvpdhlssfsljvdjmwddbnrpqnnqlfdfdspbnjwqjwrgtnrftsqcfjpmqwgwhttggjwzvgbwlhmtmmjlwhssrgshzpbcnstzlqdshdhjfgqlsmqqhpwbscsjhfbhprvhmftqngjgdbcvfldqgqsjqjfdmcvsflwzflsjfssnjpbwffnsfcnrdsphbjpgghmcthgnzmpgppqjdvbztvhnwqzndntcpjdtwwhvsmgdcthpssszrqcbntgsznpghmbqddpqscntjprlwzhbzhjtwzbwwcldwdgsttzmtnstjnngzrgvhncdbgqnllfzbthldztsdwsngjzprbvrnbzrsghlgssbqfnbvhnhzwmmmtncvsdngdtwcbjlnnzbnlrnmrvnvsjnvzdqnggmsvljlvjznwdszcmblhrsjvczpnlhsmqjsmwhbjbplbtqsgqjdllhncwdgbvzwnmqvndcbfhnvtjnzmvjhwzvdldhgfwbqqzcnbflfnwntlhmqgdhmrgwqcpmsvfbmwhbtsbdlhcnbcbswvdfffgjvddrbpzpcwsrsjnfvmzhlvbdnwttnqrmzbnnntbfvptrlhjhwjcsbnhvtwtwzvgfnzjplthqjbsjjwzdtqqvblnbvgcmvrmnvmwfrhcqgvrcjlfzdlpbfvncbtfgvnsflbjzqqhczcmtbwqmrppmptfgzvfbmcslwlfrfpvvnvvnwfvvmmdzmmtjsgqdfhngtphtlfjqrtljgnthgnbbqfrnpfmpwhpzdvzmtswwdvcnpsdcqwjdwlvbsbmlwdsjbcbgcrfljshlvpngfmsrzlfhtfqgwbcctnzzhnqhdmqzdwthftwtmpbcmqvdcdtgvltbzmszzwwmhzlfvbdqnhjqgdmstsnhftcwzvvbmnhwvgqzscwcdjbdgfmvpjdzctwqwltbwjlgcblnnhpnmggbmvqpqtgqjzspgqzvcvsdbvjgjfzdzhfpbzjqljjcgldzgnlmtjcmfgdbqgglvjqrppwmhccvqzvsrjjvfhjprwdsqsnszfprznljtcsrtqhcrpljfrccflmbpvqtzgmzhjrlbnrmmsmmjbtzwpglqgdvvvjvnfzmplsmvlvcnjshvjwntclwgpznnzwhjssgdcjbzrmsgnfgcgphrhfvrfhzwdcvsplhbmqwhpmjvqlmschznbqblvhtqfgtdggmncndhhplnzjphccjmlmtdqnmnlnpnfqdstljqnsqbrjrtspvrwvdmwzlgdmsfvctzgtmgqhqqrzpbgplzcfdqnzhsqrbcvhsccshnnpvvrpvqzqsgzgmpzfvvrrcvhdtntnsqnjrbzlbzmpgwdqzbhtlrrhbwdqjlsfgdhmvgmgbqhwvljmmqfllqvvrznrlftgzjdcgtstjffqmgvffpvtctzpdqjfnmlcdzscntctmqhrtmhrlhbjzttrcvcnlhsrvtpbmdchhntnnpnzlvqqnsrjcmblcvphqgwshnjmplgvnbsmmdzgqcpqztjhhgjvtlbpdpdwlwmmfrdgcvzfbgvbgpbjnwsssvhszwplcgvpgjwdrwngbcdjwvlsfhqlrqzgrzpfgjstqfdbrpqdvrlgdwqcsrgvhctznjjlzsmzctsqtfnhhlpjgnltssglmlwshfbrgmjqbvsmqwvszdfsvhmtrfjgwjctpsmgzzjbpwsztnnvzrhwvvmhdpgdmwzsjprhlgzcdvhznlfgjqvcwqrplcfvzmthsdsnrtfvnlrmvwplmbdvdggmlvpgdgzhvzmvzwmptzsnfrcrjspccmqjpjmhjqgrjbdcdjbzjmphmcdvjqtmdshhjrqgjgsnpzfbfgpjpczwzvmclgzgztlvzmdbwgncnndjwhhhjnhtjdmcnrmnqbmjdrdcmtvcsmftqcfhsvhsfjmtzjpnwffggpfqlqmzlbhnnhbtgzfgnjvdzmvthqjrhzbwvhcjzcsmsvsctrqbltpcrpjjnsbjdbfjqfcbpcgcwtqsflmlwprjcwlmcjjgsfdpwcqvhjpsgvdgsfnscnbzsrmrbbvdrlltzplbvgqsdnplcvbhddbtmwnfmvqhqdlrtrmrmzmhlccgwgmbdppjqdjtwmvdzfsbsggrfstjwjpjnljffwffmqncfnthnhglwvsgvzmgbzhtdfpfmdwmcldthvsnqnptpmhqctblgfsszhcfbvcrggjdhthqvvvlldshvqwmvdtfslrhzvgdfwztrczdjgcfcgtmwnphqthlgpfnrqwcgpzwnlgdvsnvzftlnlfflfsmjzhrhqjctsbvtccwbfsdrnbhszzjhqndvwcsmffnstnfdfwpbgfztjmjngdczzlgpscjtshpmmmzlnqndsttbdgfjqcvbqlphwhlhgcvjbhjmtrfzlgpwdnvzrllndbhvhlngvhlszzdcrdgvrmjwcvhhtbhnjmdzgctqnpdlrnqjzbchjtcsggsczlgmvtqvzmsqvtrhtvdmzlcdddfnbvbsnrzvgzfqjtbhjqhdznrhbfbqwtnwvrfqsznbqfzfzfgmhvjjsgbbdbdtzswwlnfrq"""
