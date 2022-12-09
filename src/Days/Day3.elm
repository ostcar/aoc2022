module Days.Day3 exposing (puzzleInput, solution, testSolution1, testSolution2)

import Expect exposing (equal)
import Set
import Test exposing (Test, test)


solution : String -> ( String, String )
solution input =
    ( solution1 input
    , solution2 input
    )


solution1 : String -> String
solution1 =
    String.lines
        >> List.filter (String.isEmpty >> not)
        >> List.map splitLine
        >> List.map findEqual
        >> List.map toPoint
        >> List.sum
        >> String.fromInt


splitLine : String -> ( String, String )
splitLine input =
    let
        half =
            String.length input // 2
    in
    ( String.left half input, String.dropLeft half input )


findEqual : ( String, String ) -> Char
findEqual ( first, second ) =
    let
        stringToSet : String -> Set.Set Char
        stringToSet s =
            String.toList s |> Set.fromList
    in
    Set.intersect (stringToSet first) (stringToSet second)
        |> Set.toList
        |> List.head
        |> Maybe.withDefault 'a'


toPoint : Char -> Int
toPoint c =
    let
        cCode =
            Char.toCode c
    in
    if Char.isUpper c then
        cCode - Char.toCode 'A' + 27

    else
        cCode - Char.toCode 'a' + 1


testSolution1 : Test
testSolution1 =
    test "test input1" <|
        \_ -> equal (solution1 testInput) "157"


solution2 : String -> String
solution2 =
    String.lines
        >> List.filter (String.isEmpty >> not)
        >> threeLines
        >> List.map findEqual3
        >> List.map toPoint
        >> List.sum
        >> String.fromInt


threeLines : List String -> List ( String, String, String )
threeLines list =
    case list of
        first :: second :: third :: rest ->
            ( first, second, third ) :: threeLines rest

        _ ->
            []


findEqual3 : ( String, String, String ) -> Char
findEqual3 ( first, second, third ) =
    let
        stringToSet : String -> Set.Set Char
        stringToSet s =
            String.toList s |> Set.fromList
    in
    Set.intersect (stringToSet first) (stringToSet second)
        |> Set.intersect (stringToSet third)
        |> Set.toList
        |> List.head
        |> Maybe.withDefault 'a'


testSolution2 : Test
testSolution2 =
    test "test input2" <|
        \_ -> equal (solution2 testInput) "70"


testInput : String
testInput =
    """
vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"""


puzzleInput : String
puzzleInput =
    """
CjhshBJCSrTTsLwqwqwb
GtmnFHlDfcpHbLZjtTTRLWwb
fDfNHHjVFNvvrvVBJJdS
PPWvWQjPhrPQwlMWJJdMDGbJTdCJ
rsqsStgNNggBNBZHSrJGdJdCFRRZCFbGbTdJ
qgBqqHzzggBpzSnBNqNSSSgcfhrVlVmwPljQVLVwVvQmmzVl
bBBGBfmGvBTnGtGJBtGpcJbZrrddjqrZhDldwdcqrjrjDr
HWPSQMsPHFsMWPVVMVSHCwDCDwwZZvwjwQZZwjdd
vVHPgHHFRLfpfJTLLtJL
LWLZhDBHhWWHjBwHwBjCTrBnnrQTQCJTJpTQBQ
vvdcqbRmvwSFmRqPFSqwdvtQnrpCQJpNNVnrptVCtCCP
wRSffqlFgvvdgdlzhLWWDzLljZhhGL
LNbTbPdTNgnShgSjmHcqtQGCtrctCPcQqc
vzWZDZZBlDwlzvDRZfFBRzVBtMMGHCcpjrqBGMtQQppqHtCq
DfRvFZjZRsRjlVWsjFlWVWvdSdbhsSmnSngTdTSTsJdSLm
nnZRbfZRTZfRsbZTFPRfpbRJdqqQNPwNqvvwvvvSwdQqdS
WjDzjMWMHpQwcSQWcJSS
hrHhHDgpphmjtMrGLDmGgmnbsflbfZCsnsClltsVsfRR
QFngsFnGdLGFGVRgLLqWPQPWvBrMDppCPrDB
NfcHZZzbHthSZtNtNfhHWhClDvCqlrqBvrCpWMWB
fSMSHjbTMVgVVngVgT
llnMffwbvCnffHvJJPJpPBNVVZDJDmmS
QssGGHhRgcqjRRTWGWRGDZcVFZBpNBmPFBDSmZNP
hzsRHWQshzgRjjsgQRTsbwzCtnrrlnrwzMCtvMff
HdddHHmtDMDTqHHSftmqdmfdssGzrsjVGtrllWlGZGsnlnnp
BQNPNPgPhBBhCJQhbCgCnrrnbZnlWznzpGssllVW
LFNZRvCPCFZmHmTLcwdwcq
zQRDChCnVhqRllpzQlzqCVVzPvNHTTFFHlNPsPNvTBPBHBFT
wcfdfDmfPHjdsHPs
cwGMwMMbwbmwJZfDSMmfwcpRVWzQqnnnzGVVnhqRVCph
CScCSPcPszFJWSMjGZHMpGMjvG
TTQfQvBTVBfrDVRDVqGMmjZqnpnGmMpnnpZZ
NDdbrQdVQDNNfvTVNdBfrDBJFcsLWcCJzWFCcFLbFcFJsc
WWdzhpHcHHrtzGBJMwmjJGmBtP
CqsgqNrVnlCBCvPjCBmPJm
LDDDLnVsqsgTQsgFcQzbQdprbWHzdb
nFpNPGLGrntlrFLpflfBTcJwSgwQvGwbgSvbvJvT
DHZWmMDZZDHPMHsDJQbbgSJcvbQgjwJM
CRdzzdDDVRHWWNlPfPzllnFLlL
ZJVqGSVCJCSgdSvtjtvcjcjbNl
pmDWFnDFMnDdFLDHffjcbjjtBNctBjBMbMcl
dLnfmdLLmrWsgZqCThgssgsq
CLsvLLQvrlrrpLpw
tmTHnNtgmzgWmpjlpjnwrrlRjw
WHgTdmNJmJTDDrtFWsVGQqBQqcCCbhdvCB
jwCHwmWRTWRWJwfcgVgflvPqPBPH
MpphdbZZpZMNZpsdpZLMgjBlBqDvLPjfLVPBgPfv
ZdrZsjQbnMCGwrRwzmTr
CMDsWppsfhjNNzzzcBrbPgnrrnVBQHBNrV
ZmGdTdvtStFDtTvtBHgPHnPHgnrnHVmH
ZtLZdwdGqtZqLTLtwvfjDWcRscqMCDsCfWjf
QQPPfPjLNLfSShfNRSRrrGHzvHrvlvnHRDJJ
WCMJJVBwbGCbnbCl
gBJmTBBMchsSjgfS
NvCQGNRQQrWRpWhhvQrNLgrJqTVzzLFZsJqJZFcJTqFMJJ
ttdBllbCSdcsJVVVzVMb
CnwfwwdlnPGQgpnPvv
WQmnmBBmWRCgDpndbD
SvjsqGGqTSTdbggS
lZbFJqLjvHZrcZNHcc
gcgQhclQlntnnvBMZlwffTBLwv
JqzNpqbmpJCbbzCfNFFqfWpZvDVTqBMTrTLvrTMTrvrZTT
NbJWCpRWSFWNWJCzmRNRdchcdHcchnfncnhntcdjng
JvDwhPWPzvzPDggWlvCQgPtHgtGnHtNqLqTnHTHHnnVg
jBsfcbpdQsRcsSpRcjZjHNGtnnVncVtqTVVNVNNT
prBbsjQdSbZdSFwMzrhPMWwzwlvC
SStQfWQmJQjjhphQ
sMVTwvLTswLwwqMPnnhglbHgglPh
RDrRVrVCsrssBFWffFCZthfNFN
ZnpgzcqgmhHtfwSDwplwVV
QCrBNLdLrrnSVSrl
LQQLLLjTBjGWnNBBGLFZbGgMcsMsZzbgbZZgsZ
ZnbzbhqPbMrnDGjtQGSRVVMGpf
gFdTlNJFGVGChJCt
HgswWLNdhwTTwWsNNvzPmbvbcZbrsbrmzP
SHpDqcJvBmJgJJHgDphHmvhTddWlLsZTTdFWMMsWtthZ
GPrRCPfRfjjwffjrrnPVPsTLZFwLWMzLdTsTFzzFWT
QPfZVNfbfQnRVjCRrPJBvSJgpDSDBHNgBJpB
mjpcZcHcrqjrNmNpNmptHNHWwvCwgwCgMmQgRQsRMgwMvnCg
DfSfFSVtDSTJVfdGJMwPnMRwnRCPQgMR
DDfzDVdbTbzVDDtrtqlHptqbjbql
lsBgqLqhqqgBBvGLBwQJJHRpJCJDHMwhwz
NCVfntPnVnfrZtfdbTntnnWDDMDwwRRJmJDWRWJmHRzpJD
ZrZbtbSZfdNVfbtCrbPTndsFvFLFSqcGLBFgvgjsgcqj
gwRCCDmlZtPDdtBBPM
VLrjccJVcJTfTtLjscVBHjHPjPQQSPpBHMjBhP
rfcJrzvvctrscvzRqGRCZvmqbmwqZw
nffqBWfRfRnpBfznpWTpTlWNNbcrDcbGbPhrGVwGlllPGN
MsJQsmQLjMMFsmjbPZNcNbrbmGVhZZ
LCJsFSsJFMFpfrTBWSvqpB
vSspfvprpTNTZNTj
LlFQCtnwMqqSmCMPmMSwClCJhBDJZZJZZjBTjQNjJbdjTT
PHtwMLPqCHsfVRGvSR
HjLDQMjtRvTmfTfmtf
BddChcvvhwhqgmqTfnSmdg
NBVVZJhZVhZsJJzhJZNHrRMPbQvHrjPvPDRvRN
GgwtwntLQmNjvRGJGv
wwzWzrzrqWjRlhJrNlrj
wBWSCMPMcdnHsTHPsn
MggMDDJzdbvsjCJvWJnJ
THBGGScfHwHqHGTGTBqfSWCsnWBCbvsbshjRnZWZCn
HwltftNGtmHHGqNlGmfPrVDDgzpVplpzDpVPbP
shMdsCMpQMCZMQsZQVDJnvvPpPLSvLSLLV
RmgzGTGRmClLNCvzVn
rCrRftmftWjbTttjcdFZBcjdsFqZQFjZ
qLwvNLtLvPGqSltLLqvNSpflMfQfMpMnBBggslMpfB
VDcVzzDRRVTDgMPBBgdfQD
VVWrcHbVzrzVjHPNqSmtqJJwjLGN
nBswlBBhntTttbFVnt
JNjTHZvLHDLVVLtCCFFPLz
THvNddgZWQNDNZgjZSfGwfsBrhmfGBhcWs
cCSbPmJqwqJjgJtTzJZT
BpBTFFTQZzQgNgZv
sFRVpsfrfrLfslwTcTccwcnCcTRC
DLjLwDPjVPnWWvVWVjcqzSCJTzSzMTtSrqRqJn
bdNGgmbGHdZdHbSRqrTJzrMtTR
dhGmFffGQsWjshcVpt
TrGzZpzWhCHcPPpF
sqsJqtlqDVDJVJttgNSbsQPfcjQcBQjhFFRSPQHFRc
VgqbtllbdvNtDdLZrHWGmwdrLM
DNrqBvvZZNDDHBFJmMNbLMRctztznRsbds
hlSlPPWfQCChPtWWfjTwLzbdRcbMsbMjnRLsgzsz
PCfWwlQpTTVmmtqmVGrvmt
lLrlLRbgrjRbRjFlRrnRRdgPdWdCwdWqmWPwqScdPc
tDZLNGHsNQZDNBGHTCVSddVCPwVqVtcdWV
BhGLBNvhHDTZDjfRvbfjljbbJf
McRctHfDctZGlZZWgpcW
TTQLQqLSLqTqhdLqPzLqLwrpsGpWVGFZGrlgWZGhFllV
QNdzbQgzSvqPzqNqvnnbtfbRfDntCfMRjn
bHQvFvffjpVvPSTvVm
RDLRRDLBnVbsDsqDSb
MtGGtJJnwnLltblMFCHfQcthjNfjHCQN
FQfFldFCSRRFQSQmLVfRGwGDDGZDhDGwmmhZtthZ
cTpscpCcNzNNvCBgbPPwvZGZggbghb
TNzznnccTjTspHWNzCTpNlRRQnlfdVFVfLLJFrFJQQ
LsMMLCQQQMTJnJMnsJlGlZJNvbNVGNNSDJ
fhcBqmfvmtRftcmfVVNNVbZSSDVGZmgG
BztFRjFjBchWzhvWTHWTMCdwTdTQnH
cpddMRdHTSNRtRztbG
mWvrQnQFhFNtsSNV
vrBnlLvWvlPCCnrrmBrnQQLmTMHwHHccMgggTNjdcDwjNgCw
hlRhqRnQQHcbBHGVVgRLVmrRgrLR
sCFMTMwtzFMzTwCsswWjCsdQJmJLGfrrgLfgPmPVVVmtLP
TMDjCFMTHShBQSDB
fcpssfGWpRDnvDRWvD
LMvQlPLtLQZbQjBqZBFnCRFZRFzR
jlQrvjlbjbLffpdpdrmGwr
hbRTjRRZthvSDvDn
LGrwPfrGfLjqvBsHzBrvstBv
mpCLCqqqLcwcwCLmLmwwdWgWgVVMTbbWbRdQVNjC
QLCqzhzQDqhHsCJjTcVdScccSVgs
NMPBBfwbmwmmGnfpTJJJcTrRjRJJddpr
mflmdfBBlnPBvBNGnwlGzhDqzHZzZtCLZqqltHtL
GGsFmSmFHHGZsqhSTQjlNQNzpptD
JfVJvvJfWMPPnVVJvhlNlltDldzpjpTzQn
LgLcwJffMMLcwPVvPMvsmRqTmbGGssgGssRRqG
ScnbPhwPHPTbwCGJBDtNZZPZDsttNB
FjfQlVVWrQgfQrrWfddnWfQlqJqJMNJBZJqsZtsJjJsvDjJq
lrVngVQpmmmrlnLGLLmTzchcwhHh
QZtDJqWZtWGmhJJjvVBP
crNMSpcdNNFcrdzlrsVGVnvhmnvHbjVjjrhV
NTszNMSpwTNFzcsTlsLRQLtWwZDDfQgZhggQ
mfmdLLLqsvZzjfPgPT
hppLhBNpHGrhHnQQhGMDhPbPbWzvZzbMzZFPbvbvgz
BNGGNLGcQpCcSstSRmct
mnjsJBjBRsmFsSRqqrGfrqqtrfrN
LZDHZZzdcdQzLbcgLwGtNVrlGrMVNfrllHrH
DPcZZzdQpZPzQQDpDdcpbcPgmBBBsnWfTBmnJmCsjjsPSTjJ
jwwHjCPvLVmhmRdJvr
gTBnbFGTTQMgnTbdbhHhrplhJdlV
DTScZTgSSnGTBFzjDwtLtDDHqwwL
RZWhWWRSgBRrdMRdCmtcdtLncHndqF
bssspTjbVDQGTVGTQsJpbvLtVmFLqqHgnLmCcFnmHC
DpfQJJQpDsGzzfDNhPBlPlMBNhghPZlW
VzJrJMBntJpMnBBJMDDGDQLLQwwDpQLGLG
WSFCWNWWWhQRzvNqLTRD
CbSlSlWHghChhWlcclgcWdHgfrmbmznnffMZMztrMbrJBBBJ
gtjBNTvDQNBPlBFlPFZPdP
mCmpfJCVmMzmfsHpCWdGPWCCWZGtSFtF
zJnhJnJzbbMMpnspmmfsJpLNgqtrjQvrTbTgLgtqgqLj
CLJnZZCJLJZJgZZZJMLSTgnRdFWpFdfFWBffpqDBfjFWQdRD
slNzNzzwwPQrfWmDmqBFWlDm
zwHhsVHwQcHJCgnngMZn
wLRLLddJLdZZZjHdRwgJsjqDVsDVSVGSscsVVmDq
tvMWfnhMvnvztzzVSVqqmcSSVsDGmW
TmhhpnnFlfMzMMRZRPTJRHwdPJZB
sLsQSLvcSrbQbFGlWlCD
BhgPBqBhPwmmpPlGhZMVCFFlbZGF
nCBBTPBHdHwmfCvTLStLJSctrJtt
vwNjwvBSSNndtdBJMJsLvZsJhZpPLM
TmCQDHGTVTLhPQhpZprq
CbCmmTzfVGfFGGCNbctwNPtcSnbjww
NWQQdHdTddhGrnJjqCRggvRmhzmm
wLLVHcFFFfwtFfJzqRvmRqzRVgVz
MlwFfflbLFfbwctDplwcwFMMNNWdWNTNSWDQQnsQNWWHsrQH
QlfbQrBjBQvfDBjhlpwpqbMzwWppGWqGwG
VJNcVCJgcntgRcsZWpPDPDqzPTqqnTpL
ZCRNRNmcJZCcNNVRmVdmHfjdrlfhSrvBDSfH
MzzPjGpjpGPPjdtHBfBNBQBrbtlclV
FqCnZcgcnFsWqmVHVHlgfBrbQVVb
STmCLFZWnTsWvdvSSdvPpGcP
lNjczlDNCSRMSmlR
VbhwhgwGQgwpvQpVDpSCbmfnCPfnMnmPmmBn
VhvGTdhwQwVVVsQQshjcDZdHdqzcDdJrjjzH
DhPffCSLCPCwfPPqqwqVjHFjzljppl
TBWBRWTMRBTTBTBdbQqztzQtWqzQFqbV
sgGTBGTGmNvrGfrz
hJgqGzqQmGQMQzgGmJGhJQSvZPfppjjPnZNTTTTpjWJvWp
lbwbRsRdbdmLdrllbbDcrHwwnjvjNfPWWZwWPjPjjZnTZZpf
dDDrdlDBHbDtVFSqmBShMMVF
ddvtMZJdJTtDvgtfZJfvtWZlHpGljLRcBcjplLwGRnnLGlGc
SNbFbrCNhQbrVQCQSCVzbLjBGRGRGwTBVwpnRGcHcw
bbhrTzrbrPrSQFrTTCmFQPCJmJqvJfZZWftZmZJqDvgfJv
vWLsTNNscttvNTLTLHRgcdqBnVSZVZVWVZqdSdJwwd
jDrFMhGPbGGFFPChDGpGBdngZMzVSSwZgJSqgMSV
bjjPQjphCPprhFrCCjFPDCTmcLgLtsTlmsNsQTtscQHt
PdhqLdNccGsrNLpScBnDznjnBnzppQwpHz
ftFtMbtfRMMWTvfRgRWbWMTjjzngzCzjzmDPjjwQDCDQmm
MRWVVfWfbFWWTbFWlvvRWPJGSLSLNJcPVsqJPcLSGr
fdRbPbHmnqvrvHDz
psTdcMgjjNpllVVgjJslMdpMhrttthznDttBWttBvWcnzBvh
gJjgVNFppjgCCVNsTTCsZbFwmmmmQGZSZLfwSZZd
csDFpcpJFbccqpFqpfggJJsljhvlTvQQtjwPTmjPPjRTtNvh
ZBGLddVCSVwNThhCQjPj
ZHLLLrSHGSBzWWzHWpgwcFbJsfcgJbsrgg
QJljRQLGJSNjMjQBLLJllFznzVCFpBnnzgwngpDCnD
mHWrTmWrdZHWvdrdWrdZttsFspFVcpzcwcggzpwzwVwDVp
vfzWvqTWWtPffWHqrWTZvTNSjRjQGGPPRbJbllQbMlJS
DFnFprBLpHcSlJHRBl
dbdMMCdsVWmMPlHSSVPVJfcf
sllvhgsdLThDnhQF
BSFTWCJWFJmBJdbcgDHgfDzHbncC
MjMPNjhlslPPLjPqPqVcDttzLBHcgDggDggDbB
MZhjMsBNNMhPrNjBrMhMPZWRmFmQFRRFFFQmWFQGFQ
NFgqSSrtlNbNffffffmFFZCf
WPvTBPPnBWmdJjCsPmVd
zBwhwwTRWwhvvzTvnhCTnCnSbbqlNStNbLltDRDHRHqtDR
NgggqJTHTJscdjggNVDVRcNHGLQWqpffZQGQGqpQWpWwQZfW
vvBPBhBFrzvnzSSrrSzPMtWZZcWGwmLnWQWpQlwGwpnl
rzbSCtrSttMctvvFMvrvPvgNJDRNHDsHNJsjTjsJJsVb
HbGVfpJbmbpHLBfHbdChRDDwDRhFlMlFVDFr
QzNQqcNgtqcNMjgqtntsgswRDDRQlFDSFQPDFRDwhCFR
ngscZtsqsznnnszqTnnqHfLWLWLWZmJpWMJGLBZb
cTNmqSbTBFhBQZjq
vswHWHWzHMMttvGGwgppttRfZFZjfBnQnfQZBhwQfhRB
lglsvMHHWHsWjHMtsvHvjWvTmJbCmcCcbCTcJlTmSmPSPT
JjSBbBLppbrvZGhhhvGwZNRtNMPCqCPqRgCFRNMgjP
lWlmDsdDnszRRwdCcNcdwd
TsDwwVTWsHTmTWVzQflQJbSGhZrZZQBhvBSrJZSr
JcrncrnrcZcGtJzfrrrzqbTWTlvW
SCRSDRPSLgRDCHdjjgmdDSHqQTzlmTNNVWbqfbvQzQlbfb
gpvpHHPPLdLRCSgjpLPtpZZMtnBMZJZBZBtMZs
lQSvJllvHBPPHPHWSPQQJtDtnhbwDDwwtwfhrrVw
MgLLdsMsgpRpTLLMgFrbFnfhNbbrhtwDwDDr
MqpTpqGRLpMgBzGPSvlhSBhW
NqpNNNPzhwzzshPwRPHWRmRFQWHDQPHD
VcbcnbjbbrrbbcnbZQDHmlRSVlvZWRFm
MttmjTtMrhqwhLhtdN
dMggwDwvMdqgqqtqwHnzVnmGmGtGRrFmsJrN
SlBClclffBPfZlssrGnJnZZzzznF
ljfTPplCpLcpBBPfTBfcCTbvbDMMFhqdvqWpMWHMDMpd
llTNZlhSvqMGlZMGhGgGlttrbVVwNjDbNFJbtjjDtN
fQWCBWQBBpBCsmzPmnmddQccwJtjdVbJDtDcrrjwjbFr
QzpQCRspPPPmzfppmmBBWLhgTZZTMRwSTgLlqvMqGG
HSfnNllsHThcchcJBjJhRL
FQQdzFCrFMbdFbrJBcqprcBrcBDqcB
mtmMFJJmnTfnsHvm
nddbfrBHdvbdBBhhhnWmtLsBGQCCtpmmMGPMQP
cwVVqVNggDgjZDFspptttQrsPgpttp
wDjczVZqSFDZVlSvrfhHlJHThh
CttLqSPLqLHhhCdGGTgdlZfclNlsfglbTg
nzFJpQJWVQjFmnmpjFWzVvBbMBTBNNTfTgZlTgFMNNZb
njDDWpVWQjQnzZzjJjnRRwCqPtLLrSqhSwCPCHqG
BFmNvfFNJRrdpMLLLVldWm
jPjGGwqsndHZqJqM
GtTSbjbtJjGQsQSSbPSGbzNgCRcCTCFhvfFgFFTBhfRc
rwBvGlDrBMSzMvGVSBwMSZgnJmmJqmcTTTmVCFJRcTgF
bbjpzLhHnFTLngLJ
zdHQdQQftWfNfNtsSvrsGBPGlswrlvDw
hdnMhghHZzFnZhDCCVTTTbtVmHmWbT
PQscSNcllJwzwbGmTmWVGm
sjprJpJjJNPpJBlpdFdgMzDDDhhvhd
SMwBWSBMPSfzqzPf
gJDlrFFQlgbFgvjDjTgrQQTGlhNNLdlqLGhNPqwPNPNfGf
jvwHQQjrjrJrppjvJpDFgDgHcsZZnnsVHsmMWCnsBsVVWZ
cblRJczlcBtBRCqNfGgHfpHCVHGp
WZWsLWmSPjMdWFGHTVNgLfvHqGDT
ZdmsFmZPmnZMsWWNsNjdmmmcQlrBQnBtQJtclBrtJwBzJz
wtMNCNwNqwtMMRnVcTlFtlcnFlsl
vrvrjzjZDDDwmwwVFT
jzrHjrrHjjLBPfQPjZBZzvpSMwCNRpRwSdRCNLqSShSR
BDgnhMDCDDpjDhBDJDfMSsLSZzCFTTLzTFLzTFZS
lrqrlmqbvtvWwVRtwlmrrqNvZlZSsLTTFTFZSPzZFzzTBFzz
HRRRbVmmwqhHHHhGJBcD
MvnmMvNjvvvmNnRcvzHgzMGtzhffHwHtwt
ZBBsFPPrTgCpSSBwHrLtwbHbLLtzLf
ZBZZssBdWRJgmgJdNn
TTLChzhDnjQLTDhTQJrzSbbJHsGrGrGFGb
BfvvpflfWVlVsFFvJHcFJFrJrt
ZwMBwwZPWMMpffflqlZMRnRNQLCNhPhDDNssnRQD
"""
