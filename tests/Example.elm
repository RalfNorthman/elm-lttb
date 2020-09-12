module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import LTTB
import Test exposing (..)


suite : Test
suite =
    describe "The LTTB module"
        [ describe "LTTB.splitIn"
            [ test "List of empty list on nParts = 0" <|
                \_ ->
                    LTTB.splitIn 0 [ 3, 9, 5 ]
                        |> Expect.equal [ [] ]
            , test "Empty list on negative nParts" <|
                \_ ->
                    LTTB.splitIn -5 [ 4, 6, 23 ]
                        |> Expect.equal []
            , test "Split list in half on nParts = 2" <|
                \_ ->
                    LTTB.splitIn 2 [ 4, 6, 23, -32 ]
                        |> Expect.equal [ [ 4, 6 ], [ 23, -32 ] ]
            , test "Split odd length list in kind of half on nParts = 2" <|
                \_ ->
                    LTTB.splitIn 2 [ 4, 6, 23, -32, 0 ]
                        |> Expect.equal [ [ 4, 6, 23 ], [ -32, 0 ] ]
            , test "Split longer list in more parts" <|
                \_ ->
                    LTTB.splitIn 3 [ 1, 2, 3, 4, 11, 12, 13, 14, 21, 22 ]
                        |> Expect.equal [ [ 1, 2, 3, 4 ], [ 11, 12, 13, 14 ], [ 21, 22 ] ]
            , test "Split another long list in just 2 parts" <|
                \_ ->
                    LTTB.splitIn 2 [ 1, 2, 3, 4, 5, 6, 11, 12, 13, 14, 15 ]
                        |> Expect.equal [ [ 1, 2, 3, 4, 5, 6 ], [ 11, 12, 13, 14, 15 ] ]
            , test "Split longer list in even more parts" <|
                \_ ->
                    LTTB.splitIn 6 [ 1, 2, 11, 12, 21, 22, 31, 32, 41, 42, 51 ]
                        |> Expect.equal [ [ 1, 2 ], [ 11, 12 ], [ 21, 22 ], [ 31, 32 ], [ 41, 42 ], [ 51 ] ]
            ]
        , describe "LTTB.ceilingDivide"
            [ test "List of length 4 nParts 2 gives chunkLength 2" <|
                \_ ->
                    LTTB.ceilingDivide [ 3, 9, 5, 7 ] 2
                        |> Expect.equal 2
            , test "List of length 5 nParts 2 gives chunkLength 3" <|
                \_ ->
                    LTTB.ceilingDivide [ 3, 9, 5, 7, 4 ] 2
                        |> Expect.equal 3
            ]
        ]
