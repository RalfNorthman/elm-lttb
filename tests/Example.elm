module Example exposing (..)

import Expect
import Fuzz exposing (Fuzzer)
import LTTB exposing (Point)
import Test exposing (..)


smallPoint : Fuzzer Point
smallPoint =
    Fuzz.map2 Point Fuzz.percentage Fuzz.percentage


point : Fuzzer Point
point =
    Fuzz.map2 Point Fuzz.float Fuzz.float


pointList : Fuzzer (List Point)
pointList =
    Fuzz.list point


thresold : Fuzzer Int
thresold =
    Fuzz.intRange 1 10


nParts : Fuzzer Int
nParts =
    Fuzz.intRange 1 5


suite : Test
suite =
    describe "The LTTB module"
        [ describe "LTTB.downsample"
            [ fuzz2 pointList thresold "Output has the right shape - fuzz version" <|
                \fuzzList fuzzInt ->
                    let
                        input =
                            { data = fuzzList
                            , thresold = fuzzInt
                            , xGetter = .x
                            , yGetter = .y
                            }

                        expectedLength =
                            min (List.length fuzzList) fuzzInt
                    in
                    LTTB.downsample input
                        |> List.length
                        |> Expect.equal expectedLength
            ]
        , test "Output has the right shape" <|
            \_ ->
                let
                    input =
                        { data =
                            [ Point 1.0 3.9
                            , Point 3.3 45.98
                            , Point 3.3 45.98
                            , Point 93.3 23.98
                            ]
                        , thresold = 3
                        , xGetter = .x
                        , yGetter = .y
                        }
                in
                LTTB.downsample input
                    |> Expect.equal
                        [ Point 1.0 3.9
                        , Point 3.3 45.98
                        , Point 93.3 23.98
                        ]
        , test "Threshold higher than list length returns list" <|
            \_ ->
                let
                    input =
                        { data =
                            [ Point 1.0 3.9
                            , Point 3.3 45.98
                            ]
                        , thresold = 3
                        , xGetter = .x
                        , yGetter = .y
                        }
                in
                LTTB.downsample input
                    |> Expect.equal
                        [ Point 1.0 3.9
                        , Point 3.3 45.98
                        ]
        , test "Threshold equal to list length returns list" <|
            \_ ->
                let
                    input =
                        { data =
                            [ Point 1.0 3.9
                            , Point 3.3 45.98
                            , Point 3.3 45.98
                            , Point 93.3 23.98
                            ]
                        , thresold = 4
                        , xGetter = .x
                        , yGetter = .y
                        }
                in
                LTTB.downsample input
                    |> Expect.equal
                        [ Point 1.0 3.9
                        , Point 3.3 45.98
                        , Point 3.3 45.98
                        , Point 93.3 23.98
                        ]
        , test "Simple triangle test" <|
            \_ ->
                let
                    input =
                        { data =
                            [ Point 0.0 0.0
                            , Point 1.0 0.0
                            , Point 2.0 -10.0
                            , Point 3.0 1.0
                            , Point 4.0 0.0
                            , Point 5.0 0.0
                            , Point 6.0 0.0
                            , Point 7.0 -1.0
                            , Point 8.0 10.0
                            , Point 9.0 0.0
                            ]
                        , thresold = 4
                        , xGetter = .x
                        , yGetter = .y
                        }
                in
                LTTB.downsample input
                    |> Expect.equal
                        [ Point 0.0 0.0
                        , Point 2.0 -10.0
                        , Point 8.0 10.0
                        , Point 9.0 0.0
                        ]
        , describe "LTTB.splitIn"
            [ fuzz2 nParts pointList "Listlength equal to nParts - Fuzz" <|
                \fuzzInt fuzzList ->
                    let
                        expectedLength =
                            min fuzzInt (List.length fuzzList)
                    in
                    LTTB.splitIn fuzzInt fuzzList
                        |> List.length
                        |> Expect.equal expectedLength
            , test "List of empty list on nParts = 0" <|
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
            , test "Split in one returns same list but inside a list" <|
                \_ ->
                    LTTB.splitIn 1 [ 1, 2, 3, 4, 11, 12, 13, 14, 21, 22 ]
                        |> Expect.equal [ [ 1, 2, 3, 4, 11, 12, 13, 14, 21, 22 ] ]
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
