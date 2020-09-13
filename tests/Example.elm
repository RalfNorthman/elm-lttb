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
        ]
