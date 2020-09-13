module LTTB exposing (..)

import List.Extra


type alias Input a =
    { data : List a
    , thresold : Int
    , xGetter : a -> Float
    , yGetter : a -> Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


downsample : Input a -> List a
downsample input =
    let
        x : a -> Float
        x =
            input.xGetter

        y : a -> Float
        y =
            input.yGetter

        area : a -> a -> Point -> Float
        area a b c =
            -- Area of a triangle with the three points as its corners
            abs (x a * (y b - c.y) + x b * (c.y - y a) + c.x * (y a - y b)) / 2

        avg : List a -> Point
        avg list_ =
            let
                listAverage list acc =
                    List.sum (List.map acc list)
                        / toFloat (List.length list)
            in
            Point (listAverage list_ x) (listAverage list_ y)

        phase1 : a -> List a -> List a
        phase1 first tail =
            case List.Extra.unconsLast tail of
                Nothing ->
                    []

                Just ( last, middle ) ->
                    if input.thresold == 1 then
                        [ first ]

                    else if input.thresold == 2 then
                        [ first, last ]

                    else
                        phase2
                            ([ first ]
                                :: splitIn (input.thresold - 2) middle
                                ++ [ [ last ] ]
                            )

        phase2 : List (List a) -> List a
        phase2 bucketList =
            let
                iter : a -> List a -> List a -> List (List a) -> List a
                iter previous current next rest =
                    let
                        selected =
                            List.Extra.maximumBy
                                (\j -> area previous j (avg next))
                                current
                                |> Maybe.withDefault previous
                    in
                    case rest of
                        head :: tail ->
                            selected :: iter selected next head tail

                        [] ->
                            selected :: next
            in
            case bucketList of
                [ first ] :: current :: next :: rest ->
                    first :: iter first current next rest

                _ ->
                    []
    in
    if input.thresold <= 0 then
        []

    else if List.length input.data <= input.thresold then
        input.data

    else
        case input.data of
            [ _ ] ->
                []

            [ a, _ ] ->
                [ a ]

            [ a, _, c ] ->
                if input.thresold >= 2 then
                    [ a, c ]

                else
                    [ a ]

            head :: tail ->
                phase1 head tail

            [] ->
                []


splitIn : Int -> List a -> List (List a)
splitIn nParts list_ =
    if nParts == 0 then
        [ [] ]

    else if nParts < 0 then
        []

    else
        let
            partLength =
                ceilingDivide list_ nParts

            chunks : Int -> List a -> List (List a)
            chunks chunkLength list =
                if chunkLength == 0 then
                    [ [] ]

                else if chunkLength < 0 then
                    []

                else if chunkLength < List.length list then
                    List.take chunkLength list
                        :: chunks chunkLength (List.drop chunkLength list)

                else
                    [ list ]
        in
        chunks partLength list_


ceilingDivide : List a -> Int -> Int
ceilingDivide list divisor =
    (List.length list
        |> toFloat
    )
        / toFloat divisor
        |> ceiling
