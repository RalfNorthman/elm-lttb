module LTTB exposing (..)

import List.Extra


type alias Input a =
    { data : List a
    , thresold : Int
    , xGetter : a -> Float
    , yGetter : a -> Float
    }


downSample : Input a -> List a
downSample input =
    let
        phase1 : a -> List a -> List a
        phase1 first tail =
            case List.Extra.unconsLast tail of
                Nothing ->
                    []

                Just ( last, middle ) ->
                    phase2
                        ([ first ]
                            :: splitIn (input.thresold - 2) middle
                            ++ [ [ last ] ]
                        )

        phase2 : List (List a) -> List a
        phase2 bucketList =
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

            head :: tail ->
                phase1 head tail

            [] ->
                []


ceilingDivide : List a -> Int -> Int
ceilingDivide list divisor =
    (List.length list
        |> toFloat
    )
        / toFloat divisor
        |> ceiling


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
