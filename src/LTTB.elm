module LTTB exposing (downSample)

import List.Extra


type alias Input a =
    { data : List a
    , thresold : Int
    , xGetter : a -> Float
    , yGetter : a -> Float
    }


downSample : Input a -> List a
downSample input =
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


splitIn : Int -> List a -> List (List a)
splitIn nParts list_ =
    let
        partLength =
            (List.length list_
                |> toFloat
            )
                / toFloat nParts
                |> ceiling

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
    [ list_ ]


phase1 : a -> List a -> List a
phase1 first tail =
    case List.Extra.unconsLast tail of
        Nothing ->
            []

        Just ( last, middle ) ->
            phase2 first middle last


phase2 : a -> List a -> a -> List a
phase2 first middle last =
    middle
