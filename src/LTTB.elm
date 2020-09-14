module LTTB exposing (downsample)

{-|


# Largest-Triangle-Three-Buckets Algorithm


## Downsampling data for plotting

    type alias Input a =
        { data : List a
        , threshold : Int
        , xGetter : a -> Float
        , yGetter : a -> Float
        }

@docs downsample

-}

import List.Extra


type alias Input a =
    { data : List a
    , threshold : Int
    , xGetter : a -> Float
    , yGetter : a -> Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


{-| Say we have a list of ten thousand of these:

    type alias OurRecord =
        { id : Int
        , time : Time.Posix
        , temperature : Float
        , humidity : Float
        , comment : String
        }

and we just want five hundred when plotting time on the x-axis and temperature on the y-axis, then we can downsample it with:

    LTTB.downsample
        { data = originalData
        , threshold = 500
        , xGetter = .time >> Time.posixToMillis >> toFloat
        , yGetter = .temperature
        }

which results in a `List OurRecord` of length `500` sorted by `.time`.

-}
downsample : Input a -> List a
downsample input =
    let
        x : a -> Float
        x =
            input.xGetter

        y : a -> Float
        y =
            input.yGetter

        data : List a
        data =
            List.sortBy x input.data

        triangleArea : a -> a -> Point -> Float
        triangleArea a b c =
            -- Area of a triangle with the three input points as its corners
            abs (x a * (y b - c.y) + x b * (c.y - y a) + c.x * (y a - y b)) / 2

        listAverage : List a -> Point
        listAverage list_ =
            let
                avg list acc =
                    List.sum (List.map acc list)
                        / toFloat (List.length list)
            in
            Point (avg list_ x) (avg list_ y)

        setup : a -> List a -> List a
        setup first tail =
            case List.Extra.unconsLast tail of
                Nothing ->
                    []

                Just ( last, middle ) ->
                    if input.threshold == 1 then
                        [ first ]

                    else if input.threshold == 2 then
                        [ first, last ]

                    else
                        execute
                            ([ first ]
                                :: splitIn (input.threshold - 2) middle
                                ++ [ [ last ] ]
                            )

        execute : List (List a) -> List a
        execute bucketList =
            let
                iter : a -> List a -> List a -> List (List a) -> List a
                iter previous current next rest =
                    let
                        selected =
                            List.Extra.maximumBy
                                (\j -> triangleArea previous j (listAverage next))
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
    if input.threshold <= 0 then
        []

    else if List.length data <= input.threshold then
        data

    else
        case data of
            head :: tail ->
                setup head tail

            [] ->
                []


splitIn : Int -> List a -> List (List a)
splitIn nParts list =
    if nParts == 0 then
        [ [] ]

    else if nParts < 0 then
        []

    else if nParts == 1 then
        [ list ]

    else
        let
            listLength =
                List.length list

            baseChunkSize =
                listLength // nParts

            nLargerChunks =
                remainderBy nParts listLength

            partLengths =
                List.repeat nLargerChunks (baseChunkSize + 1)
                    ++ List.repeat (listLength - nLargerChunks) baseChunkSize
        in
        List.Extra.groupsOfVarying partLengths list
