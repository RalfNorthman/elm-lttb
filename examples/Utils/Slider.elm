module Utils.Slider exposing (Input, slider)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Input msg =
    { label : Input.Label msg
    , value : Int
    , msg : Int -> msg
    , min : Int
    , max : Int
    }


darkGrey : Element.Color
darkGrey =
    Element.rgb 0.3 0.3 0.3


white : Element.Color
white =
    Element.rgb 1 1 1


slider : Input msg -> Element msg
slider input =
    let
        track =
            el
                [ width fill
                , height <| px 2
                , centerY
                , Background.color darkGrey
                ]
                none

        thumb =
            Input.thumb
                [ paddingXY 20 5
                , Background.color white
                , Border.width 1
                , Border.rounded 5
                , below <|
                    el
                        [ centerX
                        , padding 5
                        , Font.size 14
                        ]
                        (text <| String.fromInt input.value)
                ]
    in
    Input.slider
        [ behindContent track
        , Font.color darkGrey
        ]
        { onChange = round >> input.msg
        , label = input.label
        , min = toFloat input.min
        , max = toFloat input.max
        , value = toFloat input.value
        , thumb = thumb
        , step = Just 1.0
        }
