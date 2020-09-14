module RocketMetrics exposing (..)

import LTTB
import Length exposing (Length)
import Temperature exposing (Temperature)
import Time exposing (Posix)


type alias RocketMetrics =
    { timeStamp : Posix
    , altitude : Length
    , hullTemperature : Temperature
    , errorMsg : Maybe String
    }



-- We have three datapoints in our huge dataset:


originalData : List RocketMetrics
originalData =
    [ RocketMetrics
        (Time.millisToPosix 1519866123456)
        (Length.kilometers 0)
        (Temperature.degreesCelsius 20.38)
        Nothing
    , RocketMetrics
        (Time.millisToPosix 1519866223456)
        (Length.kilometers 21.835)
        (Temperature.degreesCelsius 47.38)
        Nothing
    , RocketMetrics
        (Time.millisToPosix 1519866323456)
        (Length.kilometers 112.318)
        (Temperature.degreesCelsius 657.38)
        (Just "Oh no!")
    ]



-- That is way too many, let's downsample it to two points:


sampledOnTimeVsAltitude : List RocketMetrics
sampledOnTimeVsAltitude =
    LTTB.downsample
        { data = originalData
        , threshold = 2
        , xGetter = .timeStamp >> Time.posixToMillis >> toFloat
        , yGetter = .altitude >> Length.inKilometers
        }



-- The peaks and valleys is probably different for other parameters:


sampledOnTimeVsTemperature : List RocketMetrics
sampledOnTimeVsTemperature =
    LTTB.downsample
        { data = originalData
        , threshold = 2
        , xGetter = .timeStamp >> Time.posixToMillis >> toFloat
        , yGetter = .hullTemperature >> Temperature.inDegreesCelsius
        }


sampledOnTemperatureVsAltitude : List RocketMetrics
sampledOnTemperatureVsAltitude =
    LTTB.downsample
        { data = originalData
        , threshold = 2
        , xGetter = .hullTemperature >> Temperature.inDegreesCelsius
        , yGetter = .altitude >> Length.inKilometers
        }
