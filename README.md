# Largest-Triangle-Three-Buckets Algorithm

A downsampling algorithm for plotting by [**Sveinn Steinarsson**](https://skemman.is/bitstream/1946/15343/3/SS_MSthesis.pdf) which is both efficient and produces good results for most cases.

When our dataset has more datapoints than the pixel width of our line chart it is often a good idea to just plot some of the points. But how do we choose which points to keep? Do we just take every tenth?

We want to keep as many significant features, peaks and valleys, of the original data as possible. But we don't want to spend too much of our computational resources to do it. Here the Largest-Triangle-Three-Buckets algorithm comes to the rescue!

In many cases we want to be doing our downsampling on the backend, e.g. in or on the way from the database. But sometimes it makes sense to do it in the frontend. When we, for whatever reason, already have our (too large for plotting) dataset in our elm environment, it it nice to be able to downsample it then and there.

Possible use cases for **elm-lttb** include:

- Downsampling datasets parsed in the frontend from a user-supplied csv.
- Resample our dataset when we zoom in (original resolution furthest in).
- Plotting many tiny charts, so called sparklines.

### Demo:

https://ralfnorthman.github.io/elm-lttb/

### Algorithm Thesis:

https://skemman.is/bitstream/1946/15343/3/SS_MSthesis.pdf

## Code Sample:

### Guest stars: [elm/time](https://package.elm-lang.org/packages/elm/time/latest/) and [ianmackenzie/elm-units](https://package.elm-lang.org/packages/ianmackenzie/elm-units/latest/)

```elm
import LTTB
import Length exposing (Length)
import Temperature exposing (Temperature)
import Time exposing (Posix)


type alias RocketMetrics =
    { timeStamp : Posix
    , altitude : Length
    , hullTemperature : Temperature
    , error : Maybe String
    }
```
### We have three datapoints in our huge dataset:
```elm
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
```
### That is way too many, let's downsample it to two points:
```elm
sampledOnTimeVsAltitude : List RocketMetrics
sampledOnTimeVsAltitude =
    LTTB.downsample
        { data = originalData
        , threshold = 2
        , xGetter = .timeStamp >> Time.posixToMillis >> toFloat
        , yGetter = .altitude >> Length.inKilometers
        }
```
### The peaks and valleys of the plot is probably different for other parameters:
```elm
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
```
