module MadridAir exposing (main)

import Browser
import Browser.Dom exposing (Error, Viewport, getViewport)
import Browser.Events
import Color exposing (toRgba)
import DateFormat.Language
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import LTTB
import LineChart
import LineChart.Colors as Colors
import LineChart.Coordinate
import LineChart.Dots as Dots
import Maybe.Extra
import Task exposing (Task)
import Time exposing (Month(..), Posix, millisToPosix, posixToMillis)
import Time.Extra
import Utils.Data
import Utils.Slider
import ZoomPlot as Plot


type alias Record =
    { date : Posix
    , ben : Maybe Float
    , ch4 : Maybe Float
    , co : Maybe Float
    , ebe : Maybe Float
    , nmhc : Maybe Float
    , no : Maybe Float
    , no_2 : Maybe Float
    , nox : Maybe Float
    , o_3 : Maybe Float
    , pm10 : Maybe Float
    , pm25 : Maybe Float
    , so_2 : Maybe Float
    , tch : Maybe Float
    , tol : Maybe Float
    , station : Int
    }


type alias Point =
    { x : Float, y : Float }


myFilter : (Record -> Maybe Float) -> Int -> List Record
myFilter acc day =
    let
        from : Posix
        from =
            Time.Extra.Parts 2018 Mar 1 13 0 0 0
                |> Time.Extra.partsToPosix Time.utc

        to : Posix
        to =
            Time.Extra.Parts 2018 Apr day 2 0 0 0
                |> Time.Extra.partsToPosix Time.utc

        between : Posix -> Posix -> Posix -> Bool
        between a b x =
            (posixToMillis x > posixToMillis a)
                && (posixToMillis x < posixToMillis b)
    in
    Utils.Data.records
        |> List.filter
            (\r -> acc r |> Maybe.Extra.isJust)
        |> List.filter
            (\r -> between from to r.date)


recordsNO : List Record
recordsNO =
    myFilter .no 15


downsampledRecordsNO : Int -> List Record
downsampledRecordsNO threshold =
    LTTB.downsample
        { data = recordsNO
        , threshold = threshold
        , xGetter = .date >> posixToMillis >> toFloat
        , yGetter = .no >> Maybe.withDefault 0
        }


recordsSO2 : List Record
recordsSO2 =
    --    myFilter .so_2 30
    myFilter .so_2 120


downsampledRecordsSO2 : Int -> List Record
downsampledRecordsSO2 threshold =
    LTTB.downsample
        { data = recordsSO2
        , threshold = threshold
        , xGetter = .date >> posixToMillis >> toFloat
        , yGetter = .so_2 >> Maybe.withDefault 0
        }



---- MODEL ----


type alias Model =
    { plotNO : Plot.State Record
    , plotSO2 : Plot.State Record
    , threshold : Int
    }


init : ( Model, Cmd msg )
init =
    ( { plotNO = Plot.init
      , plotSO2 = Plot.init
      , threshold = 100
      }
    , Cmd.none
    )



---- UPDATE ----


type MyPlot
    = PlotNO
    | PlotSO2


type Msg
    = ToPlot MyPlot (Plot.Msg Record)
    | ThresholdSlider Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToPlot PlotNO plotMsg ->
            ( { model | plotNO = Plot.update plotMsg model.plotNO }, Cmd.none )

        ToPlot PlotSO2 plotMsg ->
            ( { model | plotSO2 = Plot.update plotMsg model.plotSO2 }, Cmd.none )

        ThresholdSlider value ->
            ( { model | threshold = value }, Cmd.none )



---- VIEW ----


myPlotWidth =
    1000


myPlotHeight =
    600


plotNO : Model -> Element Msg
plotNO model =
    Plot.custom
        { lines =
            [ LineChart.line
                Colors.purple
                Dots.circle
                ""
                recordsNO
            , LineChart.line
                Colors.blue
                Dots.circle
                ""
                (downsampledRecordsNO model.threshold)
            ]
        , toMsg = ToPlot PlotNO
        , xAcc = .date >> posixToMillis >> toFloat
        , yAcc = .no >> Maybe.withDefault 0
        , pointDecoder = pointDecoderNO
        }
        |> Plot.width myPlotWidth
        |> Plot.height myPlotHeight
        |> Plot.xIsTime True
        |> Plot.marginLeft 70
        |> Plot.marginRight 40
        |> Plot.yAxisLabel "NO [μg/m³]"
        |> Plot.yAxisLabelOffsetX 30
        |> Plot.yAxisLabelOffsetY -15
        |> Plot.draw model.plotNO


pointDecoderNO : Point -> Record
pointDecoderNO { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0


plotSO2 : Model -> Element Msg
plotSO2 model =
    Plot.custom
        { lines =
            [ LineChart.line
                Colors.purple
                Dots.circle
                ""
                recordsSO2
            , LineChart.line
                Colors.blue
                Dots.circle
                ""
                (downsampledRecordsSO2 model.threshold)
            ]
        , toMsg = ToPlot PlotSO2
        , xAcc = .date >> posixToMillis >> toFloat
        , yAcc = .so_2 >> Maybe.withDefault 0
        , pointDecoder = pointDecoderSO2
        }
        |> Plot.width myPlotWidth
        |> Plot.height myPlotHeight
        |> Plot.xIsTime True
        |> Plot.marginLeft 70
        |> Plot.marginRight 40
        |> Plot.marginTop 50
        |> Plot.yAxisLabel "SO₂ [μg/m³]"
        |> Plot.yAxisLabelOffsetX 35
        |> Plot.yAxisLabelOffsetY -20
        |> Plot.draw model.plotSO2


pointDecoderSO2 : Point -> Record
pointDecoderSO2 { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing 0


thresholdSlider : Model -> Element Msg
thresholdSlider model =
    Utils.Slider.slider
        { label =
            Input.labelAbove
                [ Font.color darkGrey ]
            <|
                paragraph []
                    [ el [ Font.color <| rgb255 156 39 176, Font.bold ] <| text (String.fromInt (List.length recordsNO) ++ " points")
                    , el [] <| text " downsampled to "
                    , el [ Font.color <| rgb255 3 169 244, Font.bold ] <| text (String.fromInt model.threshold ++ " points:")
                    ]
        , value = model.threshold
        , msg = ThresholdSlider
        , min = 3
        , max = 300
        }


packageLink =
    newTabLink
        [ centerX
        , padding 20
        , Font.color darkGrey
        ]
        { url = "https://package.elm-lang.org/packages/RalfNorthman/elm-lttb/latest/"
        , label = text "RalfNorthman/elm-lttb"
        }


titleLink =
    newTabLink [ centerX, Font.size 24, Font.bold ]
        { label = text "Largest-Triangle-Three-Buckets Algorithm"
        , url = "https://skemman.is/bitstream/1946/15343/3/SS_MSthesis.pdf"
        }


darkGrey : Element.Color
darkGrey =
    Element.rgb 0.3 0.3 0.3


view : Model -> Html Msg
view model =
    layout
        [ padding 20
        , Font.color darkGrey
        ]
    <|
        column
            [ spacing 40
            , width fill
            , height fill
            ]
            [ titleLink
            , el [ centerX ] <| plotNO model
            , el [ centerX, width <| px 800 ] <| thresholdSlider model
            , packageLink
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
