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


records : List Record
records =
    myFilter .no 15


downsampledRecords : Int -> List Record
downsampledRecords threshold =
    LTTB.downsample
        { data = records
        , threshold = threshold
        , xGetter = .date >> posixToMillis >> toFloat
        , yGetter = .no >> Maybe.withDefault 0
        }



---- MODEL ----


type alias Model =
    { plot : Plot.State Record
    , threshold : Int
    }


init : ( Model, Cmd msg )
init =
    ( { plot = Plot.init
      , threshold = 100
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = ToPlot (Plot.Msg Record)
    | ThresholdSlider Int


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        ToPlot plotMsg ->
            ( { model | plot = Plot.update plotMsg model.plot }, Cmd.none )

        ThresholdSlider value ->
            ( { model | threshold = value }, Cmd.none )



---- VIEW ----


myPlotWidth =
    1000


myPlotHeight =
    600


plot : Model -> Element Msg
plot model =
    Plot.custom
        { lines =
            [ LineChart.line
                Colors.blue
                Dots.circle
                ""
                records
            , LineChart.line
                Colors.purple
                Dots.circle
                ""
                (downsampledRecords model.threshold)
            ]
        , toMsg = ToPlot
        , xAcc = .date >> posixToMillis >> toFloat
        , yAcc = .no >> Maybe.withDefault 0
        , pointDecoder = pointDecoder
        }
        |> Plot.width myPlotWidth
        |> Plot.height myPlotHeight
        |> Plot.xIsTime True
        |> Plot.marginLeft 70
        |> Plot.marginRight 70
        |> Plot.marginTop 50
        |> Plot.yAxisLabel "NO [μg/m³]"
        |> Plot.yAxisLabelOffsetX 30
        |> Plot.yAxisLabelOffsetY -5
        |> Plot.draw model.plot


pointDecoder : Point -> Record
pointDecoder { x, y } =
    Record (x |> round |> millisToPosix) Nothing Nothing Nothing Nothing Nothing (Just y) Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0


thresholdSlider : Model -> Element Msg
thresholdSlider model =
    let
        blue =
            rgb255 3 169 244

        purple =
            rgb255 156 39 176

        colorPoints color int =
            el [ Font.color color, Font.bold ] <|
                text (String.fromInt int ++ " points")
    in
    Utils.Slider.slider
        { label =
            Input.labelAbove
                [ Font.color darkGrey ]
            <|
                paragraph []
                    [ colorPoints blue (List.length records)
                    , text " downsampled to "
                    , colorPoints purple model.threshold
                    , text ":"
                    ]
        , value = model.threshold
        , msg = ThresholdSlider
        , min = 3
        , max = 300
        }


packageLink =
    newTabLink
        []
        { url = "https://package.elm-lang.org/packages/RalfNorthman/elm-lttb/latest/"
        , label = text "RalfNorthman/elm-lttb"
        }


kaggleLink =
    newTabLink
        []
        { url = "https://www.kaggle.com/decide-soluciones/air-quality-madrid"
        , label = text "kaggle.com/air-quality-madrid"
        }


titleLink =
    newTabLink [ centerX, Font.size 24, Font.bold ]
        { label = text "Largest-Triangle-Three-Buckets Algorithm"
        , url = "https://skemman.is/bitstream/1946/15343/3/SS_MSthesis.pdf"
        }


thesisLink =
    newTabLink []
        { label = text "LTTB-thesis.pdf"
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
            , width <| px 1040
            , height fill
            ]
            [ titleLink
            , el [ centerX ] <| plot model
            , el [ Font.size 14, centerX ]
                (text "Drag a rectangle in the plot to zoom in, click to zoom out.")
            , row [ centerX, width fill ]
                [ column
                    [ centerX
                    , spacing 40
                    , alignTop
                    , width <| px 550
                    ]
                    [ el [] <| thresholdSlider model
                    ]
                , column
                    [ centerX
                    , spacing 15
                    , width fill
                    , width <| px 250
                    , Font.size 18
                    ]
                    [ el [ centerX ] thesisLink
                    , el [ centerX ] packageLink
                    , el [ centerX ] kaggleLink
                    ]
                ]
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
