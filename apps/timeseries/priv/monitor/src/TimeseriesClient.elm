port module TimeseriesClient exposing (..)

import Browser exposing ( element )
import Browser.Dom exposing ( Viewport, getViewport )
import Browser.Events exposing ( onResize )
import Dict exposing ( Dict )
import Html exposing ( div, h1, h2, p, text, table, td, tr, th, thead,
                       button, select, option,
                       Html )
import Html.Attributes exposing ( id, class , selected, value)
import Html.Events exposing ( on, onClick, targetValue )
import Http exposing ( get, expectJson, expectString, Error )
import Json.Decode exposing ( dict, int, float, list, decodeString, map, at )
import LineChart
import LineChart.Area as Area
import LineChart.Axis as Axis
import LineChart.Axis.Intersection as Intersection
import LineChart.Axis.Line as AxisLine
import LineChart.Axis.Range as Range
import LineChart.Axis.Tick as Tick
import LineChart.Axis.Ticks as Ticks
import LineChart.Axis.Title as Title
import LineChart.Colors as Colors
import LineChart.Container as Container
import LineChart.Coordinate as Coordinate
import LineChart.Dots as Dots
import LineChart.Events as Events
import LineChart.Grid as Grid
import LineChart.Interpolation as Interpolation
import LineChart.Junk as Junk
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra exposing ( unique )
import Maybe.Extra exposing ( values )
import Round exposing ( round )
import Svg exposing ( Svg )
import Svg.Attributes exposing ( fill )
import Time exposing ( Posix, every )
import Task exposing ( perform )

-- width of sliding window in animation
n : Int
n = 8

-- ports to access the cookie
port cookieloaded : ( String -> msg ) -> Sub msg
port loadcookie : () -> Cmd msg

-- types, type aliases

-- datapoint: values of different dimensions
type alias Data = Dict String Float

-- timeseries: list of data points width the same keys
type alias Timeseries = List Data

type alias Point = { x : Float, y : Float }

type alias PointSeries = List Point

type alias Model = { timeseries : Dict String Timeseries
                   , columns: ColumnCount
                   , config: List ChartConfig
                   , timeseriesInfo : Dict String Int
                   , width : Int
                   }

type alias ChartConfig = { timeseriesName : String
                         , xDim : String
                         , yDim : String
                         , current : Point
                         -- last == Nothing means: mouse button up
                         , last : Maybe Point
                         , thereIsHint : Bool
                         , xMin : Float
                         , xMax : Float
                         , yMin : Float
                         , yMax : Float
                         , mouseMode : MouseMode
                         , derivated : Bool
                         , animationIdx : Int
                         , paused : Bool
                         , animationType : AnimationType
                         }

type ColumnCount = One | Two
type MouseMode = Drag | Zoom
type AnimationType = None | PointByPoint | SlidingWindow

type Msg
-- for communication with server
  = GotInfo ( Result Error ( Dict String Int ) )
  | GotTimeseries  String ( Result Error String )
-- for communicattion with js (subscriptions)
  | Tick Posix
  | GetViewport Viewport
  | Resized Int
  | CookieLoaded String
-- for chart config of what to plot
  | NewTimeseriesName Int String
  | NewXAxis Int String
  | NewYAxis Int String
  | AddChart
  | RemoveChart Int
  | PlotDerivate Int
  | PlotOriginal Int
  | OneColumn
  | TwoColumns
-- for chart config of how to plot
  | ZoomIn Int
  | ZoomOut Int
  | DragMode Int
  | ZoomMode Int
  | ResetAxis Int
  | StartSlidingWindow Int
  | StartPointByPoint Int
  | PauseContinue Int
  | StopAnimation Int
-- for events of charts
  | Hold Int Point
  | Move Int Point
  | Drop Int Point
  | LeaveChart Int Point
  | LeaveContainer Int Point

toMaybe : a -> Maybe a
toMaybe x = ( List.head [ x ] )

-- MAIN

main : Program () Model Msg
main =
  element { init = init
          , update = update
          , view = view
          , subscriptions = subscriptions
          }

-- initializations

init : flags -> ( Model, Cmd Msg )
init _ = ( { timeseries = Dict.empty
           , columns = One
           , config = [ emptyChartConfig ]
           , timeseriesInfo = Dict.empty
           , width = 0
           }
         , Cmd.batch [ loadcookie ()
                     , downloadInfo
                     , getWidthAndHight
                     ]
         )

initChartConfig : Maybe String
               -> Maybe String
               -> String
               -> Timeseries
               -> ChartConfig
initChartConfig mXDim mYDim name timeseries =
  let
    dims = unique ( List.concat ( List.map Dict.keys timeseries ) )

    xDim = Maybe.withDefault "" mXDim
    yDim = Maybe.withDefault "" mYDim
    initXDim =
      if mXDim == Nothing || not ( List.member xDim dims ) then
        -- default is needed in case of empty timeseries
        Maybe.withDefault "" ( List.head dims )
      else
        xDim
    initYDim =
      if mYDim == Nothing || not ( List.member yDim dims ) then
        Maybe.withDefault initXDim ( List.head ( List.drop 1 dims ) )
      else
        yDim
    mXDimValues = List.map ( Dict.get initXDim ) timeseries
    xDimValues = List.map ( Maybe.withDefault 0 ) mXDimValues
    mYDimValues = List.map ( Dict.get initYDim ) timeseries
    yDimValues = List.map (Maybe.withDefault 0 ) mYDimValues
  in
  { timeseriesName = name
  , xDim = initXDim
  , yDim = initYDim
  , current = Point 0 0
  , last = Nothing
  , thereIsHint = False
  , xMin = Maybe.withDefault 0 (List.minimum xDimValues)
  , xMax = Maybe.withDefault 1 (List.maximum xDimValues)
  , yMin = Maybe.withDefault 0 (List.minimum yDimValues)
  , yMax = Maybe.withDefault 1 (List.maximum yDimValues)
  , mouseMode = Zoom
  , derivated = False
  , animationIdx = 0
  , paused = False
  , animationType = None
  }

emptyChartConfig : ChartConfig
emptyChartConfig = initChartConfig Nothing Nothing "" []

-- UPDATE

updateConfig : Model
           -> ( Int, ChartConfig -> ChartConfig )
           -> Model
updateConfig model ( chartIdx, updateChartConfig ) =
  let
    change idx chartConfig =
      if idx == chartIdx then
        updateChartConfig chartConfig
      else
        chartConfig
  in
  { model | config = List.indexedMap change model.config }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
-- for communication with server
    GotInfo ( Ok info ) ->
      case List.head ( Dict.keys info ) of
        Nothing -> ( { model | timeseriesInfo = info }, Cmd.none )
        -- if there is at least one timeseries on the server
        Just timeseriesName ->
          let
            mFirstConfig = List.head model.config
            firstConfig = Maybe.withDefault emptyChartConfig mFirstConfig
          in
          -- if there was no cookie about inital timeseriesName
          if firstConfig.timeseriesName == "" then
            update ( NewTimeseriesName 0 timeseriesName )
                   { model | timeseriesInfo = info }
          else
            ( { model | timeseriesInfo = info }, Cmd.none )
    GotInfo ( Err _ ) -> ( model, Cmd.none )
    GotTimeseries name ( Ok timeseriesString ) ->
      let
        -- using json decode
        decoder = list ( dict float )
        decoded = decodeString (at ["events"] decoder) timeseriesString
        timeseries = case decoded of
                       Ok data -> data
                       Err _ -> []
        change config =
          -- in case it is the first downloaded timeseries
          if config.timeseriesName == name || config.timeseriesName == "" then
            initChartConfig ( toMaybe config.xDim )
                            ( toMaybe config.yDim )
                            name
                            timeseries
          else
            config
      in
      ( { model | timeseries = Dict.insert name timeseries model.timeseries
                , config =  List.map change model.config
        }
      , Cmd.none
      )
    GotTimeseries name ( Err _ ) -> ( model, Cmd.none )
-- for communicattion with js (subscriptions)
    Tick _ ->
      let
        change chartConfig =
          if chartConfig.animationType == None || chartConfig.paused then
            chartConfig
          else
            let
              mLength = Dict.get chartConfig.timeseriesName model.timeseriesInfo
              length = Maybe.withDefault 0 mLength
              currentLength =
                if chartConfig.derivated then
                  length - 1
                else
                  length
              lastIdx = case chartConfig.animationType of
                          None -> 0
                          PointByPoint -> currentLength
                          SlidingWindow -> currentLength - n
            in
            if chartConfig.animationIdx < lastIdx then
              { chartConfig | animationIdx = chartConfig.animationIdx + 1 }
            else
              { chartConfig | paused = True}
      in
      ( { model | config = List.map change model.config }, Cmd.none  )
    GetViewport viewport ->
      ( { model | width = Basics.floor viewport.scene.width }
      , Cmd.none
      )
    Resized width ->
      ( { model | width = width }
      , Cmd.none
      )
    CookieLoaded value ->
      let
        splitted = String.split "=" value
        mTimeseriesName = List.head ( List.reverse splitted )
        timeseriesName = Maybe.withDefault "" mTimeseriesName
        timeseriesNames = Dict.keys model.timeseriesInfo
        timeseriesInServer = List.isEmpty timeseriesNames ||
                             List.member timeseriesName timeseriesNames
      in
      if not ( timeseriesName == "" ) && timeseriesInServer then
        update ( NewTimeseriesName 0 timeseriesName ) model
      else
        ( model, Cmd.none )
-- for chart config of what to plot
    NewTimeseriesName chartIdx newTimeseriesName ->
      let
        timeseriesDownloaded = Dict.member newTimeseriesName model.timeseries
        updateChartConfig chartConfig =
          let
            mNewTimeseries = Dict.get newTimeseriesName model.timeseries
            newTimeseries = Maybe.withDefault [] mNewTimeseries
            newDims = unique ( List.concat ( List.map Dict.keys newTimeseries ) )
            xDimInNewDims = List.member chartConfig.xDim newDims
            yDimInNewDims = List.member chartConfig.yDim newDims
          in
          if ( xDimInNewDims && yDimInNewDims ) then
            initChartConfig ( toMaybe chartConfig.xDim )
                            ( toMaybe chartConfig.yDim )
                            newTimeseriesName
                            newTimeseries
          else
            if timeseriesDownloaded then
              initChartConfig Nothing Nothing newTimeseriesName newTimeseries
            else
              { chartConfig | timeseriesName = newTimeseriesName }
        cmd =
          if timeseriesDownloaded then
            Cmd.none
          else
            downloadTimeseries newTimeseriesName
      in
      ( updateConfig model ( chartIdx, updateChartConfig ), cmd )
    NewXAxis chartIdx newXAxis ->
      let
        updateChartConfig chartConfig =
          let
            mTimeseries = Dict.get chartConfig.timeseriesName model.timeseries
            timeseries = Maybe.withDefault [] mTimeseries
            newPoints = points timeseries { chartConfig | xDim = newXAxis }
            newX = List.map .x newPoints
          in
          { chartConfig | xDim = newXAxis
                        , xMin = Maybe.withDefault 0 ( List.minimum newX )
                        , xMax = Maybe.withDefault 1 ( List.maximum newX )
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    NewYAxis chartIdx newYAxis ->
      let
        updateChartConfig chartConfig =
          let
             mTimeseries = Dict.get chartConfig.timeseriesName model.timeseries
             timeseries = Maybe.withDefault [] mTimeseries
             newPoints = points timeseries { chartConfig | yDim = newYAxis }
             newY = List.map .y newPoints
          in
          { chartConfig | yDim = newYAxis
                        , yMin = Maybe.withDefault 0 ( List.minimum newY )
                        , yMax = Maybe.withDefault 1 ( List.maximum newY )
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    AddChart ->
      let
        lastConfig = List.head ( List.reverse model.config )
      in
      case lastConfig of
        Nothing ->
          let
            timeseriesNames = Dict.keys model.timeseriesInfo
            mNewTimeseriesName = List.head timeseriesNames
            newTimeseriesName = Maybe.withDefault "" mNewTimeseriesName
            newChartConfig = initChartConfig Nothing Nothing "" []
            newModel = { model | config = List.append model.config [ newChartConfig ] }
            idx = List.length model.config
          in
          update ( NewTimeseriesName idx newTimeseriesName ) newModel
        Just chartConfig ->
          ( { model | config = List.append model.config [ chartConfig ] }
          , Cmd.none
          )
    RemoveChart idx ->
      let
        newConfig = List.append ( List.take idx model.config )
                                ( List.drop ( idx + 1 ) model.config )
      in
      ( { model | config = newConfig }, Cmd.none )
    PlotDerivate chartIdx ->
      let
        updateChartConfig chartConfig =
          if chartConfig.derivated then
            chartConfig
          else
            let
              mTimeseries = Dict.get chartConfig.timeseriesName model.timeseries
              timeseries = Maybe.withDefault [] mTimeseries
              newPoints = points timeseries { chartConfig | derivated = True }
              derivatedY = List.map .y newPoints
              yMin = Maybe.withDefault 0 ( List.minimum derivatedY )
              yMax = Maybe.withDefault 1 ( List.maximum derivatedY )
            in
            { chartConfig | derivated = True
                          , yMin = yMin
                          , yMax = yMax
            }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    PlotOriginal chartIdx ->
      let
        updateChartConfig chartConfig =
          if chartConfig.derivated then
            let
              mTimeseries = Dict.get chartConfig.timeseriesName model.timeseries
              timeseries = Maybe.withDefault [] mTimeseries
              originalPoints = points timeseries
                                      { chartConfig | derivated = False }
              originalY = List.map .y originalPoints
              yMin = Maybe.withDefault 0 ( List.minimum originalY )
              yMax = Maybe.withDefault 1 ( List.maximum originalY )
            in
            { chartConfig | derivated = False
                          , yMin = yMin
                          , yMax = yMax
            }
          else
            chartConfig
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    OneColumn -> ( { model | columns = One }, Cmd.none )
    TwoColumns -> ( { model | columns = Two }, Cmd.none )
-- for chart config of how to plot
    ZoomIn chartIdx ->
      let
        updateChartConfig chartConfig =
          let
            xRange = chartConfig.xMax - chartConfig.xMin
            yRange = chartConfig.yMax - chartConfig.yMin
          in
          { chartConfig | xMin = chartConfig.xMin + 0.1*xRange
                        , xMax = chartConfig.xMax - 0.1*xRange
                        , yMin = chartConfig.yMin + 0.1*yRange
                        , yMax = chartConfig.yMax - 0.1*yRange
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    ZoomOut chartIdx ->
      let
        updateChartConfig chartConfig =
          let
            -- /0.8 => ZoomIn and Out negate each other
            xRange = (chartConfig.xMax - chartConfig.xMin) / 0.8
            yRange = (chartConfig.yMax - chartConfig.yMin) / 0.8
          in
          { chartConfig | xMin = chartConfig.xMin - 0.1*xRange
                        , xMax = chartConfig.xMax + 0.1*xRange
                        , yMin = chartConfig.yMin - 0.1*yRange
                        , yMax = chartConfig.yMax + 0.1*yRange
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    DragMode chartIdx ->
      let
        updateChartConfig chartConfig = { chartConfig | mouseMode = Drag }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    ZoomMode chartIdx ->
      let
        updateChartConfig chartConfig = { chartConfig | mouseMode = Zoom }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    ResetAxis chartIdx ->
      let
        updateChartConfig chartConfig =
          let
            mTimeseries = Dict.get chartConfig.timeseriesName model.timeseries
            timeseries = Maybe.withDefault [] mTimeseries
            currentPoints = points timeseries chartConfig
            currentX = List.map .x currentPoints
            currentY = List.map .y currentPoints
          in
          { chartConfig | xMin = Maybe.withDefault 0 (List.minimum currentX)
                        , xMax = Maybe.withDefault 1 (List.maximum currentX)
                        , yMin = Maybe.withDefault 0 (List.minimum currentY)
                        , yMax = Maybe.withDefault 1 (List.maximum currentY)
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    StartSlidingWindow chartIdx ->
      let
        updateChartConfig chartConfig =
          { chartConfig | animationType = SlidingWindow
                        , animationIdx = -1
                        , paused = False
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    StartPointByPoint chartIdx ->
      let
        updateChartConfig chartConfig =
          { chartConfig | animationType = PointByPoint
                        , animationIdx = -1
                        , paused = False
          }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    PauseContinue chartIdx ->
      let
        updateChartConfig chartConfig =
          let
            mLength = Dict.get chartConfig.timeseriesName model.timeseriesInfo
            length = Maybe.withDefault 0 mLength
            currentLength =
              if chartConfig.derivated then
                length - 1
              else
                length
            lastIdx = case chartConfig.animationType of
                        None -> 0
                        PointByPoint -> currentLength
                        SlidingWindow -> ( currentLength ) - n
          in
          if chartConfig.animationIdx == lastIdx then
            { chartConfig | paused = False, animationIdx = -1 }
          else
            { chartConfig | paused = not chartConfig.paused }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    StopAnimation chartIdx ->
      let
        updateChartConfig chartConfig = { chartConfig | animationType = None}
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
-- for events of charts
    Hold chartIdx point ->
      let
        updateChartConfig chartConfig = { chartConfig | last = toMaybe point }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    Move chartIdx point ->
      let
        updateChartConfig chartConfig =
          case chartConfig.mouseMode of
            Zoom -> { chartConfig | thereIsHint = True, current = point }
            Drag ->
              case chartConfig.last of
                Nothing -> { chartConfig | thereIsHint = True, current = point }
                Just last ->
                  let
                    currentDistX = last.x - point.x
                    currentDistY = last.y - point.y
                  in
                  { chartConfig | thereIsHint = True
                                , xMin = chartConfig.xMin + currentDistX
                                , xMax = chartConfig.xMax + currentDistX
                                , yMin = chartConfig.yMin + currentDistY
                                , yMax = chartConfig.yMax + currentDistY
                  }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    Drop chartIdx point ->
      let
        updateChartConfig chartConfig =
          case chartConfig.last of
            Nothing -> chartConfig
            Just last ->
              case chartConfig.mouseMode of
                Drag -> { chartConfig | last = Nothing }
                Zoom ->
                  let
                    xRange = chartConfig.xMax - chartConfig.xMin
                    yRange = chartConfig.yMax - chartConfig.yMin
                    xDist = abs (last.x - point.x)
                    yDist = abs (last.y - point.y)
                  in
                  if xDist > xRange*0.05 && yDist > yRange*0.05 then
                    let
                      xFrom = Basics.min last.x point.x
                      xTo = Basics.max last.x point.x
                      yFrom = Basics.min last.y point.y
                      yTo = Basics.max last.y point.y
                    in
                    { chartConfig | xMin = xFrom
                                  , xMax = xTo
                                  , yMin = yFrom
                                  , yMax = yTo
                                  , last = Nothing
                    }
                  else
                    { chartConfig | last = Nothing }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    LeaveChart chartIdx point ->
      let
        updateChartConfig chartConfig = { chartConfig | thereIsHint = False }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )
    LeaveContainer chartIdx point ->
      let
        updateChartConfig chartConfig = { chartConfig | last = Nothing }
      in
        ( updateConfig model ( chartIdx, updateChartConfig ), Cmd.none )

-- VIEW

view : Model -> Html Msg
view model =
  let
    isTimeseries name = name /= ""
    currentTimeseriesNames = unique ( List.map .timeseriesName model.config )
    timeseriesNames = List.filter isTimeseries currentTimeseriesNames
  in
  div []
      [ div [ id "header" ]
            [ h1 [] [ text "Timeseries Visualization" ] ]
      , div [ id "about" ]
            [ configurationView model
            , div [ id "timeseries" ]
                  [ h2 [] [ text "Timeseries" ]
                  , timeseriesInfoView timeseriesNames model.timeseriesInfo
                  ]
            ]
      , charts model
      ]

configurationView : Model -> Html Msg
configurationView model =
  let
    dimOptions idx =
      let
         mCurrentChartConfig = List.head ( List.drop idx model.config )
         currentChartConfig = Maybe.withDefault emptyChartConfig
                                                mCurrentChartConfig
         mTimeseries = Dict.get currentChartConfig.timeseriesName
                                model.timeseries
         timeseries = Maybe.withDefault [] mTimeseries
         sampleData = List.head timeseries
       in
         case sampleData of
           Nothing -> []
           Just sample -> Dict.keys sample
    timeseriesOptions = Dict.keys model.timeseriesInfo
    currentChartConfigView idx = chartConfigView ( dimOptions idx )
                                                 timeseriesOptions
                                                 idx
  in
    div [ id "config" ]
        [ h2 [] [ text "Configuration" ]
        , columnConfigView model.columns
        , div [ id "chartconfigs" ]
              ( List.indexedMap currentChartConfigView model.config )
        , button [ onClick AddChart ]
                 [ text "Add chart" ]
        ]

columnConfigView : ColumnCount -> Html Msg
columnConfigView columns =
  let
    ( oneClass, twoClass ) =
      case columns of
        One -> ( "active", "inactive" )
        Two -> ( "inactive", "active" )
  in
  div [ id "columnconfig" ]
      [ button [ onClick OneColumn, class oneClass ] [ text "One column" ]
      , button [ onClick TwoColumns, class twoClass ] [ text "Two columns" ]
      ]

chartConfigView : List String -> List String -> Int -> ChartConfig -> Html Msg
chartConfigView dimOptions timeseriesOptions idx config =
  let
    ( derivateClass, originalClass ) =
      if config.derivated then
        ( "active", "inactive" )
      else
        ( "inactive", "active" )
  in
  div [ class "chartconfig"]
      [ p [] [ text "x axis:" ]
      , div [ class "short_select"]
            [ select [ class "select"
                     , on "change"
                     ( map ( NewXAxis idx ) targetValue)
                     ]
                     ( List.map ( dimOption config.xDim ) dimOptions )
            ]
      , p [] [ text "y axis:" ]
      , div [ class "short_select" ]
            [ select [ class "select"
                     , on "change"
                     ( map ( NewYAxis idx ) targetValue )
                     ]
                     ( List.map ( dimOption config.yDim ) dimOptions )
            ]
      , p [] [ text "timeseries:" ]
      , div [ class "select" ]
            [ select [ class "select"
                     , on "change"
                     ( map ( NewTimeseriesName idx ) targetValue )
                     ]
                     ( List.map ( timeseriesOption config.timeseriesName )
                                timeseriesOptions
                     )
            ]
      , button [ onClick ( PlotOriginal idx ), class originalClass ]
               [ text "Original" ]
      , button [ onClick ( PlotDerivate idx ), class derivateClass ]
               [ text "Derivate" ]
      , button [ onClick ( RemoveChart idx ) ]
               [ text "Remove chart" ]
      ]

timeseriesOption : String -> String -> Html Msg
timeseriesOption timeseriesName opt =
  option [ value opt, selected ( timeseriesName == opt ) ] [ text opt ]

dimOption : String -> String -> Html Msg
dimOption selectedDim opt =
  option [ value opt, selected ( opt == selectedDim ) ] [ text opt ]

timeseriesInfoView : List String -> Dict String Int -> Html Msg
timeseriesInfoView timeseriesNames info =
  let
    toTableRow timeseriesName =
      let
        mLength = Dict.get timeseriesName info
        length = Maybe.withDefault 0 mLength
      in
      tr []
         [ td [] [ text timeseriesName ]
         , td [] [ text ( String.fromInt length )]
         ]
  in
  table []
        ( List.concat [ [ thead []
                                [ th [] [text "Name"]
                                , th [] [text "Length"]
                                ]
                        ]
                      , List.map toTableRow timeseriesNames
                      ]
        )

charts : Model -> Html Msg
charts model =
  let
    width = case model.columns of
      One -> model.width
      Two -> model.width // 2 -- integer division
    smallWidth = Basics.round ( ( toFloat width ) * 0.9 )
    currentChart = chart model.timeseries smallWidth ( model.columns == Two )
  in
  div [ id "charts" ]
      ( List.indexedMap currentChart model.config )

chart : Dict String Timeseries
     -> Int
     -> Bool
     -> Int
     -> ChartConfig
     -> Svg.Svg Msg
chart timeseries width twoColumns chartIdx chartConfig =
  let
    mCurrentTimeseries = Dict.get chartConfig.timeseriesName timeseries
    currentTimeseries = Maybe.withDefault [] mCurrentTimeseries
    currentPoints = points currentTimeseries chartConfig
    idx = Basics.max 0 chartConfig.animationIdx
    pointsToPlot = case chartConfig.animationType of
                     None -> currentPoints
                     PointByPoint -> List.take idx currentPoints
                     SlidingWindow ->
                       List.drop idx ( List.take ( idx + n ) currentPoints )
    color = if chartConfig.derivated then
              Colors.blue
            else
              Colors.strongBlue
    float =
      if twoColumns && ( modBy 2 chartIdx ) /= 0 then
        "float_right"
      else
        "float_left"
    lastIdx = case chartConfig.animationType of
                None -> 0
                PointByPoint -> List.length currentPoints
                SlidingWindow -> ( List.length currentPoints ) - n
    pauseButtonText =
      if chartConfig.paused then
        if chartConfig.animationIdx < lastIdx then
          "Continue"
        else
          "Start over"
      else
        "Pause"
    height = Basics.round ( ( toFloat width ) * 0.5 )
    splittedName = String.split "-" chartConfig.timeseriesName
    name = case List.head ( List.reverse splittedName ) of
             Nothing -> ""
             Just splittedPart -> splittedPart
  in
  div [ class "chart", class float ]
      [ chartControlView chartIdx
                         chartConfig.mouseMode
                         pauseButtonText
                         chartConfig.animationType
      , LineChart.viewCustom
          { y = Axis.custom
                  { title = Title.default chartConfig.yDim
                  , variable = Just << .y
                  , pixels = height
                  , range = Range.window chartConfig.yMin chartConfig.yMax
                  , axisLine = AxisLine.custom <| \pointRange axisRange ->
                                 { color = Colors.gray
                                 , width = 2
                                 , events = []
                                 , start = chartConfig.yMin
                                 , end = chartConfig.yMax
                                 }
                  , ticks = customTicks chartConfig.yMin chartConfig.yMax 8
                  }
          , x = Axis.custom
                  { title = Title.default chartConfig.xDim
                  , variable = Just << .x
                  , pixels = width
                  , range = Range.window chartConfig.xMin chartConfig.xMax
                  , axisLine = AxisLine.custom <| \pointRange axisRange ->
                                 { color = Colors.gray
                                 , width = 2
                                 , events = []
                                 , start = chartConfig.xMin
                                 , end = chartConfig.xMax
                                 }
                  , ticks = customTicks chartConfig.xMin chartConfig.xMax 8
                  }
          , container = Container.default "line-chart"
          , interpolation = Interpolation.default
          , intersection = Intersection.default
          , legends = Legends.default
          , events = events chartIdx
          , junk =
              Junk.custom <| \system ->
                { below = [ zoomRect chartConfig system ]
                , above = hintView ( width, height )
                                   currentPoints
                                   chartConfig
                                   system
                , html = []
                }
          , grid = Grid.default
          , area = Area.default
          , line = Line.default
          , dots = Dots.custom (Dots.full 5)
          }
          [ LineChart.line color
                           Dots.circle
                           name
                           pointsToPlot
          ]
  ]

customTicks : Float -> Float -> Int -> Ticks.Config msg
customTicks start end count =
  let
    list = listFromRange start end count -- in CALCULATIONS block
    listHead = List.head list
    listEnd = List.head ( Maybe.withDefault [] ( List.tail list ) )
    step = ( Maybe.withDefault 1 listEnd ) - ( Maybe.withDefault 0 listHead )
    decimals = Basics.floor ( -1 * ( logBase 10 step ) + 1 )
    customTick number =
      Tick.custom
        { position = number
        , color = Colors.black
        , width = 1
        , length = 7
        , grid = True
        , direction = Tick.negative
        , label = List.head [ Junk.label Colors.black
                                        ( Round.round decimals number )
                            ]
        }
  in
  Ticks.custom <| \dataRange range -> List.map customTick list

chartControlView : Int -> MouseMode -> String -> AnimationType -> Html Msg
chartControlView chartIdx mouseMode pauseText animationType =
  let
    ( dragClass, zoomClass ) =
      case mouseMode of
        Drag -> ( "active", "inactive" )
        Zoom -> ( "inactive", "active" )
  in
  div [ class "control" ]
      [ div []
            [ button [ onClick ( ZoomIn chartIdx ) ]
                     [ text "Zoom in" ]
            , button [ onClick ( ZoomOut chartIdx ) ]
                     [ text "Zoom out" ]
            , button [ onClick ( ResetAxis chartIdx ) ]
                     [ text "Reset axis" ]
            , button [ onClick ( DragMode chartIdx ), class dragClass ]
                     [ text "Drag" ]
           ,  button [ onClick ( ZoomMode chartIdx ), class zoomClass ]
                     [ text "Zoom" ]
           ]
      , animationButtons animationType pauseText chartIdx
      ]

animationButtons : AnimationType -> String -> Int -> Html Msg
animationButtons animationType pauseText chartIdx =
  let
    ( pointByPointClass, slidingWindowClass ) =
      case animationType of
        None -> ("inactive", "inactive")
        PointByPoint -> ("active", "inactive")
        SlidingWindow -> ("inactive", "active")
  in
  if animationType == None then
    div []
        [ button [ onClick ( StartPointByPoint chartIdx )
                 , class pointByPointClass
                 ]
                 [ text "Animation: point by point"]
        , button [ onClick ( StartSlidingWindow chartIdx )
                 , class slidingWindowClass
                 ]
                 [ text "Animation: sliding window"]
        ]
  else
    div []
        [ button [ onClick ( StartPointByPoint chartIdx )
                 , class pointByPointClass
                 ]
                 [ text "Animation: point by point"]
        , button [ onClick ( StartSlidingWindow chartIdx )
                 , class slidingWindowClass
                 ]
                 [ text "Animation: sliding window"]
        , button [ onClick ( PauseContinue chartIdx ) ]
                 [ text pauseText ]
        , button [ onClick ( StopAnimation chartIdx ) ]
                 [ text "Stop animation" ]
        ]

events : Int -> Events.Config Point Msg
events idx =
  let
    options bool =
      { stopPropagation = True
      , preventDefault = True
      , catchOutsideChart = bool
      }
    event = Events.onWithOptions
  in
  Events.custom
    [ event "mousedown"  (options True)  (Hold idx)           Events.getData
    , event "mousemove"  (options False) (Move idx)           Events.getData
    , event "mouseup"    (options True)  (Drop idx)           Events.getData
    , event "mouseleave" (options False) (LeaveChart idx)     Events.getData
    , event "mouseleave" (options True)  (LeaveContainer idx) Events.getData
    ]

hintView : ( Int, Int )
        -> PointSeries
        -> ChartConfig
        -> Coordinate.System
        -> List ( Svg.Svg msg )
hintView ( height, width ) currentPoints config system =
  if config.thereIsHint then
    let
      distsq idx point =
        let
         xDist = point.x - config.current.x
         yDist = point.y - config.current.y
        in
        { idx = idx, distsq = xDist^2 + yDist^2, point = point }
      distsqs = List.sortBy .distsq ( List.indexedMap distsq currentPoints )
      ( hinted, hintColor ) =
        case List.head distsqs of
          Just min ->
            if min.distsq < ( ( config.xMax - config.xMin ) * 0.03 )^2 then
              ( min.point, Colors.strongBlue )
            else
              ( config.current, Colors.black )
          Nothing -> ( config.current, Colors.black )
      (xHint, yHint) = hintOfPoint config hinted
      xLabel = system.x.max + ( system.x.max - system.x.min ) * 0.05
      labelHeight = ( system.y.max - system.y.min ) / ( toFloat height ) * 50
      yLabel = Basics.min hinted.y (system.y.max - 2 * labelHeight )
      yLabelDown = yLabel - labelHeight
    in
    [ Junk.labelAt system
                   xLabel yLabel
                   system.x.min system.y.min
                   "right"
                   hintColor xHint
    , Junk.labelAt system
                   xLabel yLabelDown
                   system.x.min system.y.min
                   "right"
                   hintColor yHint
    ]
  else
    []

zoomRect : ChartConfig -> Coordinate.System -> Svg.Svg msg
zoomRect config system =
  case config.mouseMode of
    Drag -> text ""
    Zoom -> case config.last of
      Nothing -> text ""
      Just last ->
        Junk.rectangle system [ fill "#b6b6b61a" ]
                       last.x config.current.x
                       last.y config.current.y

hintOfPoint : ChartConfig -> Coordinate.Point -> ( String, String )
hintOfPoint config point =
  let
     xRounded = Round.round 2 point.x
     yRounded = Round.round 2 point.y
  in
  ( config.xDim ++ ": " ++ xRounded,  config.yDim ++": " ++ yRounded )

-- CONVERT TIMESERIES TO LIST OF POINTS

points : Timeseries -> ChartConfig -> PointSeries
points timeseries config =
  let
    currentDataToPoint = dataToPoint config.xDim config.yDim
    extractedPoints = List.map currentDataToPoint timeseries
    originalPoints = values extractedPoints
  in
  if config.derivated then
    derivate originalPoints
  else
    originalPoints

dataToPoint : String -> String -> Data -> Maybe Point
dataToPoint xDim yDim dict =
  let
    x = Dict.get xDim dict
    justX = Maybe.withDefault 0 x
    y = Dict.get yDim dict
    justY = Maybe.withDefault 0 y
    currentKeys = Dict.keys dict
  in
  if ( List.member xDim currentKeys ) && ( List.member yDim currentKeys ) then
    toMaybe ( Point justX justY )
  else
    Nothing

-- COMMUNICATION

downloadInfo : Cmd Msg
downloadInfo =
  Http.get
    { url = "/summary"
    , expect = expectJson GotInfo ( dict int )
    }

downloadTimeseries : String -> Cmd Msg
downloadTimeseries name =
  Http.get
    { url = "/download/" ++ name
    , expect = expectString ( GotTimeseries name )
    }

getWidthAndHight : Cmd Msg
getWidthAndHight = perform GetViewport getViewport

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every 1000 Tick
            , onResize ( \w h -> Resized w )
            , cookieloaded CookieLoaded
            ]

-- CALCULATIONS

means : List Float -> List Float
means x =
  let
    firsts = List.take ( ( List.length x) - 1 ) x
    lasts = Maybe.withDefault [] ( List.tail x )
    mean start end = ( end + start ) / 2
  in
  List.map2 mean firsts lasts

differences : List Float -> List Float
differences x =
  let
     firsts = List.take ( ( List.length x) - 1 ) x
     lasts = Maybe.withDefault [] ( List.tail x )
     diff start end = end - start
  in
  List.map2 diff firsts lasts

derivate : PointSeries -> PointSeries
derivate data =
  let
    xx = List.map .x data
    yy = List.map .y data
    xDiffs = differences xx
    yDiffs = differences yy
    der x y = y / x
    derivated = List.map2 der xDiffs yDiffs
    toPoint x y = Point x y
  in
  List.map2 toPoint ( means xx ) derivated

listFromRange : Float -> Float -> Int -> List Float
listFromRange start end count =
  let
    decimals = toFloat ( Basics.floor ( ( logBase 10 ( end - start) ) - 1 ) )
    scale = 10^decimals
    step = ( ( end - start ) / ( (toFloat count ) - 1 ) )
    roundedStep = ( toFloat ( Basics.round ( step / scale ) ) ) * scale
    roundedStart = ( toFloat ( Basics.round ( start / scale ) ) ) * scale
    range = List.range 0 ( 2 * count )
    member idx =
      let
        x = roundedStart + ( ( toFloat idx ) * roundedStep )
      in
      x
      --( toFloat ( Basics.round ( x / scale ) ) ) * scale
    list = List.map member range
    inInterval x = start <= x && x <= end
  in
  List.filter inInterval list
