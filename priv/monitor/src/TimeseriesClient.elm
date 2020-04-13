module TimeseriesClient exposing (..)

import Browser
import Browser.Dom exposing ( Viewport, getViewport )
import Browser.Events exposing ( onResize )
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, targetValue)
import Http
import Json.Decode as Json
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
import LineChart.Junk as Junk exposing (..)
import LineChart.Legends as Legends
import LineChart.Line as Line
import List.Extra exposing (unique)
import Round exposing (round)
import String
import Svg
import Svg.Attributes as SVGA
import Time
import Task

n = 3

type alias Model = { datasets : Dict String ( List ( Dict String Float ) )
                   , columns: ColumnCount
                   , config: List ChartConfig
                   , timeseriesInfo : Dict String Int
                   , width : Int
                   }

type alias Data = Dict String.String Float

type alias Point = { x : Float, y : Float }
type alias Info = { name : String, length : Int }

type alias ChartConfig = { datasetName : String
                         , xDim : String
                         , yDim : String
                         , current : Point
                         , last : Maybe Point
                         , thereIsHint : Bool
                         , xMin : Float
                         , xMax : Float
                         , yMin : Float
                         , yMax : Float
                         , mouseMode : MouseMode
                         , mouseButton : UpDown
                         , derivated : Bool
                         , animationIdx : Int
                         , paused : Bool
                         , animationType : AnimationType }


type ColumnCount = One | Two
type MouseMode = Drag | Zoom
type UpDown = Up | Down
type AnimationType = None | PointByPoint | SlidingWindow

type Msg = NewDatasetName Int String
         | NewXAxis Int String
         | NewYAxis Int String
         | AddChart
         | RemoveChart Int
         | GotInfo  ( Result Http.Error ( Dict String Int ) )
         | GotDataset  String ( Result Http.Error String )
         | OneColumn
         | TwoColumns
         | Hold Int Point
         | Move Int Point
         | Drop Int Point
         | LeaveChart Int Point
         | LeaveContainer Int Point
         | ZoomIn Int
         | ZoomOut Int
         | DragMode Int
         | ZoomMode Int
         | ResetAxis Int
         | PlotDerivate Int
         | PlotOriginal Int
         | StartSlidingWindow Int
         | StartPointByPoint Int
         | PauseContinue Int
         | StopAnimation Int
         | Tick Time.Posix
         | GetViewport Viewport
         | Resized Int

main : Program () Model Msg
main =
  Browser.element { init = init
                  , update = update
                  , view = view
                  , subscriptions = subscriptions }

cmaybe x = ( List.head [ x ] )

init : flags -> ( Model, Cmd Msg )
init _ = ( { datasets = Dict.empty
           , columns = One
           , config = [ initChartConfig Nothing Nothing "" [] ]
           , timeseriesInfo = Dict.empty
           , width = 0
           }
         , Cmd.batch [ downloadInfo, Task.perform GetViewport getViewport]
         )

initChartConfig : Maybe String -> Maybe String -> String -> List Data -> ChartConfig
initChartConfig dim1 dim2 name data =
  let
    sample_data = ( List.head data )
    dims =
      case sample_data of
        Nothing -> []
        Just sample -> Dict.keys sample
    dim_x = case dim1 of
              Just dim -> dim
              Nothing ->Maybe.withDefault "t" ( List.head dims ) -- default t needed in case of empty dataset
    dim_y = case dim2 of
              Just dim -> dim
              Nothing -> Maybe.withDefault dim_x ( List.head ( List.drop 1 dims) )
    m_dim_x_points = List.map ( Dict.get dim_x ) data
    dim_x_points = List.map (Maybe.withDefault 0) m_dim_x_points
    m_dim_y_points = List.map ( Dict.get dim_y ) data
    dim_y_points = List.map (Maybe.withDefault 0) m_dim_y_points
  in
  { datasetName = name
  , xDim = dim_x
  , yDim = dim_y
  , current = Point 0 0
  , last = Nothing
  , thereIsHint = False
  , xMin = Maybe.withDefault 0 (List.minimum dim_x_points)
  , xMax = Maybe.withDefault 1 (List.maximum dim_x_points)
  , yMin = Maybe.withDefault 0 (List.minimum dim_y_points)
  , yMax = Maybe.withDefault 1 (List.maximum dim_y_points)
  , mouseMode = Zoom
  , mouseButton = Up
  , derivated = False
  , animationIdx = 0
  , paused = False
  , animationType = None
  }

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    NewDatasetName chartIdx newDatasetName ->
      let
        change idx config = 
          if idx == chartIdx then
            let
              new_data = Maybe.withDefault [] ( Dict.get newDatasetName model.datasets )
              new_sample_data = ( List.head new_data )
              new_dims =
                case new_sample_data of
                  Nothing -> []
                  Just sample -> Dict.keys sample
            in
            case ( List.member config.xDim new_dims ) &&
                 ( List.member config.yDim new_dims ) of
              False -> initChartConfig Nothing Nothing newDatasetName new_data
              True -> initChartConfig ( cmaybe config.xDim ) ( cmaybe config.yDim ) newDatasetName new_data
          else
            config
        cmd =
          case Dict.member newDatasetName model.datasets of
            True -> Cmd.none
            False -> downloadDataset newDatasetName
      in
      ( { model | config = List.indexedMap change model.config }
      , cmd
      )
    NewXAxis chartIdx newXAxis ->
      let
        change idx config =  
          if idx == chartIdx then
            let
              mDataset = Dict.get config.datasetName model.datasets
              dataset = Maybe.withDefault [] mDataset
              data = points dataset { config | xDim = newXAxis }
              data_x = List.map .x data
            in
             { config | xDim = newXAxis
                      , xMin = Maybe.withDefault 0 (List.minimum data_x)
                      , xMax = Maybe.withDefault 1 (List.maximum data_x)
                      }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    NewYAxis chartIdx newYAxis -> 
      let
        change idx config = 
          if idx == chartIdx then
            let
              mDataset = Dict.get config.datasetName model.datasets
              dataset = Maybe.withDefault [] mDataset
              data = points dataset { config | yDim = newYAxis }
              data_y = List.map .y data
            in
            { config | yDim = newYAxis 
                     , yMin = Maybe.withDefault 0 (List.minimum data_y)
                     , yMax = Maybe.withDefault 1 (List.maximum data_y)
                      }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    AddChart ->
      let
        lastConfig = List.head ( List.reverse model.config )
        newDatasetName =
          case lastConfig of
            Nothing -> Maybe.withDefault "" ( List.head (Dict.keys model.timeseriesInfo) )
            Just config -> config.datasetName 
        newDataset = Maybe.withDefault [] ( Dict.get newDatasetName model.datasets ) 
      in
      ( { model | config = List.append model.config [ initChartConfig Nothing Nothing newDatasetName newDataset ] }
      , Cmd.none
      )
    RemoveChart idx ->
      let
        new_config = List.append ( List.take idx model.config )
                                 ( List.drop ( idx + 1 ) model.config )
      in
      ( { model | config = new_config }
      , Cmd.none
      )
    GotInfo ( Ok info ) ->
      case Dict.isEmpty model.timeseriesInfo of
        False ->
          ( { model | timeseriesInfo = info }
          , Cmd.none )
        True -> 
          case List.head ( Dict.keys info ) of
            Nothing ->
              ( { model | timeseriesInfo = info }
              , Cmd.none )
            Just datasetName ->
              update ( NewDatasetName 0 datasetName )
                     { model | timeseriesInfo = info }
    GotInfo ( Err _ ) ->
      ( model
      , Cmd.none ) -- TODO
    GotDataset name ( Ok dataString ) ->
      let
        decoder = Json.list ( Json.dict Json.float )
        decoded = Json.decodeString decoder dataString
        dataset = case decoded of
                    Ok data -> data
                    Err _ -> []
        change config =
          if config.datasetName == name then
            initChartConfig Nothing Nothing name dataset
          else
            config
      in
      ( { model | datasets = Dict.insert name dataset model.datasets
                , config =  List.map change model.config 
        }
      , Cmd.none )
    GotDataset name ( Err _ ) ->
      ( model
      , Cmd.none ) -- TODO
    OneColumn ->  ( { model | columns = One }, Cmd.none )
    TwoColumns -> ( { model | columns = Two }, Cmd.none )
    Hold chartIdx point ->
      let
        change idx config =
          if idx == chartIdx then
            { config | last = ( List.head [ point ] ) -- last = point (rest is to make it Maybe Point)
                    , mouseButton = Down
            }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    Move chartIdx point ->
      let
        change idx config =
          if idx == chartIdx then
            case config.mouseMode of
              Zoom -> { config | thereIsHint = True, current = point }
              Drag ->
                case config.last of
                  Nothing -> { config | thereIsHint = True, current = point } -- mouse up <= last == Nothing
                  Just last -> 
                    let
                      currentDistX = last.x - point.x
                      currentDistY = last.y - point.y
                    in
                    { config | thereIsHint = True
                             , xMin = config.xMin + currentDistX
                             , xMax = config.xMax + currentDistX
                             , yMin = config.yMin + currentDistY
                             , yMax = config.yMax + currentDistY }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    Drop chartIdx point ->
      let
        change idx config =
          if idx == chartIdx then
            case config.last of
              Nothing -> config
              Just last ->
                case config.mouseMode of
                  Drag -> { config | last = Nothing, mouseButton = Up }
                  Zoom ->
                    let
                      xRange = config.xMax - config.xMin
                      yRange = config.yMax - config.yMin
                      xDist = abs (last.x - point.x)
                      yDist = abs (last.y - point.y)
                    in
                    case xDist > xRange*0.05 && yDist > yRange*0.05 of
                      False -> { config | last = Nothing, mouseButton = Up }
                      True ->
                        let
                          sorted_x = List.sort [ last.x, point.x ]
                          x_from = Maybe.withDefault config.xMin (List.head sorted_x)
                          x_to = Maybe.withDefault config.xMax (List.head (List.reverse sorted_x))
                          sorted_y = List.sort [ last.y, point.y ]
                          y_from = Maybe.withDefault config.yMin (List.head sorted_y)
                          y_to = Maybe.withDefault config.yMax (List.head (List.reverse sorted_y))
                        in
                        { config | xMin = x_from
                                 , xMax = x_to
                                 , yMin = y_from
                                 , yMax = y_to
                                 , last = Nothing
                                 , mouseButton = Up
                        }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    LeaveChart chartIdx point ->
      let  
        change idx config =
          if idx == chartIdx then
            { config | thereIsHint = False }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    LeaveContainer chartIdx point ->
      let
        change idx config =
          if idx == chartIdx then
            { config | last = Nothing
                     , mouseButton = Up
            }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    ZoomIn chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            let
              xRange = config.xMax - config.xMin
              yRange = config.yMax - config.yMin
            in
            { config | xMin = config.xMin + 0.1*xRange
                     , xMax = config.xMax - 0.1*xRange
                     , yMin = config.yMin + 0.1*yRange
                     , yMax = config.yMax - 0.1*yRange
            }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    ZoomOut chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            let
              xRange = (config.xMax - config.xMin) / 0.8 -- so ZoomIn and Out negate each other
              yRange = (config.yMax - config.yMin) / 0.8
            in
            { config | xMin = config.xMin - 0.1*xRange
                     , xMax = config.xMax + 0.1*xRange
                     , yMin = config.yMin - 0.1*yRange
                     , yMax = config.yMax + 0.1*yRange
            }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )        
    DragMode chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            { config | mouseMode = Drag }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    ZoomMode chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            { config | mouseMode = Zoom }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    ResetAxis chartIdx ->
      let
        mCurrentConfig = List.head ( List.drop ( chartIdx - 1 ) model.config )
        currentConfig = Maybe.withDefault ( initChartConfig Nothing Nothing "" [] ) mCurrentConfig
        mData = Dict.get currentConfig.datasetName model.datasets
        data = Maybe.withDefault [] mData
        currentPoints = points data currentConfig
        x = List.map .x currentPoints
        y = List.map .y currentPoints
        change idx config =
          if idx == chartIdx then
            { config | xMin = Maybe.withDefault 0 (List.minimum x)
                     , xMax = Maybe.withDefault 1 (List.maximum x)
                     , yMin = Maybe.withDefault 0 (List.minimum y)
                     , yMax = Maybe.withDefault 1 (List.maximum y)
            }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    PlotDerivate chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            if config.derivated then
              config
            else
              let
                mData = Dict.get config.datasetName model.datasets
                data = Maybe.withDefault [] mData
                currentPoints = points data { config | derivated = True }
                derivatedY = List.map .y currentPoints
              in
              { config | derivated = True
                       , yMin = Maybe.withDefault 0 ( List.minimum derivatedY )
                       , yMax = Maybe.withDefault 1 ( List.maximum derivatedY )
              }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    PlotOriginal chartIdx ->
      let
        change idx config =
          if idx == chartIdx then
            if not config.derivated then
              config
            else
              let
                mData = Dict.get config.datasetName model.datasets
                data = Maybe.withDefault [] mData
                currentPoints = points data { config | derivated = False }
                currentY = List.map .y currentPoints
              in
              { config | derivated = False
                       , yMin = Maybe.withDefault 0 ( List.minimum currentY )
                       , yMax = Maybe.withDefault 1 ( List.maximum currentY )
              }
          else
            config
      in
      ( { model | config = List.indexedMap change model.config}
      , Cmd.none
      )
    StartSlidingWindow chartIdx ->
       let
        change idx config =
          if idx == chartIdx then
            { config | animationType = SlidingWindow
                     , animationIdx = -1
                     , paused = False}
          else
            config
       in
       ( { model | config = List.indexedMap change model.config}
        , Cmd.none
       )
    StartPointByPoint chartIdx ->
       let
        change idx config =
          if idx == chartIdx then
            { config | animationType = PointByPoint
                     , animationIdx = -1
                     , paused = False}
          else
            config
       in
       ( { model | config = List.indexedMap change model.config}
        , Cmd.none
       )
    PauseContinue chartIdx ->
       let
        change idx config =
          if idx == chartIdx then
            let
              mData = Dict.get config.datasetName model.datasets
              data = Maybe.withDefault [] mData
              currentPoints = points data config
              lastIdx = case config.animationType of
                          None -> 0
                          PointByPoint -> List.length currentPoints
                          SlidingWindow -> ( List.length currentPoints ) - n 
            in
            if config.animationIdx == lastIdx then
              { config | paused = False, animationIdx = -1 }
            else
              { config | paused = not config.paused }
          else
            config
       in
       ( { model | config = List.indexedMap change model.config}
        , Cmd.none
       )
    StopAnimation chartIdx ->
       let
        change idx config =
          if idx == chartIdx then
            { config | animationType = None}
          else
            config
       in
       ( { model | config = List.indexedMap change model.config}
        , Cmd.none
       )
    Tick _ ->
      let
        change config =
          let
            mData = Dict.get config.datasetName model.datasets
            data = Maybe.withDefault [] mData
            currentPoints = points data config
            lastIdx = case config.animationType of
                        None -> 0
                        PointByPoint -> List.length currentPoints
                        SlidingWindow -> ( List.length currentPoints ) - n 
            in
            if config.paused then
              config
            else
              if config.animationIdx < lastIdx then
                { config | animationIdx = config.animationIdx + 1}
              else
                { config | paused = True}
      in
      ( { model | config = List.map change model.config}
        , Cmd.none
      )
    GetViewport viewport ->
      ( { model | width = Basics.round ( viewport.scene.width * 0.9 ) }
      , Cmd.none
      )
    Resized width ->
      ( { model | width = Basics.round ( ( toFloat width ) * 0.9 ) }
      , Cmd.none
      )

view : Model -> Html Msg
view model =
  let
    currentDatasets = unique ( List.map .datasetName model.config )
    toDict datasetName =
      let
         mLength = Dict.get datasetName model.timeseriesInfo
         length =  Maybe.withDefault 0 mLength 
      in
      { name = datasetName, length = length } 
    timeseriesInfo = List.map toDict currentDatasets
  in
  Html.div []
    [ div [ class "header" ]
          [ h1 [] [ text "Timeseries Visualization" ] ]
    , div [ class "about" ]
          [ div [ id "description" ] [ text description ]
           , configuration model
           , div [ id "dataset" ]
                 [ h2 [] [ text "Datasets" ]
                 , datasetInfo timeseriesInfo
                 ]
          ]
    , charts model
    ]

configuration: Model -> Html Msg
configuration model =
  let
    dimOptions idx =
      let
        currentConfig = Maybe.withDefault ( initChartConfig Nothing Nothing "" [])
                                          ( List.head ( List.drop (idx-1) model.config ) )
        data = Maybe.withDefault [] ( Dict.get currentConfig.datasetName model.datasets )
        sample_data = List.head data
      in
      case sample_data of
        Nothing -> []
        Just sample -> Dict.keys sample
    dataOptions = Dict.keys model.timeseriesInfo
    currentChartConfig idx = chartConfig ( dimOptions idx ) dataOptions idx
  in
  div [ id "config" ] [ h2 [] [ text "Configuration" ]
                      , columnConfig model.columns
                      , div [ id "chartconfigs" ]
                            ( List.indexedMap currentChartConfig model.config )
                      , button [ onClick AddChart ]
                               [ text "Add chart" ]
                      ]

columnConfig : ColumnCount -> Html Msg
columnConfig columns =
  let
    ( one_class, two_class ) =
      case columns of
        One -> ( "active", "inactive" )
        Two -> ( "inactive", "active" )
  in
  div [ id "columnconfig" ]
    [ button [ onClick OneColumn, class one_class ] [ text "One column" ]
    , button [ onClick TwoColumns, class two_class ] [ text "Two columns" ]
    ]

chartConfig : List String -> List String -> Int -> ChartConfig -> Html Msg
chartConfig dimOptions datasetOptions idx config =
  let
    ( derivate_class, original_class ) =
      case config.derivated of
        True -> ( "active", "inactive" )
        False -> ( "inactive", "active" )
  in
  div [ class "chartconfig"]
      [ text "x axis:"
      , div [ class "select", class "short_select"] 
            [ select [ class "select", on "change" (Json.map ( NewXAxis idx ) targetValue) ]
                     (List.map (dimOption config.xDim) dimOptions)
            ]
      , text "y axis:"
      , div [ class "select", class "short_select" ]
            [ select [ class "select", on "change" (Json.map ( NewYAxis idx ) targetValue) ]
                     (List.map (dimOption config.yDim) dimOptions)
            ]
      , text "dataset:"
      , div [ class "select" ]
            [ select [ class "select", on "change" (Json.map ( NewDatasetName idx ) targetValue) ]
                     (List.map (dataOption config.datasetName) datasetOptions) ]
      , button [ onClick ( PlotOriginal idx ), class original_class ] [ text "Original" ]
      , button [ onClick ( PlotDerivate idx ), class derivate_class ] [ text "Derivate" ]
      , button [ onClick ( RemoveChart idx ) ]
               [ text "Remove chart" ]
      ]

dataOption : String -> String -> Html Msg
dataOption datasetName opt =
  option [ value opt, selected ( datasetName == opt ) ] [ text opt ]

dimOption : String -> String -> Html Msg
dimOption selectedDim opt =
  option [ value opt, selected ( opt == selectedDim ) ] [ text opt ]

datasetInfo : List Info -> Html Msg
datasetInfo info =
  let
    toTableRow infoRow =
      tr []
         [ td [] [ text infoRow.name ]
         , td [] [ text ( String.fromInt infoRow.length )]
         ]
  in
  table []
    ( List.concat [ [ thead []
                          [ th [] [text "Name"]
                          , th [] [text "Length"]
                          ]
                  ]
                  , List.map toTableRow info
                  ] )

means : List Float -> List Float
means x =
  let
    firsts = List.take ( ( List.length x) - 1 ) x
    lasts = Maybe.withDefault [] ( List.tail x )
    mean start end = ( end + start ) /2
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

derivate : List Point -> List Point
derivate data =
  let
    xx = ( List.map .x data )
    xDiffs = differences xx
    yDiffs = differences ( List.map .y data )
    der x y = y / x
    derivated = List.map2 der xDiffs yDiffs
    toPoint x y = Point x y
  in
  List.map2 toPoint ( means xx ) derivated

charts : Model -> Html Msg
charts model =
  let
    width = case model.columns of
              One -> model.width
              Two -> model.width // 2 -- integer division
    currentChart = chart model.datasets width ( model.columns == Two )
    columns =
      case model.columns of
        One -> "one_column"
        Two -> "two_columns"
  in
  div [ id "charts", class columns ]
      ( List.indexedMap currentChart model.config )

chart : Dict String ( List ( Dict String Float ) )
     -> Int
     -> Bool
     -> Int
     -> ChartConfig
     -> Svg.Svg Msg
chart datasets width twoColumns chartIdx config =
  let
    dataset = Maybe.withDefault [] ( Dict.get config.datasetName datasets )
    currentPoints = points dataset config
    idx = Basics.max 0 config.animationIdx
    pointsToPlot = case config.animationType of
                     None -> currentPoints
                     PointByPoint -> List.take idx currentPoints
                     SlidingWindow -> List.drop idx (List.take (idx + n) currentPoints)
    color = if config.derivated then
              Colors.blue
            else
              Colors.strongBlue
    float =
      if twoColumns then
        if ( modBy 2 chartIdx ) == 0 then
          "float_left"
        else
          "float_right"
      else
        "float_left"
    lastIdx = case config.animationType of
                None -> 0
                PointByPoint -> List.length currentPoints
                SlidingWindow -> ( List.length currentPoints ) - n 
    pauseButtonText =
      if config.paused then
       if config.animationIdx < lastIdx then 
         "Continue"
       else
         "Start over"
      else
        "Pause"
    height = Basics.round ( ( toFloat width ) * 0.5 )
  in
  div [ class "chart", class float ]
      [ chartConfigDiv chartIdx config.mouseMode pauseButtonText config.animationType
      , LineChart.viewCustom
          { y = Axis.custom
                  { title = Title.default config.yDim
                   , variable = Just << .y
                   , pixels = height
                   , range = Range.window config.yMin config.yMax
                   , axisLine = AxisLine.custom <| \dataRange axisRange ->
                                 { color = Colors.gray
                                 , width = 2
                                 , events = []
                                 , start = config.yMin
                                 , end = config.yMax
                                 }
                   , ticks = customTicks config.yMin config.yMax 8
                   }
          , x = Axis.custom
                  { title = Title.default config.xDim
                  , variable = Just << .x
                  , pixels = width
                  , range = Range.window config.xMin config.xMax
                  , axisLine = AxisLine.custom <| \dataRange axisRange ->
                                 { color = Colors.gray
                                 , width = 2
                                 , events = []
                                 , start = config.xMin
                                 , end = config.xMax
                                 }
                  , ticks = customTicks config.xMin config.xMax 8
                  }
          , container = Container.default "line-chart"
          , interpolation = Interpolation.default
          , intersection = Intersection.default
          , legends = Legends.default
          , events = events chartIdx
          , junk = 
              Junk.custom <| \system ->
                { below = [ zoomRect config system ]
                , above = sectionBand ( width, height ) currentPoints config system
                , html = []
                }
          , grid = Grid.default
          , area = Area.default
          , line = Line.default
          , dots = Dots.default
          }
          [LineChart.line color Dots.circle config.datasetName pointsToPlot] ]

customTicks : Float -> Float -> Int -> Ticks.Config msg
customTicks start end count =
  let
    list = listFromRange start end count
    list1 = List.head list
    list2 = List.head ( Maybe.withDefault [] ( List.tail list ) )
    step = ( Maybe.withDefault 1 list2 ) - ( Maybe.withDefault 0 list1 )
    decimals = Basics.floor ( -1 * ( logBase 10 step ) + 1 ) 
    customTick number = Tick.custom
      { position = number
      , color = Colors.black
      , width = 1
      , length = 7
      , grid = True
      , direction = Tick.negative
      , label = List.head [ Junk.label Colors.black (Round.round decimals number) ]
      }
  in
  Ticks.custom <| \dataRange range ->
    List.map customTick list

listFromRange : Float -> Float -> Int -> List Float
listFromRange start end count =
  let
    decimals = toFloat ( Basics.floor ( ( logBase 10 ( end - start) ) - 1 ) )
    step = ( ( end - start ) / ( (toFloat count ) - 1 ) )
    roundedStep = (toFloat (Basics.round ( step / 10^decimals) )) * 10^decimals
    roundedStart = (toFloat (Basics.round ( start / 10^decimals) )) * 10^decimals
    range = List.range 0 ( 2 * count )
    member idx = 
      let
         x = roundedStart + ( ( toFloat idx ) * roundedStep )
      in
      (toFloat (Basics.round ( x / 10^decimals) )) * 10^decimals
    list = List.map member range
    inInterval x = start <= x && x <= end
  in
  List.filter inInterval list

chartConfigDiv : Int -> MouseMode -> String -> AnimationType -> Html Msg
chartConfigDiv chartIdx mouseMode pauseText animationType=
  let
    ( dragClass, zoomClass ) =
      case mouseMode of
        Drag -> ("active", "inactive")
        Zoom -> ("inactive", "active")
  in
  div [class "control"]
      [ div []
            [ button [ onClick ( ZoomIn chartIdx ) ]
                     [ text "Zoom in" ]
            , button [ onClick ( ZoomOut chartIdx ) ]
                     [ text "Zoom out" ]
            , button [ onClick ( ResetAxis chartIdx ) ]
                     [ text "Reset Axis" ]
            , button [ onClick ( DragMode chartIdx ), class dragClass ]
                     [ text "Drag" ]
            , button [ onClick ( ZoomMode chartIdx ), class zoomClass ]
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
                 , class pointByPointClass ]
                 [ text "Animation: point by point"]
        , button [ onClick ( StartSlidingWindow chartIdx )
                 , class slidingWindowClass ]
                 [ text "Animation: sliding window"]
        ]
  else
    div []
        [ button [ onClick ( StartPointByPoint chartIdx )
                 , class pointByPointClass ]
                 [ text "Animation: point by point"]
        , button [ onClick ( StartSlidingWindow chartIdx )
                 , class slidingWindowClass ]
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
    in
    Events.custom
      [ Events.onWithOptions "mousedown"  (options True)  (Hold idx)           Events.getData
      , Events.onWithOptions "mousemove"  (options False) (Move idx)           Events.getData
      , Events.onWithOptions "mouseup"    (options True)  (Drop idx)           Events.getData
      , Events.onWithOptions "mouseleave" (options False) (LeaveChart idx)     Events.getData
      , Events.onWithOptions "mouseleave" (options True)  (LeaveContainer idx) Events.getData
      ]

sectionBand : ( Int, Int ) -> List Point -> ChartConfig -> Coordinate.System -> List ( Svg.Svg msg )
sectionBand ( height, width ) currentPoints config system =
  if config.thereIsHint then
    let
      dist idx data_point = { idx = idx, dist = (data_point.x - config.current.x)^2 + (data_point.y - config.current.y)^2, point = data_point }
      dists = List.sortBy .dist ( List.indexedMap dist currentPoints )
      ( hinted, hintColor ) =
        case List.head dists of
            Just min ->
              case min.dist < ((config.xMax - config.xMin)*0.03)^2  of
                True -> ( min.point, Colors.strongBlue )
                False -> ( config.current, Colors.black )
            Nothing -> ( config.current, Colors.black )
      (hint_x, hint_y) = hintOfPoint config hinted
      label_x = system.x.max + (system.x.max - system.x.min)*0.05
      labelHeight = ( system.y.max - system.y.min ) / ( toFloat height ) * 50 
      label_y = Basics.min hinted.y (system.y.max - 2 * labelHeight )
      label_y_down = label_y - labelHeight
    in
    [ Junk.labelAt system
        label_x label_y
        system.x.min system.y.min
        "right"
        hintColor hint_x
    , Junk.labelAt system
        label_x label_y_down
        system.x.min system.y.min
        "right"
        hintColor hint_y
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
                Junk.rectangle system [ SVGA.fill "#b6b6b61a" ]
                               last.x config.current.x
                               last.y config.current.y


hintOfPoint : ChartConfig -> Coordinate.Point -> ( String, String )
hintOfPoint config point =
  let
    rounded_x = Round.round 2 point.x
    rounded_y = Round.round 2 point.y
  in
  ( config.xDim ++ ": " ++ rounded_x
  ,  config.yDim ++": " ++ rounded_y )

points : List ( Dict String Float ) -> ChartConfig -> List Point
points dataset config =
  let
    currentDataToPoint = dataToPoint config.xDim config.yDim
    originalPoints = List.map currentDataToPoint dataset
  in
  if config.derivated then
    derivate originalPoints
  else
    originalPoints 

dataToPoint : String -> String -> Data -> Point
dataToPoint xDim yDim dict =
  let
    x = Dict.get xDim dict
    just_x = Maybe.withDefault 0 x
    y = Dict.get yDim dict
    just_y = Maybe.withDefault 0 y
  in
  Point just_x just_y

description: String
description= "Use the configuration box to add/remove charts and to select the dimensions of the datasets. Select here whether the data should be derivated. Use the One Column/Two Columns buttons to change the layout. Use the buttons for each chart to zoom and drag in that chart. Use the buttons for each chart to animate point by point or with a sliding window."

downloadInfo : Cmd Msg
downloadInfo =
  Http.get
  { url = "/info" -- TODO
  , expect =
    Http.expectJson GotInfo ( Json.dict Json.int )
  }

downloadDataset : String -> Cmd Msg
downloadDataset name =
  Http.get
  { url = "/download/" ++ name
  , expect =
    Http.expectString ( GotDataset name )
  }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch [ Time.every 1000 Tick
            , onResize ( \w h -> Resized w )
            ]
