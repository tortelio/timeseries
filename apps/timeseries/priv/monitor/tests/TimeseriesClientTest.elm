module TimeseriesClientTest exposing (..)

import Dict exposing (Dict, fromList)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import TimeseriesClient exposing (..)
import Time

calculations : Test
calculations =
  describe "Calculations"
    [ describe "Derivate"
      [ test "Means of list"
          ( \_ -> means [ 3, 5, 8, 9, 11 ] |> Expect.equal [ 4, 6.5, 8.5, 10 ] )
      , test "Mean of 2 number"
          ( \_ -> means [ 1, 2 ] |> Expect.equal [ 1.5 ] )
      , test "Mean of 1 numbers"
          ( \_ -> means [ 1 ] |> Expect.equal [] )
      , test "Means of empty alist"
          ( \_ -> means [] |> Expect.equal [] )

      , test "Differences of list"
          ( \_ -> differences [ -1, 5, 8, 9, 5 ] |> Expect.equal [ 6, 3, 1, -4 ] )
      , test "Differences of 2 numbers"
          ( \_ -> differences [ 0, -1.4 ] |> Expect.equal [ -1.4 ] )
      , test "Differences of 1 number"
          ( \_ -> differences [ 1 ] |> Expect.equal [] )
      , test "Differences of empty list"
          ( \_ -> differences [] |> Expect.equal [] )

      , test "Derivate of list"
          ( \_ -> let
                    points = [ Point 1 1, Point 2 4, Point 4 3 ]
                    derivated = [ Point 1.5 3, Point 3 -0.5 ]
                  in
                  derivate points |> Expect.equal derivated )
      , test "Derivate of 2 number"
          ( \_ -> let
                    points = [ Point -1 0, Point 1 4 ]
                    derivated = [ Point 0 2 ]
                  in
                  derivate points |> Expect.equal derivated )
      , test "Derivate of 1 number"
          ( \_ -> derivate [ Point 1 1 ] |> Expect.equal [] )
      , test "Derivate of empty list"
          ( \_ -> derivate [] |> Expect.equal [] )
      ]
    , describe "List from range"
        [ test "With even ends"
           ( \_ -> listFromRange 0 2 5 |> Expect.equal [ 0, 0.5, 1, 1.5, 2]  )
        , test "With uneven ends"
           ( \_ -> let
                     list = [ 0, 0.5, 1, 1.5, 2]
                   in
                   listFromRange -0.02 2.1 5 |> Expect.equal list )
        , test "With more members than third argument"
           ( \_ -> let
                     intList = List.range 0 11 -- length = 12
                     scaleDown x = ( toFloat x ) / 10
                     floatList = List.map scaleDown intList
                     calculated_list = listFromRange 0 1.1 11 -- argument = 11
                     diff x y = abs ( x - y ) 
                     diffs = List.map2 diff floatList calculated_list
                     isClose d = d < 0.001
                   in
                   List.all isClose diffs |> Expect.equal True  )
      ]
    ]

inits :Test
inits =
  describe "Init chartconfig"
    [ test "No given dimension"
        ( \_ -> initChartConfig Nothing Nothing "testName" testTimeseries
        |> Expect.equal { timeseriesName = "testName"
                        , xDim = "t"
                        , yDim = "x"
                        , current = Point 0 0
                        , last = Nothing
                        , thereIsHint = False
                        , xMin = 1
                        , xMax = 9
                        , yMin = -1
                        , yMax = 7
                        , mouseMode = Zoom
                        , derivated = False
                        , animationIdx = 0
                        , paused = False
                        , animationType = None
                        } )
    , test "First dimension is given"
       ( \_ -> let
                 config = initChartConfig ( toMaybe "x" )
                                          Nothing
                                          "testName"
                                          testTimeseries
                 stringTest = [ config.xDim, config.yDim] == [ "x", "x" ]
                 minsAndMaxs = [ config.xMin
                               , config.xMax
                               , config.yMin
                               , config.yMax
                               ]
                 floatTest = minsAndMaxs == [ -1, 7, -1, 7 ]
               in
               [ stringTest, floatTest ]
               |> Expect.equal [ True, True ] )
    , test "Second dimension is given"
       ( \_ -> let
                 config = initChartConfig Nothing
                                          ( toMaybe "y" )
                                          "testName"
                                          testTimeseries
                 stringTest = [ config.xDim, config.yDim] == [ "t", "y" ]
                 minsAndMaxs = [ config.xMin
                               , config.xMax
                               , config.yMin
                               , config.yMax
                               ]
                 floatTest = minsAndMaxs == [ 1, 9, 2, 7 ]
               in
               [ stringTest, floatTest ]
               |> Expect.equal [ True, True ] )
    , test "Both dimensions are given"
       ( \_ -> let
                 config = initChartConfig ( toMaybe "y" )
                                          ( toMaybe "x" )
                                          "testName"
                                          testTimeseries
                 stringTest = [ config.xDim, config.yDim] == [ "y", "x" ]
                 minsAndMaxs = [ config.xMin
                               , config.xMax
                               , config.yMin
                               , config.yMax
                               ]
                 floatTest = minsAndMaxs == [ 2, 7, -1, 7 ]
               in
               [ stringTest, floatTest ]
               |> Expect.equal [ True, True ] )
    ]

updates : Test
updates =
  let
    model = { timeseries =
                Dict.fromList [ ("first", [ Dict.fromList [ ("t", 1 )
                                                          , ( "x", 2 )
                                                          ]
                                          ] )
                              , ("second", [ Dict.fromList [ ("t", 100)
                                                           , ( "x", 200 )
                                                           ]
                                           ] )
                              , ("third", [ Dict.fromList [ ("tt", 2)
                                                          , ( "xx", 3 )
                                                          ]
                                          ] )
                              , ("last", testTimeseries )
                              ]
           , columns = One
           , config = [ initChartConfig Nothing
                                        Nothing
                                        "first"
                                        [ Dict.fromList [ ("t", 1)
                                                        , ( "x", 2 )
                                                        ]
                                        ]
                      ]
           , timeseriesInfo = Dict.fromList [ ( "first", 1 )
                                            , ( "second", 1 )
                                            , ( "third", 1 )
                                            , ("last", 5) ]
           , width = 100
           }
  in
  describe "Update"
    [ describe "New timeseries name"
        [ test "New dimensions"
            ( \_ -> let
                      ( newModel, _ ) = update ( NewTimeseriesName 0 "third" )
                                               model
                      mConfig = List.head newModel.config
                      config = Maybe.withDefault emptyChartConfig mConfig
                      stringTest = [ config.xDim, config.yDim ] == [ "tt", "xx" ]
                      minsAndMaxs = [ config.xMin
                                    , config.xMax
                                    , config.yMin
                                    , config.yMax
                                    ]
                      floatTest = minsAndMaxs == [ 2, 2, 3, 3 ]
                    in
                    [ stringTest, floatTest ]
                    |> Expect.equal [ True, True ] )
        , test "Old dimensions"
            ( \_ -> let
                      ( newModel, _ ) = update ( NewTimeseriesName 0 "second" )
                                               model
                      mConfig = List.head newModel.config
                      config = Maybe.withDefault emptyChartConfig mConfig
                      stringTest = [ config.xDim, config.yDim ] == [ "t", "x" ]
                      minsAndMaxs = [ config.xMin
                                    , config.xMax
                                    , config.yMin
                                    , config.yMax
                                    ]
                      floatTest = minsAndMaxs == [ 100, 100, 200, 200 ]
                    in
                    [ stringTest, floatTest ]
                    |> Expect.equal [ True, True ] )
        ]
    , test "NewXAxis"
        ( \_ -> let
                  ( newModel, _ ) = update ( NewXAxis 0 "x" ) model
                  mConfig = List.head newModel.config
                  config = Maybe.withDefault emptyChartConfig mConfig
                  stringTest = config.xDim == "x"
                  floatTest = [ config.xMin, config.xMax ] == [ 2, 2 ]
                in
                [ stringTest, floatTest ]
                |> Expect.equal [ True, True ] )
    , test "AddChart"
        ( \_ -> let
                  ( newModel, _ ) = update ( AddChart ) model
                  mChartConfig = ( List.head model.config )
                  chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                in
                newModel.config |> Expect.equal [ chartConfig, chartConfig ] )
    , test "RemoveChart"
        ( \_ -> let
                  ( newModel, _ ) = update ( RemoveChart 0 ) model
                in
                newModel.config |> Expect.equal [] )
    , test "TwoColumns"
        ( \_ -> let
                  ( newModel, _ ) = update ( TwoColumns ) model
                in
                newModel.columns |> Expect.equal Two )
    , describe "Zoom"
        [ test "ZoomIn button"
            ( \_ -> let
                      ( origModel, _ ) = update (  NewTimeseriesName 0 "last" )
                                               model
                      ( newModel, _ ) = update ( ZoomIn 0 ) origModel
                      mChartConfig = ( List.head newModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                      minsAndMaxs = [ chartConfig.xMin
                                    , chartConfig.xMax
                                    , chartConfig.yMin
                                    , chartConfig.yMax
                                    ]
                    in
                    minsAndMaxs |> Expect.equal [ 1.8, 8.2, -1 + 0.8 , 6.2 ] )
        , test "Zoom in and out negates each other"
            ( \_ -> let
                      ( origModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                                model
                      ( inModel, _ ) = update ( ZoomIn 0 ) origModel
                      ( outModel, _ ) = update ( ZoomOut 0 ) inModel
                    in
                    outModel |> Expect.equal origModel )
        , test "By hand"
            ( \_ -> let
                      ( origModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                                model
                      ( holdModel, _ ) = update ( Hold 0 ( Point 2 0 ) )
                                                origModel
                      ( moveModel, _ ) = update ( Move 0 ( Point 4 2 ) )
                                                holdModel
                      ( dropModel, _ ) = update ( Drop 0 ( Point 4 2 ) )
                                                moveModel
                      mChartConfig = ( List.head dropModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                      minsAndMaxs = [ chartConfig.xMin
                                    , chartConfig.xMax
                                    , chartConfig.yMin
                                    , chartConfig.yMax
                                    ]
                    in
                    minsAndMaxs |> Expect.equal [ 2, 4, 0 , 2 ] )
        , test "By hand too little zoom box"
            ( \_ -> let
                      ( origModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                                model
                      ( holdModel, _ ) = update ( Hold 0 ( Point 2 0 ) )
                                                origModel
                      ( moveModel, _ ) = update ( Move 0 ( Point 2.1 0.1 ) )
                                                holdModel
                      ( dropModel, _ ) = update ( Drop 0 ( Point 2.1 0.1 ) )
                                                moveModel
                      mChartConfig = ( List.head dropModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                      minsAndMaxs = [ chartConfig.xMin
                                    , chartConfig.xMax
                                    , chartConfig.yMin
                                    , chartConfig.yMax
                                    ]
                    in
                    minsAndMaxs |> Expect.equal [ 1, 9, -1, 7 ] )
        ]
    , test "Drag by hand"
        ( \_ -> let
                  ( origModel, _ ) = update (  NewTimeseriesName 0 "last" ) model
                  ( dragModel, _ ) = update ( DragMode 0 ) origModel

                  ( holdModel, _ ) = update ( Hold 0 ( Point 2 0 ) ) dragModel
                  ( moveModel, _ ) = update ( Move 0 ( Point 4 0 ) ) holdModel
                  mChartConfig = ( List.head moveModel.config )
                  chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                  minsAndMaxs = [ chartConfig.xMin
                                , chartConfig.xMax
                                , chartConfig.yMin
                                , chartConfig.yMax
                                ]
                in
                minsAndMaxs |> Expect.equal [ -1, 7, -1, 7 ] )
    , describe "Reset axis"
      [ test "After zooming by hand"
          ( \_ -> let
                    ( origModel, _ ) = update (  NewTimeseriesName 0 "last" )
                                              model
                    ( holdModel, _ ) = update ( Hold 0 ( Point 2 0 ) ) origModel
                    ( moveModel, _ ) = update ( Move 0 ( Point 4 2 ) ) holdModel
                    ( dropModel, _ ) = update ( Drop 0 ( Point 4 2 ) ) moveModel
                    ( resetModel, _ ) = update ( ResetAxis 0 ) dropModel
                    mChartConfig = ( List.head resetModel.config )
                    chartConfig = Maybe.withDefault emptyChartConfig
                                                    mChartConfig
                    minsAndMaxs = [ chartConfig.xMin
                                  , chartConfig.xMax
                                  , chartConfig.yMin
                                  , chartConfig.yMax
                                  ]
                  in
                  minsAndMaxs |> Expect.equal [ 1, 9, -1, 7 ] )
      , test "After zooming by button"
          ( \_ -> let
                    ( addModel, _ ) = update ( AddChart ) model
                    ( newtimeseriesModel, _ ) = update ( NewTimeseriesName 1 "last" )
                                                       addModel
                    ( zoomModel, _ ) = update ( ZoomIn 1 ) newtimeseriesModel
                    ( resetModel, _ ) = update ( ResetAxis 1 ) zoomModel
                    mConfigTail = List.tail resetModel.config
                    mChartConfig = List.head ( Maybe.withDefault [] mConfigTail )
                    chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                    minsAndMaxs = [ chartConfig.xMin
                                  , chartConfig.xMax
                                  , chartConfig.yMin
                                  , chartConfig.yMax
                                  ]
                  in
                  minsAndMaxs |> Expect.equal [ 1, 9, -1, 7 ] )
      , test "After dragging"
          ( \_ -> let
                    ( origModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                              model
                    ( dragModel, _ ) = update ( DragMode 0 ) origModel
                    ( holdModel, _ ) = update ( Hold 0 ( Point 2 0 ) ) dragModel
                    ( moveModel, _ ) = update ( Move 0 ( Point 4 0 ) ) holdModel
                    ( dropModel, _ ) = update ( Drop 0 ( Point 4 0 ) ) moveModel
                    ( resetModel, _ ) = update ( ResetAxis 0 ) dropModel
                    mChartConfig = ( List.head resetModel.config )
                    chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                    minsAndMaxs = [ chartConfig.xMin
                                  , chartConfig.xMax
                                  , chartConfig.yMin
                                  , chartConfig.yMax
                                  ]
                  in
                  minsAndMaxs |> Expect.equal [ 1, 9, -1, 7 ] )
      ]
    , test "Plot derivate"
        ( \_ -> let
                  ( origModel, _ ) = update (  NewTimeseriesName 0 "last" ) model
                  ( derivateModel, _ ) = update ( PlotDerivate 0 ) origModel
                  mChartConfig = ( List.head derivateModel.config )
                  chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                  minsAndMaxs = [ chartConfig.xMin
                                , chartConfig.xMax
                                , chartConfig.yMin
                                , chartConfig.yMax
                                ]
                in
                minsAndMaxs |> Expect.equal [ 1, 9, -2.5, 3 ] )
    , test "Plot original after derivate"
        ( \_ -> let
                  ( newModel, _ ) = update (  NewTimeseriesName 0 "last" ) model
                  ( derivateModel, _ ) = update ( PlotDerivate 0 ) newModel
                  ( origModel, _ ) = update ( PlotOriginal 0 ) derivateModel
                  mChartConfig = ( List.head origModel.config )
                  chartConfig = Maybe.withDefault emptyChartConfig mChartConfig
                  minsAndMaxs = [ chartConfig.xMin
                                , chartConfig.xMax
                                , chartConfig.yMin
                                , chartConfig.yMax
                                ]
                in
                minsAndMaxs |> Expect.equal [ 1, 9, -1, 7 ] )
    , describe "Tick"
        [ test "In the middle"
            ( \_ -> let
                      t = Time.millisToPosix 0
                      ( newModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                               model
                      ( pointbypointModel, _ ) = update ( StartPointByPoint 0 )
                                                        newModel
                      ( firstModel, _ ) = update ( Tick t )pointbypointModel
                      ( secondModel, _ ) = update ( Tick t ) firstModel
                      ( thirdModel, _ ) = update ( Tick t )secondModel
                      mChartConfig = ( List.head thirdModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                    in
                    chartConfig.animationIdx |> Expect.equal 2 )
        , test "Paused"
            ( \_ -> let
                      t = Time.millisToPosix 0
                      ( newModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                               model
                      ( pointbypointModel, _ ) = update ( StartPointByPoint 0 )
                                                 newModel
                      ( firstModel, _ ) = update ( Tick t ) pointbypointModel
                      ( secondModel, _ ) = update ( Tick t ) firstModel
                      ( pausedModel, _ ) = update ( PauseContinue 0 ) secondModel
                      ( thirdModel, _ ) = update ( Tick t ) pausedModel
                      mChartConfig = ( List.head thirdModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                    in
                    chartConfig.animationIdx |> Expect.equal 1 )
        , test "End"
            ( \_ -> let
                      t = Time.millisToPosix 0
                      ( newModel, _ ) = update ( NewTimeseriesName 0 "last" )
                                               model
                      ( slidingwindowModel, _ ) = update ( StartSlidingWindow 0 )
                                                         newModel
                      ( firstModel, _ ) = update ( Tick t ) slidingwindowModel
                      ( secondModel, _ ) = update ( Tick t ) firstModel
                      ( thirdModel, _ ) = update ( Tick t ) secondModel
                      ( fourthModel, _ ) = update ( Tick t ) thirdModel
                      ( fifthModel, _ ) = update ( Tick t ) fourthModel
                      mChartConfig = ( List.head thirdModel.config )
                      chartConfig = Maybe.withDefault emptyChartConfig
                                                      mChartConfig
                    in
                    chartConfig.animationIdx |> Expect.equal 2 )
        ]
    ]

pointsInChart : Test
pointsInChart =
  let
    config = initChartConfig Nothing Nothing "testName" testTimeseries
  in
  describe "Points in charts"
    [ describe "Data to point"
        [ test "Only one key in data"
            ( \_ -> let
                      list = [ ( "t", 1 ) ]
                      data = Dict.fromList list
                      point = toMaybe ( Point 1 1 )
                    in
                    dataToPoint "t" "t" data |> Expect.equal point )
        , test "Two keys in data"
            ( \_ -> let
                      list = [ ( "t", 1 ), ( "x", 100 ) ]
                      data = ( Dict.fromList list )
                      point = toMaybe ( Point 1 100 )
                    in
                    dataToPoint "t" "x" data |> Expect.equal point )
        , test "Three keys, switched"
            ( \_ -> let
                      list = [ ( "t", 1 ), ( "x", 100 ), ( "y", 0.1 ) ]
                      data = ( Dict.fromList list )
                      point = toMaybe ( Point 0.1 1 )
                    in
                    dataToPoint "y" "t" data |> Expect.equal point )
        ]
    , describe "Timeseries to list of point"
        [ test "Same dimension"
            ( \_ -> let
                      testPoints = points testTimeseries { config | xDim = "t"
                                                                  , yDim = "t"
                                                         }
                      chartPoints = [ Point 1 1
                                    , Point 3 3
                                    , Point 4 4
                                    , Point 7 7
                                    , Point 9 9
                                    ]
                    in
                    testPoints |> Expect.equal chartPoints )
        , test "Separate dimensions"
            ( \_ -> let
                      testPoints = points testTimeseries { config | xDim = "t"
                                                                  , yDim = "x"
                                                         }
                      chartPoints = [ Point 1 0
                                    , Point 3 -1
                                    , Point 4 2
                                    , Point 7 7
                                    , Point 9 2
                                    ]
                    in
                    testPoints |> Expect.equal chartPoints )
        , test "Separate dimensions, switched"
            ( \_ -> let
                      testPoints = points testTimeseries { config | xDim = "y"
                                                                  , yDim = "t"
                                                         }
                      chartPoints = [ Point 2 1
                                    , Point 3 3
                                    , Point 4 4
                                    , Point 5 7
                                    , Point 7 9
                                    ]
                    in
                    testPoints |> Expect.equal chartPoints )
        ]
    ]

testTimeseries : Timeseries
testTimeseries =
  [ Dict.fromList [ ("t", 1), ("x", 0), ("y", 2) ]
  , Dict.fromList [ ("t", 3), ("x", -1), ("y", 3) ]
  , Dict.fromList [ ("t", 4), ("x", 2), ("y", 4) ]
  , Dict.fromList [ ("t", 7), ("x", 7), ("y", 5) ]
  , Dict.fromList [ ("t", 9), ("x", 2), ("y", 7) ]
  ]
