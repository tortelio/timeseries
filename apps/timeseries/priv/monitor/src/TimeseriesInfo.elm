module TimeseriesInfo exposing ( TimeseriesInfo, timeseriesInfo, viewTimeseriesInfo )

-- TODO remove if possible
import Html exposing (..)
import ViewCommon exposing (..)

type alias TimeseriesInfo =
  { name : String
  , length : Int
  }

timeseriesInfo : String -> Int -> TimeseriesInfo
timeseriesInfo name length =
  { name = name
  , length = length
  }

viewTimeseriesInfo : TimeseriesInfo -> Html message
viewTimeseriesInfo {name,length} =
  viewRow
  [ viewCell name
  , viewCell ( String.fromInt length )
  ]
