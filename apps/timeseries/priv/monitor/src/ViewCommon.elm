module ViewCommon exposing ( viewContainer, viewRow , viewCell )

import Html exposing (..)
import Html.Attributes exposing (..)

viewContainer : List ( Html message ) -> Html message
viewContainer rows =
  Html.div [ class "container" ] rows

viewRow : List ( Html message ) -> Html message
viewRow cells =
  Html.div [ class "row" ] cells

viewCell : String -> Html message
viewCell value =
  Html.div [ class "cell" ] [ Html.text value ]
