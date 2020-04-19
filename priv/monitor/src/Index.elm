port module Index exposing (..)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Json

port save : String -> Cmd msg

type alias Model = { timeseriesInfo : Dict String Int }

type Msg
  = GotInfo  ( Result Http.Error ( Dict String Int ) )
  | TimeseriesChosen String

main : Program () Model Msg
main =
  Browser.element { init = init
                  , update = update
                  , view = view
                  , subscriptions = subscriptions
                  }

init : flags -> ( Model, Cmd Msg )
init _ = ( { timeseriesInfo = Dict.empty } , downloadInfo )

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model = 
  case msg of
    GotInfo ( Ok info ) -> ( { model | timeseriesInfo = info } , Cmd.none )
    GotInfo ( Err _ ) -> ( model, Cmd.none )
    TimeseriesChosen chosenName ->
      ( model, Cmd.batch [ save chosenName
                         , ( Nav.load  "TimeseriesClient.html" )
                         ]
      )

view : Model -> Html Msg
view model =
  let
    toInfoDict timeseriesName =
      let
         mLength = Dict.get timeseriesName model.timeseriesInfo
         length =  Maybe.withDefault 0 mLength 
      in
      { name = timeseriesName, length = length } 
    timeseriesInfoDict = List.map toInfoDict ( Dict.keys model.timeseriesInfo )
    toTableRow infoRow =
      tr []
         [ td [] [ button [ onClick ( TimeseriesChosen infoRow.name ) ]
                          [ text infoRow.name ]
                 ]
         , td [] [ text ( String.fromInt infoRow.length ) ]
         ]
    tableContent = ( List.concat [ [ thead []
                                           [ th [] [text "Name"]
                                           , th [] [text "Length"]
                                           ]
                                   ]
                                 , List.map toTableRow timeseriesInfoDict
                                 ]
                   )

  in
  Html.div []
    [ div [ class "header" ]
          [ h1 [] [ text "Timeseries Visualization" ] ]
    , div [] [ text "Choose a timeseries!" ]
    , div [ class "table" ] [ table [] tableContent ]
    ]

downloadInfo : Cmd Msg
downloadInfo =
  Http.get
  { url = "/info"
  , expect = Http.expectJson GotInfo ( Json.dict Json.int )
  }

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
