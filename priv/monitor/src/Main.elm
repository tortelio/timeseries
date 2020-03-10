module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

import Dict exposing (..)

import Http
import Json.Decode

import ViewCommon exposing (..)
import TimeseriesInfo exposing (..)

type alias Model =
  { timeseries : List TimeseriesInfo }

type Message = GotInfo ( Result Http.Error ( Dict String Int ) )

main : Program () Model Message
main =
  Browser.element
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : flags -> ( Model, Cmd Message )
init _ =
  ( { timeseries = [] }
  , downloadInfo
  )

view : Model -> Html Message
view {timeseries} =
  viewContainer ( List.map viewTimeseriesInfo timeseries )

update : Message -> Model -> ( Model, Cmd Message )
update message model =
  case message of
    GotInfo ( Ok info ) ->
      let xxx = ( List.map (\( string , length) -> timeseriesInfo string length) ( Dict.toList info ) )
      in
      ( { model | timeseries = xxx }
      , Cmd.none )
    GotInfo ( Err _ ) ->
      ( model
      , Cmd.none ) -- TODO

downloadInfo : Cmd Message
downloadInfo =
  Http.get
  { url = "/info" -- TODO
  , expect =
    Http.expectJson GotInfo ( Json.Decode.dict Json.Decode.int )
  }

subscriptions : Model -> Sub Message
subscriptions model =
  Sub.none
