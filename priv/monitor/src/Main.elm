module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)

import Dict exposing (..)

import Http
import Json.Decode

type alias Model =
  { timeseries : Dict String Int }

type Message = GotInfo  ( Result Http.Error ( Dict String Int ) )

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
  ( { timeseries = Dict.empty }
  , downloadInfo
  )

view : Model -> Html Message
view model =
  Html.div
  [ class "items" ]
  ( List.map viewTimeseries ( Dict.toList model.timeseries ) )

viewTimeseries : ( String, Int ) -> Html Message
viewTimeseries ( name, length ) =
  Html.div
  [ class "item" ]
  ( List.map viewRowCell [ name, ( String.fromInt length ) ] )

viewRowCell : String -> Html Message
viewRowCell string =
  Html.div [ class "row-cell" ] [ Html.text string ]

update : Message -> Model -> ( Model, Cmd Message )
update message model =
  case message of
    GotInfo ( Ok info ) ->
      ( { model | timeseries = info }
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
