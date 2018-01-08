module Main exposing (main)

import EditableValue exposing (EditableValue)

import Json.Encode exposing (..)
import Json.Decode exposing (decodeString)
import Html exposing (..)

type alias Model =
  { editableValue : EditableValue
  }

main : Program Never Model EditableValue
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd EditableValue)
init =
  let
    jsonString = "{\"foo\": \"foo\", \"bar\": {\"baz\":[1,2,3,4]}}"
    editableValue =
    case decodeString EditableValue.decoder jsonString of
      Ok editableValue -> editableValue
      Err err -> EditableValue.null
  in
    ( { editableValue = editableValue }
    , Cmd.none
    )

update : EditableValue -> Model -> (Model, Cmd EditableValue)
update newValue model =
  ({model | editableValue = newValue}, Cmd.none)

view : Model -> Html EditableValue
view model =
  let
    editableValue = model.editableValue
  in
    div []
      [ EditableValue.view EditableValue.config editableValue
      , br [] []
      , br [] []
      , text <| toString <| EditableValue.encode editableValue
      ]


subscriptions : Model -> Sub EditableValue
subscriptions model = Sub.none
