# Editable JSON

## About

This package allows editing JSON files with unknown structure in an Elm view. This might be necessary e.g. for managing a noSQL server with varying document structures.


## Example
```Elm

import EditableValue exposing (..)

import Http
import Html exposing (..)

type alias Model =
  { editableValue : Maybe EditableValue
  }

type Msg
  = ReceivedJson (Result Http.Error EditableValue)
  | SetEditableValue EditableValue

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

init : (Model, Cmd Msg)
init =
  let
    url = "http://echo.jsontest.com/key/value/otherkey/othervalue"
    request = Http.get url decoder
    requestAdvertList msg = Http.send msg request
  in
    ( { editableValue = Nothing }
    , requestAdvertList ReceivedJson
    )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedJson (Ok editableValue) -> ({model | editableValue = Just <| editableValue}, Cmd.none)
    ReceivedJson (Err err) -> Debug.crash <| toString err
    SetEditableValue newViewer -> ({model | editableValue = Just newViewer}, Cmd.none)

view : Model -> Html Msg
view model =
  case model.editableValue of
    Just editableValue ->
      div []
        [ Html.map SetEditableValue (editableView config editableValue)
        , br [] []
        , br [] []
        , text <| toString <| encode editableValue
        ]
    Nothing -> div [] []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
```
