
import JsonValue exposing (..)
import JsonValue.Decode as Decode
import JsonValue.Encode as Encode
import JsonValue.View exposing (..)

import Http
import Html exposing (..)

requestAdvertList : ((Result Http.Error JsonValue) -> msg) -> Cmd msg
requestAdvertList msg =
  let
    url = "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=red-flag"
    --url = "https://my-json-server.typicode.com/typicode/demo/posts/1"
    request = Http.get url Decode.jsonValue
  in Http.send msg request

main : Program Never Model Msg
main = Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

type alias Model =
  { jsonValueViewer : Maybe JsonValueViewer
  }

type Msg
  = ReceivedJson (Result Http.Error JsonValue)
  | SetJsonViewer JsonValueViewer


init : (Model, Cmd Msg)
init =
  ( { jsonValueViewer = Nothing }
  , requestAdvertList ReceivedJson
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ReceivedJson (Ok jsonValue) -> ({model | jsonValueViewer = Just <| fromJsonValue jsonValue}, Cmd.none)
    ReceivedJson (Err err) -> Debug.crash <| toString err
    SetJsonViewer newViewer -> ({model | jsonValueViewer = Just newViewer}, Cmd.none)

view : Model -> Html Msg
view model =
  case model.jsonValueViewer of
    Just jsonValueViewer ->
      div []
        [ Html.map SetJsonViewer (viewEditJson jsonValueViewer)
        , br [] []
        , br [] []
        , text <| toString <| Encode.jsonValue <| toJsonValue jsonValueViewer
        ]
    Nothing -> div [] []

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
