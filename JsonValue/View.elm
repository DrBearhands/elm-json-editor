module JsonValue.View exposing (JsonValueViewer(..), viewJson, viewEditJson, fromJsonValue, toJsonValue)

import JsonValue exposing (..)
import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type JsonValueViewer
  = JsonVString String
  | JsonVInt Int
  | JsonVFloat Float
  | JsonVArray (Array JsonValueViewer)
  | JsonVObject (Array (String, JsonValueViewer))
  | JsonVBool Bool
  | JsonVNull

fromJsonValue : JsonValue -> JsonValueViewer
fromJsonValue value =
  case value of
    JsonString str -> JsonVString str
    JsonInt int -> JsonVInt int
    JsonFloat float -> JsonVFloat float
    JsonArray array -> JsonVArray <| Array.map fromJsonValue array
    JsonObject dict -> JsonVObject <| Array.fromList <| Dict.toList <| Dict.map (\_ -> fromJsonValue) dict
    JsonBool bool -> JsonVBool bool
    JsonNull -> JsonVNull

toJsonValue : JsonValueViewer -> JsonValue
toJsonValue viewer =
  case viewer of
    JsonVString str -> JsonString str
    JsonVInt int -> JsonInt int
    JsonVFloat float -> JsonFloat float
    JsonVArray array -> JsonArray <| Array.map toJsonValue array
    JsonVObject dict -> JsonObject <| Dict.fromList <| Array.toList <| Array.map (\(key, value) -> (key, toJsonValue value)) dict
    JsonVBool bool -> JsonBool bool
    JsonVNull -> JsonNull


viewJson : JsonValueViewer -> Html msg
viewJson jsonValue =
  case jsonValue of
    JsonVString str -> text str
    JsonVInt int -> text <| toString int
    JsonVFloat float -> text <| toString float
    JsonVArray list ->
      div []
        [ text "["
        , ul [] <| Array.toList <| Array.map (\jv -> li [] [viewJson jv]) list
        , text "]"
        ]
    JsonVObject obj ->
      div []
        [ text "{"
        , ul []
          <| List.map
            ( \(key, val) ->
               li []
                [ text <| key ++ ": "
                , viewJson val
                ]
            )
            ( Array.toList obj )
        , text "}"
        ]
    JsonVBool bool -> text <| toString bool
    JsonVNull -> text "null"

makeNullButton : Html JsonValueViewer
makeNullButton =
  button
    [ onClick JsonVNull ]
    [ text "X" ]

inlineStyle = style [("display", "inline-block")]

arrayDelete : Int -> Array a -> Array a
arrayDelete ii arr =
  Array.append
    (Array.slice 0 ii arr)
    (Array.slice (ii+1) (Array.length arr) arr)

viewEditJson : JsonValueViewer -> Html JsonValueViewer
viewEditJson jsonValue =
  case jsonValue of
    JsonVString str ->
      div [inlineStyle]
        [ input [type_ "text", value str, onInput (\newstring -> JsonVString newstring)] []
        , makeNullButton
        ]
    JsonVInt int ->
      div [inlineStyle]
        [ input [type_ "number", value (toString int), step "1", onInput (\newIntStr -> JsonVInt <| Result.withDefault int <| String.toInt newIntStr) ] []
        , makeNullButton
        ]
    JsonVFloat float ->
      div [inlineStyle]
        [ input [type_ "number", value (toString float), step "any", onInput (\newIntStr -> JsonVFloat <| Result.withDefault float <| String.toFloat newIntStr) ] []
        , makeNullButton
        ]
    JsonVArray array ->
      div [inlineStyle]
        [ text "["
        , ul [] <| List.concat
          [ Array.toList <| Array.indexedMap
            ( \ii jv ->
              li []
                [ Html.map (\newJson -> JsonVArray (Array.set ii newJson array) ) (viewEditJson jv)
                , button [onClick <| JsonVArray <| arrayDelete ii array] [text "Delete field"]
                ]
            ) array
          , [ li []
              [ button [onClick <| JsonVArray <| Array.push JsonVNull array] [text "add entry"]
              ]
            ]
          ]
        , text "]"
        , makeNullButton
        ]
    JsonVObject obj ->
      div [inlineStyle]
        [ text "{"
        , table [style [("margin-left", "30px")] ]
          [ tbody [] <| List.concat
            [ Array.toList <| Array.indexedMap
               ( \ii (key, val) ->
                tr []
                  [ td [style [("vertical-align", "top")]]
                    [ input
                      [ type_ "text"
                      , value key
                      , onInput (\newKey -> JsonVObject <| Array.set ii (newKey, val) obj)
                      ]
                      []
                    ] --text <| key ++ ": "
                  , td [] [Html.map (\newVal -> JsonVObject (Array.set ii (key, newVal) obj) ) (viewEditJson val)]
                  , td [] [button [onClick <| JsonVObject <| arrayDelete ii obj] [text "Delete field"]]
                  ]
               )
               obj
            , [ tr [] [td [] [button [onClick <| JsonVObject <| Array.push ("", JsonVNull) obj] [text "add field"]]] ]
            ]
          ]
        , text "}"
        , makeNullButton
        ]
    JsonVBool bool ->
      div [inlineStyle]
        [ text <| toString bool
        , makeNullButton
        ]
    JsonVNull ->
      div []
        [ text "null"
        , button [onClick <| JsonVString ""] [text "make string"]
        , button [onClick <| JsonVInt 0] [text "make int"]
        , button [onClick <| JsonVFloat 0] [text "make float"]
        , button [onClick <| JsonVObject Array.empty] [text "make object"]
        , button [onClick <| JsonVArray Array.empty] [text "make array"]
        ]
