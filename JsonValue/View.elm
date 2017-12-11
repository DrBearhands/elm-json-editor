module JsonValue.View exposing (viewJson, viewEditJson)

import JsonValue exposing (..)
import Array exposing (Array)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

viewJson : JsonValue -> Html msg
viewJson jsonValue =
  case jsonValue of
    JsonString str -> text str
    JsonInt int -> text <| toString int
    JsonFloat float -> text <| toString float
    JsonArray list ->
      div []
        [ text "["
        , ul [] <| Array.toList <| Array.map (\jv -> li [] [viewJson jv]) list
        , text "]"
        ]
    JsonObject obj ->
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
    JsonBool bool -> text <| toString bool
    JsonNull -> text "null"

makeNullButton : Html JsonValue
makeNullButton =
  button
    [ onClick JsonNull ]
    [ text "X" ]

inlineStyle = style [("display", "inline-block")]

arrayDelete : Int -> Array a -> Array a
arrayDelete ii arr =
  Array.append
    (Array.slice 0 ii arr)
    (Array.slice (ii+1) (Array.length arr) arr)

viewEditJson : JsonValue -> Html JsonValue
viewEditJson jsonValue =
  case jsonValue of
    JsonString str ->
      div [inlineStyle]
        [ input [type_ "text", value str, onInput (\newstring -> JsonString newstring)] []
        , makeNullButton
        ]
    JsonInt int ->
      div [inlineStyle]
        [ input [type_ "number", value (toString int), step "1", onInput (\newIntStr -> JsonInt <| Result.withDefault int <| String.toInt newIntStr) ] []
        , makeNullButton
        ]
    JsonFloat float ->
      div [inlineStyle]
        [ input [type_ "number", value (toString float), onInput (\newIntStr -> JsonFloat <| Result.withDefault float <| String.toFloat newIntStr) ] []
        , makeNullButton
        ]
    JsonArray array ->
      div [inlineStyle]
        [ text "["
        , ul [] <| List.concat
          [ Array.toList <| Array.indexedMap
            ( \ii jv ->
              li []
                [ Html.map (\newJson -> JsonArray (Array.set ii newJson array) ) (viewEditJson jv)
                , button [onClick <| JsonArray <| arrayDelete ii array] [text "Delete field"]
                ]
            ) array
          , [ li []
              [ button [onClick <| JsonArray <| Array.push JsonNull array] [text "add entry"]
              ]
            ]
          ]
        , text "]"
        ]
    JsonObject obj ->
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
                      , onInput (\newKey -> JsonObject <| Array.set ii (newKey, val) obj)
                      ]
                      []
                    ] --text <| key ++ ": "
                  , td [] [Html.map (\newVal -> JsonObject (Array.set ii (key, newVal) obj) ) (viewEditJson val)]
                  , td [] [button [onClick <| JsonObject <| arrayDelete ii obj] [text "Delete field"]]
                  ]
               )
               obj
            , [ tr [] [td [] [button [onClick <| JsonObject <| Array.push ("", JsonNull) obj] [text "add field"]]] ]
            ]
          ]
        , text "}"
        ]
    JsonBool bool ->
      div [inlineStyle]
        [ text <| toString bool
        , makeNullButton
        ]
    JsonNull ->
      div []
        [ text "null"
        , button [onClick <| JsonString ""] [text "make string"]
        , button [onClick <| JsonInt 0] [text "make int"]
        , button [onClick <| JsonFloat 0] [text "make float"]
        , button [onClick <| JsonObject Array.empty] [text "make object"]
        , button [onClick <| JsonArray Array.empty] [text "make array"]
        ]
