module EditableValue exposing (EditableValue, Config, null, config, encode, decoder, view, editableView)

{-| This package allows editing JSON files with unknown structure in an Elm view.
This might be necessary e.g. for managing a noSQL server with varying document structures.

# Types
@docs EditableValue
@docs Config

# Initialization
@docs null

# Encoding/Decoding
@docs decoder
@docs encode

# Config
@docs config

# Views
@docs view
@docs editableView
-}

import Dict exposing (Dict)
import Array exposing (Array)
import Json.Encode as Encode exposing(Value)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

{-| Opaque type holding the state of the JSON
-}
type EditableValue
  = JsonString String
  | JsonInt Int
  | JsonFloat Float
  | JsonArray (Array EditableValue)
  | JsonObject (Array (String, EditableValue))
  | JsonBool Bool
  | JsonNull

{-|
Constructs a 'null' EditableValue
-}
null : EditableValue
null = JsonNull

{-| Opaque type used for configuring the view functions
-}
type Config =
  Config {}

{-| Basic constructor for Config
-}
config : Config
config = Config {}

{-|
-}
encode : EditableValue -> Value
encode value =
  case value of
    JsonString s -> Encode.string s
    JsonInt i -> Encode.int i
    JsonFloat f -> Encode.float f
    JsonArray a -> Encode.array (Array.map encode a)
    JsonObject a -> Encode.object <| Array.toList <| Array.map (\(key, val) -> (key, encode val)) a
    JsonBool b -> Encode.bool b
    JsonNull -> Encode.null

{-|
-}
decoder : Decoder EditableValue
decoder =
  let
    jsonString : Decoder EditableValue
    jsonString = Decode.map JsonString Decode.string
    jsonInt = Decode.map JsonInt Decode.int
    jsonFloat = Decode.map JsonFloat Decode.float
    jsonArray = Decode.map JsonArray <| Decode.array <| Decode.lazy (\_ -> decoder)
    jsonObject = Decode.map (JsonObject << Array.fromList) <| Decode.keyValuePairs <| Decode.lazy (\_ -> decoder)
    jsonBool = Decode.map JsonBool Decode.bool
  in
    Decode.oneOf
      [ jsonInt
      , jsonFloat
      , jsonBool
      , jsonString
      , Decode.lazy (\_ -> jsonArray)
      , Decode.lazy (\_ -> jsonObject)
      ]

{-| Basic view function, does not allow editing the JSON
-}
view : Config -> EditableValue -> Html msg
view config editableValue =
  case editableValue of
    JsonString str -> text str
    JsonInt int -> text <| toString int
    JsonFloat float -> text <| toString float
    JsonArray list ->
      div []
        [ text "["
        , ul [] <| Array.toList <| Array.map (\jv -> li [] [view config jv]) list
        , text "]"
        ]
    JsonObject obj ->
      div []
        [ text "{"
        , table [style [("margin-left", "30px")] ]
          [ tbody [] <| List.concat
            [ Array.toList <| Array.indexedMap
               ( \ii (key, σ) ->
                tr []
                  [ td [style [("vertical-align", "top")]]
                    [ text <| key ++ ": "
                    , view config σ
                    ]
                  ]
               )
               obj
            ]
          ]
        , text "}"
        ]
    JsonBool bool -> text <| toString bool
    JsonNull -> text "null"

{-| View function that allows editing the JSON. The EditableValue cmd produced by this HTML should be used as the new EditableValue.
E.g.
```
type Msg
  = SetEditableValue EditableValue
  ...

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetEditableValue newViewer -> ({model | editableValue = newViewer}, Cmd.none)
    ...
```
-}
editableView : Config -> EditableValue -> Html EditableValue
editableView config editableValue =
  let
    makeNullButton =
      button
        [ onClick JsonNull ]
        [ text "X" ]
    arrayDelete ii arr =
      Array.append
        (Array.slice 0 ii arr)
        (Array.slice (ii+1) (Array.length arr) arr)
    inlineStyle = style [("display", "inline-block")]
  in
    case editableValue of
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
          [ input [type_ "number", value (toString float), step "any", onInput (\newIntStr -> JsonFloat <| Result.withDefault float <| String.toFloat newIntStr) ] []
          , makeNullButton
          ]
      JsonArray array ->
        div [inlineStyle]
          [ text "["
          , ul [] <| List.concat
            [ Array.toList <| Array.indexedMap
              ( \ii jv ->
                li []
                  [ Html.map (\newJson -> JsonArray (Array.set ii newJson array) ) (editableView config jv)
                  , button [onClick <| JsonArray <| arrayDelete ii array] [text "Delete field"]
                  ]
              ) array
            , [ li []
                [ button [onClick <| JsonArray <| Array.push JsonNull array] [text "add entry"]
                ]
              ]
            ]
          , text "]"
          , makeNullButton
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
                    , td [] [Html.map (\newVal -> JsonObject (Array.set ii (key, newVal) obj) ) (editableView config val)]
                    , td [] [button [onClick <| JsonObject <| arrayDelete ii obj] [text "Delete field"]]
                    ]
                 )
                 obj
              , [ tr [] [td [] [button [onClick <| JsonObject <| Array.push ("", JsonNull) obj] [text "add field"]]] ]
              ]
            ]
          , text "}"
          , makeNullButton
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
