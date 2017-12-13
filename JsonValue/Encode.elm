module JsonValue.Encode exposing (jsonValue)

import JsonValue exposing (..)

import Json.Encode exposing (Value, string, int, float, bool, null, array, object)
import Array
import Dict

jsonValue : JsonValue -> Value
jsonValue value =
  case value of
    JsonString s -> string s
    JsonInt i -> int i
    JsonFloat f -> float f
    JsonArray a -> array (Array.map jsonValue a)
    JsonObject dict -> object <| Dict.toList <| Dict.map (\_ -> jsonValue) dict
    JsonBool b -> bool b
    JsonNull -> null
