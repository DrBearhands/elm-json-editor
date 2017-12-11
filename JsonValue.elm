module JsonValue exposing (JsonValue(..))

import Dict exposing (Dict)
import Array exposing (Array)

type JsonValue
  = JsonString String
  | JsonInt Int
  | JsonFloat Float
  | JsonArray (Array JsonValue)
  | JsonObject (Array (String, JsonValue))
  | JsonBool Bool
  | JsonNull
