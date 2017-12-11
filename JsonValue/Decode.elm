module JsonValue.Decode exposing (jsonValue)

import JsonValue exposing (..)
import Json.Decode as Decode exposing (..)

import Array

jsonValue : Decoder JsonValue
jsonValue =
  oneOf
    [ jsonInt
    , jsonFloat
    , jsonBool
    , jsonString
    , Decode.lazy (\_ -> jsonArray)
    , Decode.lazy (\_ -> jsonObject)
    ]

jsonString : Decoder JsonValue
jsonString = map JsonString string

jsonInt : Decoder JsonValue
jsonInt = map JsonInt int

jsonFloat : Decoder JsonValue
jsonFloat = map JsonFloat float

jsonArray : Decoder JsonValue
jsonArray = map JsonArray <| array <| Decode.lazy (\_ -> jsonValue)

jsonObject : Decoder JsonValue
jsonObject = map (JsonObject << Array.fromList) <| keyValuePairs <| Decode.lazy (\_ -> jsonValue)

jsonBool : Decoder JsonValue
jsonBool = map JsonBool bool
