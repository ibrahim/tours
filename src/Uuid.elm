module Uuid exposing (Uuid, decoder, toString, urlParser)

import Json.Decode as Decode exposing (Decoder)
import Url.Parser exposing (Parser)



-- TYPES


type Uuid
    = Uuid String



-- CREATE


urlParser : Parser (Uuid -> a) a
urlParser =
    Url.Parser.custom "UUID" (\str -> Just (Uuid str))


decoder : Decoder Uuid
decoder =
    Decode.map Uuid Decode.string



-- TRANSFORM


toString : Uuid -> String
toString (Uuid str) =
    str
