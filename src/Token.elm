module Token exposing (Token, decoder, encode, toHtml, toString)

import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Url.Parser



-- TYPES


type Token
    = Token String



-- CREATE


decoder : Decoder Token
decoder =
    Decode.map Token Decode.string



-- TRANSFORM


encode : Token -> Value
encode (Token token) =
    Encode.string token


toString : Token -> String
toString (Token token) =
    token


toHtml : Token -> Html msg
toHtml (Token token) =
    Html.text token
