-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.ScalarCodecs exposing (Id, codecs)

import Json.Decode as Decode exposing (Decoder)
import Tour.Scalar exposing (defaultCodecs)


type alias Id =
    Tour.Scalar.Id


codecs : Tour.Scalar.Codecs Id
codecs =
    Tour.Scalar.defineCodecs
        { codecId = defaultCodecs.codecId
        }
