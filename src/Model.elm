module Model exposing (Model)

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)
import Types exposing (..)


type alias Model =
    RemoteData (Graphql.Http.Error Response) Response
