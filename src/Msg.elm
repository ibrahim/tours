module Msg exposing (Msg(..))

import Graphql.Http
import Graphql.Http.GraphqlError
import Model exposing (Model)
import RemoteData exposing (RemoteData)
import Types exposing (..)


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error User) User)
    | GotEventResponse (RemoteData (Graphql.Http.Error (Maybe Event)) (Maybe Event))
    | SetEventTitle String
    | SubmitEvent
