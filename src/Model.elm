module Model exposing (Model, init)

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)
import Types exposing (..)


type alias Model =
    { userTrips : RemoteGraphqlResponse
    , current_user : Authentication
    , trips : List Trip
    , event_title : String
    , endpoint : Endpoint
    }


init =
    { current_user = Unauthenticated
    , trips = []
    , userTrips = RemoteData.Loading
    , event_title = ""
    , endpoint = "http://localhost:5555/graphql"
    }
