module Api exposing (endpoint, getUserTrips)

import Graphql.Http
import Graphql.Http.GraphqlError
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Endpoint, Response, Trip)


endpoint : String
endpoint =
    "http://localhost:5555/graphql"


getUserTrips : Graphql.Http.Request Response
getUserTrips =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest endpoint
