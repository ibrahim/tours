module Api exposing (getUserTrips)

import Graphql.Http
import Graphql.Http.GraphqlError
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Endpoint, Response, Trip)


getUserTrips : Endpoint -> Graphql.Http.Request Response
getUserTrips endpoint =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest endpoint
