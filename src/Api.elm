module Api exposing (getUserTrips)

import Graphql.Http
import Graphql.Http.GraphqlError
import Msg exposing (Msg(..))
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Endpoint)


getUserTrips : Endpoint -> Cmd Msg
getUserTrips endpoint =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest
            endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
