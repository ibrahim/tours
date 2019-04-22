module Api exposing (getUserTrips)

import Graphql.Http
import Graphql.Http.GraphqlError
import Msg exposing (Msg(..))
import Queries
import RemoteData exposing (RemoteData)


getUserTrips : Cmd Msg
getUserTrips =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest
            "http://localhost:5555/graphql"
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)
