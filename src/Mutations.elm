module Mutations exposing (saveEventRequest)

-- import Page.Planner exposing (Msg(..))

import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Queries
import RemoteData exposing (RemoteData)
import Tour.InputObject exposing (SaveEventInput, buildSaveEventInput)
import Tour.Mutation as Mutation exposing (SaveEventRequiredArguments)
import Tour.Object
import Tour.Object.SaveEventPayload as SaveEventPayload exposing (user)
import Types exposing (Endpoint, Event(..), EventAttributes, UserTrip)
import Uuid exposing (Uuid, toString)


saveEventMutation : EventAttributes -> Uuid -> SelectionSet (Maybe UserTrip) RootMutation
saveEventMutation { title, event_type } trip_id =
    let
        saveEventInput =
            { input =
                buildSaveEventInput
                    (\x ->
                        { x
                            | title = Present title
                            , trip_id = Present (Uuid.toString trip_id)
                            , type_ = Present event_type
                        }
                    )
            }
    in
    Mutation.saveEvent saveEventInput <| user <| Queries.userTripSelection trip_id



-- |> SelectionSet.map (\event -> case event of Just event -> event Nothing -> )


saveEventRequest : Endpoint -> EventAttributes -> Uuid -> Graphql.Http.Request (Maybe UserTrip)
saveEventRequest endpoint event trip_id =
    saveEventMutation event trip_id
        |> Graphql.Http.mutationRequest endpoint
