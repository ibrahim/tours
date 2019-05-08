module Mutations exposing (eventRequest)

-- import Page.Planner exposing (Msg(..))

import Api exposing (endpoint)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Queries
import RemoteData exposing (RemoteData)
import Tour.InputObject exposing (SaveEventInput, buildSaveEventInput)
import Tour.Mutation as Mutation exposing (SaveEventRequiredArguments)
import Tour.Object
import Tour.Object.SaveEventPayload as SaveEventPayload exposing (user)
import Types exposing (Event(..), EventAttributes, EventForm, EventInputs(..), UserTrip)
import Uuid exposing (Uuid, toString)


saveEventMutation : EventInputs -> Uuid -> Maybe Uuid -> SelectionSet (Maybe UserTrip) RootMutation
saveEventMutation inputs trip_id event_id =
    let
        event_input =
            case inputs of
                CreateEvent { title, uuid, event_type } ->
                    \x ->
                        { x
                            | title = Present title
                            , trip_id = Present (Uuid.toString trip_id)
                            , type_ = Present event_type
                        }

                UpdateEvent { title, uuid, price, event_type } ->
                    \x ->
                        { x
                            | uuid = Present uuid
                            , title = Present title
                            , price = fromMaybe price
                            , trip_id = Present (Uuid.toString trip_id)
                            , type_ = Present event_type
                        }

        saveEventInput =
            { input =
                buildSaveEventInput event_input
            }
    in
    Mutation.saveEvent saveEventInput <| user <| Queries.userTripSelection trip_id event_id



-- |> SelectionSet.map (\event -> case event of Just event -> event Nothing -> )


eventRequest : EventInputs -> Uuid -> Maybe Uuid -> Graphql.Http.Request (Maybe UserTrip)
eventRequest event trip_id event_id =
    saveEventMutation event trip_id event_id
        |> Graphql.Http.mutationRequest Api.endpoint
