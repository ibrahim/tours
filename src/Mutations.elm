module Mutations exposing (eventRequest, sectionRequest)

-- import Page.Planner exposing (Msg(..))

import Api exposing (endpoint)
import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..), fromMaybe)
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Queries
import RemoteData exposing (RemoteData)
import Tour.InputObject exposing (SaveEventInput, buildSaveEventInput, buildSaveSectionInput)
import Tour.Mutation as Mutation exposing (SaveEventRequiredArguments)
import Tour.Object
import Tour.Object.SaveEventPayload as SaveEventPayload exposing (user)
import Tour.Object.SaveSectionPayload as SaveSectionPayload exposing (user)
import Types exposing (..)
import Uuid exposing (Uuid, toString)



-- Section


saveSectionMutation : SectionInputs -> TripId -> Maybe SectionId -> SelectionSet (Maybe UserTrip) RootMutation
saveSectionMutation inputs trip_id section_id =
    let
        section_input =
            case inputs of
                CreateSection _ ->
                    identity

                UpdateSection { title, is_day, day_order, day_date } ->
                    \x ->
                        { x
                            | uuid = fromMaybe <| Maybe.map Uuid.toString section_id
                            , is_day = Present is_day
                            , day_order = Present day_order
                            , day_date = Present day_date
                        }

        required_args =
            case inputs of
                CreateSection { title } ->
                    { trip_id = Uuid.toString trip_id, title = title }

                UpdateSection { title } ->
                    { trip_id = Uuid.toString trip_id, title = title }

        saveSectionInput =
            { input =
                buildSaveSectionInput required_args section_input
            }
    in
    Mutation.saveSection saveSectionInput <| SaveSectionPayload.user <| Queries.userTripSelection trip_id Nothing


sectionRequest : SectionInputs -> TripId -> Maybe SectionId -> Graphql.Http.Request (Maybe UserTrip)
sectionRequest section trip_id section_id =
    saveSectionMutation section trip_id section_id
        |> Graphql.Http.mutationRequest Api.endpoint



-- Event


saveEventMutation : EventInputs -> TripId -> SectionId -> Maybe EventId -> SelectionSet (Maybe UserTrip) RootMutation
saveEventMutation inputs trip_id section_id event_id =
    let
        event_input =
            case inputs of
                CreateEvent _ ->
                    identity

                UpdateEvent { title, price } ->
                    \x ->
                        { x
                            | uuid = fromMaybe <| Maybe.map Uuid.toString event_id
                            , title = fromMaybe title
                            , price = fromMaybe price
                        }

        required_args =
            case inputs of
                CreateEvent { event_type } ->
                    { trip_id = Uuid.toString trip_id, section_id = Uuid.toString section_id, type_ = event_type }

                UpdateEvent { event_type } ->
                    { trip_id = Uuid.toString trip_id, section_id = Uuid.toString section_id, type_ = event_type }

        saveEventInput =
            { input =
                buildSaveEventInput required_args event_input
            }
    in
    Mutation.saveEvent saveEventInput <| SaveEventPayload.user <| Queries.userTripSelection trip_id event_id



-- |> SelectionSet.map (\event -> case event of Just event -> event Nothing -> )


eventRequest : EventInputs -> TripId -> SectionId -> Maybe EventId -> Graphql.Http.Request (Maybe UserTrip)
eventRequest event trip_id section_id event_id =
    saveEventMutation event trip_id section_id event_id
        |> Graphql.Http.mutationRequest Api.endpoint
