module Mutations exposing (saveEventRequest)

import Graphql.Http
import Graphql.Operation exposing (RootMutation)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet(..), hardcoded, with)
import Msg exposing (Msg(..))
import Queries
import RemoteData exposing (RemoteData)
import Tour.InputObject exposing (SaveEventInput, buildSaveEventInput)
import Tour.Mutation as Mutation exposing (SaveEventRequiredArguments)
import Tour.Object
import Tour.Object.SaveEventPayload as SaveEventPayload exposing (event)
import Types exposing (Endpoint, Event(..), EventAttributes)


saveEventMutation : EventAttributes -> SelectionSet (Maybe Event) RootMutation
saveEventMutation { title } =
    let
        saveEventInput =
            { input = buildSaveEventInput (\x -> { x | title = Present title })
            }
    in
    Mutation.saveEvent saveEventInput (event Queries.eventsSelection)



-- |> SelectionSet.map (\event -> case event of Just event -> event Nothing -> )


saveEventRequest : Endpoint -> EventAttributes -> Cmd Msg
saveEventRequest endpoint event =
    saveEventMutation event
        |> Graphql.Http.mutationRequest endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotEventResponse)
