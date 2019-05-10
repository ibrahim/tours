module Queries exposing
    ( eventSelection
    , userTripQuery
    , userTripSelection
    , userTripsQuery
    )

import Graphql.Operation exposing (RootQuery)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet, hardcoded, with)
import Tour.Object
import Tour.Object.Activity
import Tour.Object.Cruise
import Tour.Object.Dining
import Tour.Object.Flight
import Tour.Object.Information
import Tour.Object.Lodging
import Tour.Object.Section
import Tour.Object.Transportation
import Tour.Object.Trip
import Tour.Object.User exposing (TripsOptionalArguments)
import Tour.Query as Query exposing (current_user)
import Tour.Scalar
import Tour.Union
import Tour.Union.Event
import Types exposing (..)
import Uuid exposing (Uuid, toString)


userTripsQuery : SelectionSet UserTrips RootQuery
userTripsQuery =
    Query.current_user <|
        SelectionSet.map2 UserTrips
            Tour.Object.User.email
            (Tour.Object.User.trips identity <|
                SelectionSet.map3 Trip
                    Tour.Object.Trip.uuid
                    Tour.Object.Trip.name
                    Tour.Object.Trip.price
            )


userTripQuery : Uuid -> Maybe Uuid -> SelectionSet UserTrip RootQuery
userTripQuery trip_id event_id =
    Query.current_user <| userTripSelection trip_id event_id


userTripSelection : Uuid -> Maybe Uuid -> SelectionSet UserTrip Tour.Object.User
userTripSelection trip_id event_id =
    let
        optional_trip_id =
            \o -> { o | uuid = Present (Uuid.toString trip_id) }

        optional_event_args =
            case event_id of
                Just uuid ->
                    \o ->
                        { o
                            | uuid = Present (Uuid.toString uuid)
                            , trip_id = Present (Uuid.toString trip_id)
                        }

                Nothing ->
                    identity
    in
    SelectionSet.map3 UserTrip
        Tour.Object.User.email
        (Tour.Object.User.event optional_event_args eventSelection)
        (Tour.Object.User.trips optional_trip_id <|
            SelectionSet.map5 TripWithEvents
                Tour.Object.Trip.uuid
                Tour.Object.Trip.name
                Tour.Object.Trip.price
                (Tour.Object.Trip.sections sectionSelection)
                (Tour.Object.Trip.events eventSelection)
        )



-- eventQuery : Maybe Uuid -> SelectionSet (Maybe Event) RootQuery
-- eventQuery maybe_uuid =
--     let
--         optional_uuid =
--             case maybe_uuid of
--                 Just uuid ->
--                     \o -> { o | uuid = Present (Uuid.toString uuid) }
--
--                 Nothing ->
--                     identity
--     in
--     Query.event optional_uuid <| eventSelection


tripSelection =
    SelectionSet.map3 Trip
        Tour.Object.Trip.uuid
        Tour.Object.Trip.name
        Tour.Object.Trip.price


sectionSelection =
    SelectionSet.map5 Section
        Tour.Object.Section.uuid
        Tour.Object.Section.title
        Tour.Object.Section.is_day
        Tour.Object.Section.day_order
        Tour.Object.Section.day_date


eventSelection : SelectionSet Event Tour.Union.Event
eventSelection =
    Tour.Union.Event.fragments
        { onActivity = activitySelection
        , onLodging = lodgingSelection
        , onFlight = flightSelection
        , onTransportation = transportationSelection
        , onCruise = cruiseSelection
        , onInformation = informationSelection
        , onDining = diningSelection
        }


activitySelection =
    SelectionSet.map5 Activity
        Tour.Object.Activity.uuid
        Tour.Object.Activity.section_id
        Tour.Object.Activity.type_
        Tour.Object.Activity.title
        Tour.Object.Activity.price


lodgingSelection =
    SelectionSet.map5 Lodging
        Tour.Object.Lodging.uuid
        Tour.Object.Lodging.section_id
        Tour.Object.Lodging.type_
        Tour.Object.Lodging.title
        Tour.Object.Lodging.price


flightSelection =
    SelectionSet.map5 Flight
        Tour.Object.Flight.uuid
        Tour.Object.Flight.section_id
        Tour.Object.Flight.type_
        Tour.Object.Flight.title
        Tour.Object.Flight.price


transportationSelection =
    SelectionSet.map5 Transportation
        Tour.Object.Transportation.uuid
        Tour.Object.Transportation.section_id
        Tour.Object.Transportation.type_
        Tour.Object.Transportation.title
        Tour.Object.Transportation.price


cruiseSelection =
    SelectionSet.map5 Cruise
        Tour.Object.Cruise.uuid
        Tour.Object.Cruise.section_id
        Tour.Object.Cruise.type_
        Tour.Object.Cruise.title
        Tour.Object.Cruise.price


informationSelection =
    SelectionSet.map4 Information
        Tour.Object.Information.uuid
        Tour.Object.Information.section_id
        Tour.Object.Information.type_
        Tour.Object.Information.title


diningSelection =
    SelectionSet.map4 Dining
        Tour.Object.Dining.uuid
        Tour.Object.Dining.section_id
        Tour.Object.Dining.type_
        Tour.Object.Dining.title
