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
    SelectionSet.succeed Activity
        |> with Tour.Object.Activity.uuid
        |> with Tour.Object.Activity.section_id
        |> with Tour.Object.Activity.type_
        |> with Tour.Object.Activity.title
        |> with Tour.Object.Activity.price
        |> with Tour.Object.Activity.notes
        |> with Tour.Object.Activity.currency
        |> with Tour.Object.Activity.starts_at
        |> with Tour.Object.Activity.ends_at
        |> with Tour.Object.Activity.duration
        |> with Tour.Object.Activity.booked_through
        |> with Tour.Object.Activity.confirmation
        |> with Tour.Object.Activity.provider


lodgingSelection =
    SelectionSet.succeed Lodging
        |> with Tour.Object.Lodging.uuid
        |> with Tour.Object.Lodging.section_id
        |> with Tour.Object.Lodging.type_
        |> with Tour.Object.Lodging.title
        |> with Tour.Object.Lodging.price
        |> with Tour.Object.Lodging.currency
        |> with Tour.Object.Lodging.notes
        |> with Tour.Object.Lodging.starts_at
        |> with Tour.Object.Lodging.ends_at
        |> with Tour.Object.Lodging.duration
        |> with Tour.Object.Lodging.booked_through
        |> with Tour.Object.Lodging.confirmation
        |> with Tour.Object.Lodging.provider


flightSelection =
    SelectionSet.succeed Flight
        |> with Tour.Object.Flight.uuid
        |> with Tour.Object.Flight.section_id
        |> with Tour.Object.Flight.type_
        |> with Tour.Object.Flight.title
        |> with Tour.Object.Flight.price
        |> with Tour.Object.Flight.currency
        |> with Tour.Object.Flight.notes
        |> with Tour.Object.Flight.starts_at
        |> with Tour.Object.Flight.ends_at
        |> with Tour.Object.Flight.duration
        |> with Tour.Object.Flight.booked_through
        |> with Tour.Object.Flight.confirmation
        |> with Tour.Object.Flight.airline
        |> with Tour.Object.Flight.flight_number
        |> with Tour.Object.Flight.terminal
        |> with Tour.Object.Flight.gate


transportationSelection =
    SelectionSet.succeed Transportation
        |> with Tour.Object.Transportation.uuid
        |> with Tour.Object.Transportation.section_id
        |> with Tour.Object.Transportation.type_
        |> with Tour.Object.Transportation.title
        |> with Tour.Object.Transportation.price
        |> with Tour.Object.Transportation.currency
        |> with Tour.Object.Transportation.notes
        |> with Tour.Object.Transportation.starts_at
        |> with Tour.Object.Transportation.ends_at
        |> with Tour.Object.Transportation.duration
        |> with Tour.Object.Transportation.booked_through
        |> with Tour.Object.Transportation.confirmation
        |> with Tour.Object.Transportation.carrier
        |> with Tour.Object.Transportation.phone_number


cruiseSelection =
    SelectionSet.succeed Cruise
        |> with Tour.Object.Cruise.uuid
        |> with Tour.Object.Cruise.section_id
        |> with Tour.Object.Cruise.type_
        |> with Tour.Object.Cruise.title
        |> with Tour.Object.Cruise.price
        |> with Tour.Object.Cruise.currency
        |> with Tour.Object.Cruise.notes
        |> with Tour.Object.Cruise.starts_at
        |> with Tour.Object.Cruise.ends_at
        |> with Tour.Object.Cruise.duration
        |> with Tour.Object.Cruise.booked_through
        |> with Tour.Object.Cruise.confirmation
        |> with Tour.Object.Cruise.carrier
        |> with Tour.Object.Cruise.cabin_type
        |> with Tour.Object.Cruise.cabin_number


informationSelection =
    SelectionSet.succeed Information
        |> with Tour.Object.Information.uuid
        |> with Tour.Object.Information.section_id
        |> with Tour.Object.Information.type_
        |> with Tour.Object.Information.title
        |> with Tour.Object.Information.notes
        |> with Tour.Object.Information.info_type


diningSelection =
    SelectionSet.succeed Dining
        |> with Tour.Object.Dining.uuid
        |> with Tour.Object.Dining.section_id
        |> with Tour.Object.Dining.type_
        |> with Tour.Object.Dining.title
        |> with Tour.Object.Dining.price
        |> with Tour.Object.Dining.currency
        |> with Tour.Object.Dining.notes
        |> with Tour.Object.Dining.starts_at
        |> with Tour.Object.Dining.ends_at
        |> with Tour.Object.Dining.duration
        |> with Tour.Object.Dining.booked_through
        |> with Tour.Object.Dining.confirmation
        |> with Tour.Object.Dining.provider
