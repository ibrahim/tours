module Queries exposing (eventQuery, eventSelection, userTripQuery, userTripSelection, userTripsQuery)

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
import Tour.Object.Transportation
import Tour.Object.Trip
import Tour.Object.User exposing (TripsOptionalArguments)
import Tour.Query as Query exposing (current_user)
import Tour.Scalar
import Tour.Union
import Tour.Union.Event
import Types exposing (Event(..), Response, Trip, TripWithEvents, UserTrip, UserTrips)
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


userTripQuery : Uuid -> SelectionSet UserTrip RootQuery
userTripQuery uuid =
    Query.current_user <| userTripSelection uuid


userTripSelection : Uuid -> SelectionSet UserTrip Tour.Object.User
userTripSelection uuid =
    let
        optional_args =
            \o -> { o | uuid = Present (Uuid.toString uuid) }
    in
    SelectionSet.map2 UserTrip
        Tour.Object.User.email
        (Tour.Object.User.trips optional_args <|
            SelectionSet.map4 TripWithEvents
                Tour.Object.Trip.uuid
                Tour.Object.Trip.name
                Tour.Object.Trip.price
                (Tour.Object.Trip.events eventSelection)
        )


eventQuery : Uuid -> SelectionSet Event RootQuery
eventQuery uuid =
    Query.event { uuid = Uuid.toString uuid } <| eventSelection


tripSelection =
    SelectionSet.map3 Trip
        Tour.Object.Trip.uuid
        Tour.Object.Trip.name
        Tour.Object.Trip.price


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
    SelectionSet.map3 Activity
        Tour.Object.Activity.uuid
        Tour.Object.Activity.title
        Tour.Object.Activity.price


lodgingSelection =
    SelectionSet.map3 Lodging
        Tour.Object.Lodging.uuid
        Tour.Object.Lodging.title
        Tour.Object.Lodging.price


flightSelection =
    SelectionSet.map3 Flight
        Tour.Object.Flight.uuid
        Tour.Object.Flight.title
        Tour.Object.Flight.price


transportationSelection =
    SelectionSet.map3 Transportation
        Tour.Object.Transportation.uuid
        Tour.Object.Transportation.title
        Tour.Object.Transportation.price


cruiseSelection =
    SelectionSet.map3 Cruise
        Tour.Object.Cruise.uuid
        Tour.Object.Cruise.title
        Tour.Object.Cruise.price


informationSelection =
    SelectionSet.map2 Information
        Tour.Object.Information.uuid
        Tour.Object.Information.title


diningSelection =
    SelectionSet.map2 Dining
        Tour.Object.Dining.uuid
        Tour.Object.Dining.title
