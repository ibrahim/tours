module Queries exposing (eventsSelection, userTripQuery, userTripSelection, userTripsQuery)

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
                (Tour.Object.Trip.events eventsSelection)
        )


tripSelection =
    SelectionSet.map3 Trip
        Tour.Object.Trip.uuid
        Tour.Object.Trip.name
        Tour.Object.Trip.price


eventsSelection : SelectionSet Event Tour.Union.Event
eventsSelection =
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
    SelectionSet.map2 Activity
        Tour.Object.Activity.title
        Tour.Object.Activity.price


lodgingSelection =
    SelectionSet.map2 Lodging
        Tour.Object.Lodging.title
        Tour.Object.Lodging.price


flightSelection =
    SelectionSet.map2 Flight
        Tour.Object.Flight.title
        Tour.Object.Flight.price


transportationSelection =
    SelectionSet.map2 Transportation
        Tour.Object.Transportation.title
        Tour.Object.Transportation.price


cruiseSelection =
    SelectionSet.map2 Cruise
        Tour.Object.Cruise.title
        Tour.Object.Cruise.price


informationSelection =
    SelectionSet.map Information Tour.Object.Information.title


diningSelection =
    SelectionSet.map Dining Tour.Object.Dining.title
