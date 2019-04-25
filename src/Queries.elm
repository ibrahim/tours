module Queries exposing (eventsSelection, userTripsQuery)

import Graphql.Operation exposing (RootQuery)
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
import Tour.Object.User
import Tour.Query as Query exposing (current_user)
import Tour.Scalar
import Tour.Union
import Tour.Union.Event
import Types exposing (Event(..), Response, Trip, User)


userTripsQuery : SelectionSet Response RootQuery
userTripsQuery =
    Query.current_user userSelection



-- userSelection : SelectionSet User Tour.Object.User


userSelection =
    SelectionSet.map2 User
        Tour.Object.User.email
        (Tour.Object.User.trips identity tripSelection)



-- tripSelection : SelectionSet Trip Tour.Object.Trip


tripSelection =
    SelectionSet.map3 Trip
        Tour.Object.Trip.name
        Tour.Object.Trip.price
        (Tour.Object.Trip.events eventsSelection)


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
