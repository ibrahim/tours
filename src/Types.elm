module Types exposing (AppError(..), Authentication(..), Endpoint, Event(..), EventAttributes, Problem(..), RemoteGraphqlResponse, Response, Trip, TripWithEvents, User, UserTrip, UserTrips)

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)
import Uuid exposing (Uuid)


type AppError
    = AuthenticationError
    | GraphqlError


type Problem
    = Problem AppError String


type alias EventAttributes =
    { uuid : Maybe String
    , title : String
    , event_type : String
    }


type alias Email =
    String


type alias Endpoint =
    String


type Authentication
    = Authenticated Email
    | Unauthenticated


type alias RemoteGraphqlResponse =
    RemoteData (Graphql.Http.Error Response) Response


type alias Response =
    User


type alias UserTrips =
    { email : String
    , trips : List Trip
    }


type alias User =
    { email : String
    , trips : List Trip
    }


type alias UserTrip =
    { email : String
    , trips : List TripWithEvents
    }


type alias TripWithEvents =
    { uuid : String
    , name : String
    , price : Maybe String
    , events : List Event
    }


type alias Trip =
    { uuid : String
    , name : String
    , price : Maybe String
    }


type Event
    = Activity String String (Maybe Int)
    | Lodging String String (Maybe Int)
    | Flight String String (Maybe Int)
    | Transportation String String (Maybe Int)
    | Cruise String String (Maybe Int)
    | Information String String
    | Dining String String
