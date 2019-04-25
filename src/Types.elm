module Types exposing (Authentication(..), Endpoint, Event(..), EventAttributes, RemoteGraphqlResponse, Response, Trip, User)

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)


type alias EventAttributes =
    { uuid : Maybe String
    , title : String
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


type alias User =
    { email : String
    , trips : List Trip
    }


type alias Trip =
    { name : String
    , price : Maybe String
    , events : List Event
    }


type Event
    = Activity String (Maybe Int)
    | Lodging String (Maybe Int)
    | Flight String (Maybe Int)
    | Transportation String (Maybe Int)
    | Cruise String (Maybe Int)
    | Information String
    | Dining String
