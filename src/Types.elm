module Types exposing
    ( Authentication(..)
    , Endpoint
    , Event(..)
    , EventAttributes
    , EventForm
    , EventInputs(..)
    , RemoteGraphqlResponse
    , Response
    , Trip
    , TripWithEvents
    , User
    , UserTrip
    , UserTrips
    )

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)
import Uuid exposing (Uuid)


type EventInputs
    = CreateEvent EventAttributes
    | UpdateEvent EventForm


type alias EventAttributes =
    { uuid : Maybe String
    , title : String
    , event_type : String
    }


type alias EventForm =
    { uuid : Maybe String
    , title : String
    , event_type : String
    , price : Maybe Int
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
    , event : Maybe Event
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



-- Activity uuid type title price


type Event
    = Activity String String String (Maybe Int)
    | Lodging String String String (Maybe Int)
    | Flight String String String (Maybe Int)
    | Transportation String String String (Maybe Int)
    | Cruise String String String (Maybe Int)
    | Information String String String
    | Dining String String String
