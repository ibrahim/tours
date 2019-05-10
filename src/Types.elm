module Types exposing
    ( Authentication(..)
    , Endpoint
    , Event(..)
    , EventAttributes
    , EventForm
    , EventId
    , EventInputs(..)
    , RemoteGraphqlResponse
    , Response
    , Section
    , SectionId
    , SectionInputs(..)
    , Trip
    , TripId
    , TripWithEvents
    , User
    , UserTrip
    , UserTrips
    )

import Graphql.Http
import Graphql.Http.GraphqlError
import RemoteData exposing (RemoteData)
import Uuid exposing (Uuid)


type alias SectionId =
    Uuid


type alias EventId =
    Uuid


type alias TripId =
    Uuid


type EventInputs
    = CreateEvent EventAttributes
    | UpdateEvent EventForm


type SectionInputs
    = CreateSection SectionNewForm
    | UpdateSection SectionEditForm


type alias SectionNewForm =
    { title : String
    , trip_id : String
    }


type alias SectionEditForm =
    { title : String
    , trip_id : String
    , is_day : Bool
    , day_date : Int
    , day_order : Int
    }


type alias EventAttributes =
    { title : String
    , event_type : String
    }


type alias EventForm =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    }


type alias Section =
    { uuid : String
    , title : String
    , is_day : Maybe Bool
    , day_order : Maybe Int
    , day_date : Maybe Int
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
    , sections : List Section
    , events : List Event
    }


type alias Trip =
    { uuid : String
    , name : String
    , price : Maybe String
    }



-- Activity uuid type title price


type alias EventUuid =
    String


type alias SectionUuid =
    String


type alias EventType =
    String


type alias Title =
    Maybe String


type alias Price =
    Maybe Int


type Event
    = Activity EventUuid SectionUuid EventType Title Price
    | Lodging EventUuid SectionUuid EventType Title Price
    | Flight EventUuid SectionUuid EventType Title Price
    | Transportation EventUuid SectionUuid EventType Title Price
    | Cruise EventUuid SectionUuid EventType Title Price
    | Information EventUuid SectionUuid EventType Title
    | Dining EventUuid SectionUuid EventType Title
