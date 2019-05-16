module Types exposing
    ( ActivityFormFields
    , Authentication(..)
    , CruiseFormFields
    , DiningFormFields
    , Endpoint
    , Event(..)
    , EventAttributes
    , EventForm
    , EventId
    , EventInputs(..)
    , FlightFormFields
    , InformationFormFields
    , LodgingFormFields
    , RemoteGraphqlResponse
    , Response
    , Section
    , SectionId
    , SectionInputs(..)
    , TransportationFormFields
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
    { event_type : String }



-- type EventForm
--     = ActivityForm FormFields
--     | LodgingForm FormFields
--     | FlightForm FormFields
--     | TransportationForm FormFields
--     | CruiseForm FormFields
--     | DiningForm FormFields
--     | InformationForm FormFields


type alias EventForm =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , provider : Provider
    , carrier : Carrier
    , phone_number : PhoneNumber
    , airline : AirLine
    , flight_number : FlightNumber
    , terminal : Terminal
    , gate : Gate
    , cabin_type : CabinType
    , cabin_number : CabinNumber
    , info_type : InfoType
    }


type alias ActivityFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , provider : Provider
    }


type alias LodgingFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    }


type alias TransportationFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , carrier : Carrier
    , phone_number : PhoneNumber
    }


type alias FlightFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , airline : AirLine
    , flight_number : FlightNumber
    , terminal : Terminal
    , gate : Gate
    }


type alias CruiseFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , carrier : Carrier
    , cabin_type : CabinType
    , cabin_number : CabinNumber
    }


type alias DiningFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , price : Price
    , currency : Currency
    , notes : Notes
    , starts_at : StartsAt
    , duration : Duration
    , booked_through : BookedThrough
    , confirmation : Confirmation
    , provider : Provider
    }


type alias InformationFormFields =
    { uuid : EventUuid
    , title : Title
    , section_id : SectionUuid
    , event_type : EventType
    , notes : Notes
    , info_type : InfoType
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


type alias Notes =
    Maybe String


type alias Currency =
    Maybe String


type alias StartsAt =
    Maybe Float


type alias Duration =
    Maybe Int


type alias BookedThrough =
    Maybe String


type alias Confirmation =
    Maybe String


type alias Provider =
    Maybe String


type alias Carrier =
    Maybe String


type alias CabinType =
    Maybe String


type alias CabinNumber =
    Maybe String


type alias AirLine =
    Maybe String


type alias FlightNumber =
    Maybe String


type alias Terminal =
    Maybe String


type alias Gate =
    Maybe String


type alias PhoneNumber =
    Maybe String


type alias InfoType =
    Maybe String


type Event
    = Activity EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation Provider
    | Lodging EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation Provider
    | Flight EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation AirLine FlightNumber Terminal Gate
    | Transportation EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation Carrier PhoneNumber
    | Cruise EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation Carrier CabinType CabinNumber
    | Information EventUuid SectionUuid EventType Title Notes InfoType
    | Dining EventUuid SectionUuid EventType Title Price Currency Notes StartsAt Duration BookedThrough Confirmation Provider
