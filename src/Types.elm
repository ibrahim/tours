module Types exposing (Event(..), Response, Trip, User)


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
