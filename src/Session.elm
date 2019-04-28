module Session exposing (Session(..), navKey)

import Browser.Navigation as Nav
import Url exposing (Url)



-- TYPES


type Session
    = LoggedIn Url Nav.Key
    | Guest Url Nav.Key



-- INFO


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn _ key ->
            key

        Guest _ key ->
            key
