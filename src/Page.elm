module Page exposing (Page(..), header)

import Api
import Browser
import Html exposing (Html, a, button, div, h1, h4, input, li, p, pre, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)


type Page
    = Other
    | Home
    | Login
    | Planner


header : String -> Html msg
header title =
    div []
        [ ul []
            [ h1 [] [ text title ]
            , li []
                [ a [ href "#/" ] [ text "Home" ]
                , a [ href "#/planner" ] [ text "Tour Planner" ]
                , a [ href "#/login" ] [ text "login" ]
                ]
            ]
        ]
