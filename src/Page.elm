module Page exposing (Page(..), header)

import Api
import Browser
import Html exposing (Html, a, button, div, h1, h4, input, li, p, pre, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)
import Route exposing (..)


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
                [ a [ Route.href Route.Home ] [ text "Home" ]
                , a [ Route.href Route.Login ] [ text "login" ]
                ]
            ]
        ]
