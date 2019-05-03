module Page exposing (Page(..), header)

import Api
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Route exposing (Route(..))


type Page
    = Other
    | Home
    | Login
    | Planner


header : String -> Html msg
header title =
    nav [ attribute "aria-label" "main navigation", class "navbar", attribute "role" "navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ href "", class "navbar-item", Route.href Route.Home ]
                [ img [ attribute "height" "31px", src "/logo.jpg", attribute "width" "32px" ]
                    []
                , span [ class "title is-4" ] [ text "TourFax" ]
                ]
            , a [ href "", attribute "aria-expanded" "false", attribute "aria-label" "menu", class "navbar-burger burger", attribute "data-target" "navbarBasicExample", attribute "role" "button" ]
                [ span [ attribute "aria-hidden" "true" ]
                    []
                , span [ attribute "aria-hidden" "true" ]
                    []
                , span [ attribute "aria-hidden" "true" ]
                    []
                ]
            ]
        , div [ class "navbar-menu", id "navbarBasicExample" ]
            [ div [ class "navbar-start" ]
                [ a [ href "", class "navbar-item" ]
                    [ text "Home      " ]
                , a [ href "", class "navbar-item" ]
                    [ text "Documentation      " ]
                , div [ class "navbar-item has-dropdown is-hoverable" ]
                    [ a [ class "navbar-link" ]
                        [ text "More        " ]
                    , div [ class "navbar-dropdown" ]
                        [ a [ class "navbar-item" ]
                            [ text "About          " ]
                        , a [ class "navbar-item" ]
                            [ text "Jobs          " ]
                        , a [ class "navbar-item" ]
                            [ text "Contact          " ]
                        , hr [ class "navbar-divider" ]
                            []
                        , a [ class "navbar-item" ]
                            [ text "Report an issue          " ]
                        ]
                    ]
                ]
            , div [ class "navbar-end" ]
                [ div [ class "navbar-item" ]
                    [ div [ class "buttons" ]
                        [ a [ class "button is-primary" ]
                            [ strong []
                                [ text "Sign up" ]
                            ]
                        , a [ class "button is-light", Route.href Route.Login ]
                            [ text "Log in" ]
                        ]
                    ]
                ]
            ]
        ]
