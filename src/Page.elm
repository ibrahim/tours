module Page exposing (Page(..), header, layout)

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


layout : String -> List (Html msg) -> List (Html msg)
layout title content =
    [ div []
        [ header title
        , div [ class "layout" ] content
        ]
    ]


header : String -> Html msg
header title =
    nav [ attribute "aria-label" "main navigation", class "navbar", attribute "role" "navigation" ]
        [ div [ class "navbar-brand" ]
            [ a [ href "", class "navbar-item is-flex", Route.href Route.Home ]
                [ img [ src "/Tourfax_logo.png", attribute "alt" "TourFax" ] [] ]
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
                , search
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


search =
    div [ class "dropdown is-active-not navbar-item" ]
        [ div [ class "dropdown-trigger control is-small has-icons-left is-loading-not" ]
            [ input [ placeholder "Search", attribute "aria-controls" "dropdown-menu", attribute "aria-haspopup" "true", class "input is-rounded is-small" ] []
            , span [ class "icon is-small is-left" ]
                [ i [ class "fas fa-search" ]
                    []
                ]
            ]
        , div [ class "dropdown-menu", id "dropdown-menu", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ]
                [ a [ class "dropdown-item", href "#" ]
                    [ text "Dropdown item      " ]
                , a [ class "dropdown-item" ]
                    [ text "Other dropdown item      " ]
                , a [ class "dropdown-item is-active", href "#" ]
                    [ text "Active dropdown item      " ]
                , a [ class "dropdown-item", href "#" ]
                    [ text "Other dropdown item      " ]
                , hr [ class "dropdown-divider" ]
                    []
                , a [ class "dropdown-item", href "#" ]
                    [ text "With a divider      " ]
                ]
            ]
        ]
