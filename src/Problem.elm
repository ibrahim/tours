module Problem exposing (AppError(..), Problem(..), showProblems)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type AppError
    = AuthenticationError
    | GraphqlError


type Problem
    = Problem AppError String


showProblems toMsg problems =
    ul [] (List.map (viewProblem toMsg) problems)


container : msg -> List (Html msg) -> Html msg
container deleteMsg content =
    div [ class "notification is-danger" ]
        [ button [ class "delete", onClick deleteMsg ] []
        , div [] content
        ]


viewProblem toMsg problem =
    case problem of
        Problem AuthenticationError message ->
            container toMsg [ a [ href "#/login" ] [ text <| message ++ ". Click here to login." ] ]

        Problem _ message ->
            container toMsg [ p [] [ text message ] ]
