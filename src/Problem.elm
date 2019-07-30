module Problem exposing (AppError(..), Problem(..), ValidatedField(..), showProblems)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


type AppError
    = AuthenticationError
    | GraphqlError
    | InvalidEntry ValidatedField
    | InvalidData
    | RecordNotFound
    | ServerError


type ValidatedField
    = ValidatedField String


type Problem
    = Problem AppError String


showProblems toMsg problems =
    ul [] (List.map (viewProblem toMsg) problems)


container : msg -> List (Html msg) -> String -> Html msg
container deleteMsg content classes =
    div [ class <| "notification " ++ classes ]
        [ button [ class "delete", onClick deleteMsg ] []
        , div [] content
        ]


viewProblem toMsg problem =
    case problem of
        Problem AuthenticationError message ->
            container
                toMsg
                [ p []
                    [ text <| message ++ ". "
                    , a [ href "#/login" ] [ text <| "Click to sign-in." ]
                    ]
                ]
                "is-danger"

        Problem (InvalidEntry (ValidatedField field)) message ->
            container toMsg [ p [] [ text <| field ++ ":" ++ message ] ] "is-warning"

        Problem ServerError message ->
            container toMsg [ p [] [ text message ] ] "is-warning"

        Problem _ message ->
            container toMsg [ p [] [ text message ] ] "is-warning"
