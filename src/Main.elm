module Main exposing (main)

import Api
import Browser
import Graphql.Document as Document
import Graphql.Http
import Graphql.Http.GraphqlError
import Helpers.Main
import Html exposing (Html, button, div, h1, input, li, p, pre, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Model exposing (Model)
import Msg exposing (Msg(..))
import Mutations
import PrintAny
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Authentication(..), Event(..), Response, Trip, User)



-- Elm Architecture Setup


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    let
        model =
            Model.init
    in
    ( model, Api.getUserTrips model.endpoint )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            case response of
                RemoteData.Success { email, trips } ->
                    ( { model | current_user = Authenticated email, trips = trips }, Cmd.none )

                RemoteData.Failure errorData ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotEventResponse response ->
            ( model, Cmd.none )

        SubmitEvent ->
            ( model, Mutations.saveEventRequest model.endpoint { uuid = Nothing, title = model.event_title } )

        SetEventTitle title ->
            ( { model | event_title = title }, Cmd.none )


view : Model -> Browser.Document Msg
view { trips, current_user, event_title } =
    { title = "TourFax"
    , body =
        [ case current_user of
            Authenticated email ->
                div []
                    [ text email
                    , eventForm
                    , text ("New Event: " ++ event_title)
                    , ul [] (List.map viewTrip trips)
                    ]

            Unauthenticated ->
                text "Unauthenticated"
        ]
    }


viewTrip : Trip -> Html msg
viewTrip { name, events } =
    li []
        [ text name
        , ul [] (List.map viewEvent events)
        ]


eventForm =
    div []
        [ input [ onInput SetEventTitle ] []
        , button [ onClick SubmitEvent ] [ text "New Event" ]
        ]


viewEvent : Event -> Html msg
viewEvent event =
    li []
        [ case event of
            Dining title ->
                li [] [ text title ]

            Information title ->
                li [] [ text title ]

            Activity title price ->
                title_with_price title price

            Lodging title price ->
                title_with_price title price

            Flight title price ->
                title_with_price title price

            Transportation title price ->
                title_with_price title price

            Cruise title price ->
                title_with_price title price
        ]


title_with_price : String -> Maybe Int -> Html msg
title_with_price title price =
    li [] [ text title, text <| String.fromInt <| Maybe.withDefault 0 price ]


successView : Response -> Html Msg
successView successData =
    div []
        [ h1 [] [ text "Response" ]
        , successData |> PrintAny.view
        ]


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            "Http Error"


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
