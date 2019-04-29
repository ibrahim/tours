module Page.Planner exposing (Model, Msg(..), getUserTrips, init, subscriptions, toSession, update, view)

import Api
import Browser
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (Html, button, div, h1, input, li, p, pre, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Mutations
import Page exposing (header)
import PrintAny
import Queries
import RemoteData exposing (RemoteData)
import Session exposing (Session(..))
import Types exposing (..)



-- Msg {{{


type Msg
    = GotResponse (RemoteData (Graphql.Http.Error User) User)
    | GotEventResponse (RemoteData (Graphql.Http.Error (Maybe Event)) (Maybe Event))
    | SetEventTitle String
    | SubmitEvent



-- Msg }}}
-- Model {{{


type alias Model =
    { session : Session
    , userTrips : RemoteGraphqlResponse
    , current_user : Authentication
    , trips : List Trip
    , event_title : String
    }



-- Model }}}
-- init {{{


initial_state : Session -> Model
initial_state session =
    { session = session
    , current_user = Unauthenticated
    , trips = []
    , userTrips = RemoteData.Loading
    , event_title = ""
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( initial_state session, getUserTrips )



-- init }}}
-- update {{{


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

        SubmitEvent ->
            ( model, saveEvent { uuid = Nothing, title = model.event_title } )

        SetEventTitle title ->
            ( { model | event_title = title }, Cmd.none )

        GotEventResponse response ->
            ( model, Cmd.none )



-- Update }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view { trips, current_user, event_title } =
    { title = "TourFax"
    , content =
        [ div [] [ Page.header "Planner" ]
        , case current_user of
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



-- View }}}
-- Cmd Msg {{{


saveEvent : EventAttributes -> Cmd Msg
saveEvent event =
    Mutations.saveEventRequest Api.endpoint event
        |> Graphql.Http.send (RemoteData.fromResult >> GotEventResponse)


getUserTrips : Cmd Msg
getUserTrips =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest Api.endpoint
        |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)



-- CMD }}}
-- Sub Msg {{{


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Sub Msg }}}


toSession : Model -> Session
toSession model =
    model.session
