module Page.Planner exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Browser
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (Html, a, button, div, h1, input, li, option, p, pre, select, text, ul)
import Html.Attributes exposing (href, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Mutations
import Page exposing (header)
import Queries
import RemoteData exposing (RemoteData)
import Session exposing (Session(..), viewer)
import Token exposing (toString)
import Types exposing (..)
import Username exposing (toString)
import Uuid exposing (Uuid(..), toString)
import Viewer exposing (cred, tokenStr, username)



-- Msg {{{
-- | GotEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))


type Msg
    = GotUserTripResponse (RemoteData (Graphql.Http.Error UserTrip) UserTrip)
    | GotEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotFetchEventResponse (RemoteData (Graphql.Http.Error Event) Event)
    | SetEventTitle String
    | SetEventType String
    | ShowEvent Uuid
    | SubmitEvent
    | ReportProblem Problem



-- Msg }}}
-- Model {{{


type alias Model =
    { session : Session
    , trip : RemoteData (Graphql.Http.Error UserTrip) UserTrip
    , event : RemoteData (Graphql.Http.Error Event) Event
    , trip_id : Uuid
    , event_title : String
    , event_type : String
    , event_id : Maybe Uuid
    , problems : List Problem
    }



-- Model }}}
-- init {{{


event_types : List String
event_types =
    [ "Event::Activity"
    , "Event::Lodging"
    , "Event::Flight"
    , "Event::Transportation"
    , "Event::Cruise"
    , "Event::Information"
    , "Event::Dining"
    ]


initial_state : Session -> Uuid -> Model
initial_state session uuid =
    { session = session
    , trip = RemoteData.Loading
    , event = RemoteData.NotAsked
    , trip_id = uuid
    , event_title = ""
    , event_type = ""
    , event_id = Nothing
    , problems = []
    }


init : Session -> Uuid -> ( Model, Cmd Msg )
init session uuid =
    ( initial_state session uuid, getUserTrip session uuid )



-- init }}}
-- update {{{


update msg model =
    case msg of
        GotUserTripResponse response ->
            let
                resolve =
                    \data -> { model | trip = response }

                reject =
                    \problems ->
                        { model
                            | problems = problems
                            , trip = RemoteData.NotAsked
                        }
            in
            Api.processQueryResponse response model resolve reject

        GotFetchEventResponse response ->
            let
                resolve =
                    \data -> { model | event = response }

                reject =
                    \problems ->
                        { model
                            | problems = problems
                            , event = RemoteData.NotAsked
                        }
            in
            Api.processQueryResponse response model resolve reject

        SubmitEvent ->
            ( model, saveEvent { uuid = Nothing, title = model.event_title, event_type = model.event_type } model.session model.trip_id )

        SetEventTitle title ->
            ( { model | event_title = title }, Cmd.none )

        SetEventType typ_ ->
            ( { model | event_type = typ_ }, Cmd.none )

        GotEventResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        ReportProblem problem ->
            ( { model | problems = [ problem ] }, Cmd.none )

        ShowEvent uuid ->
            ( { model | event_id = Just uuid }, fetchEvent model.session uuid )



-- Update }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view { trip, session, event_title, problems } =
    { title = "TourFax"
    , content =
        [ div [] [ Page.header "Planner" ]
        , case session of
            LoggedIn _ viewer ->
                div []
                    [ div []
                        [ text (Username.toString (Viewer.username viewer))
                        , showProblem problems
                        , eventForm
                        , text ("New Event: " ++ event_title)
                        , ul [] [ viewTrip trip ]
                        ]
                    ]

            Guest _ ->
                text "Unauthenticated"
        ]
    }


showProblem problems =
    ul [] (List.map viewProblem problems)


container content =
    div [] content


viewProblem problem =
    case problem of
        Problem AuthenticationError message ->
            container [ a [ href "#/login" ] [ text <| message ++ ". Click here to login." ] ]

        Problem _ message ->
            container [ p [] [ text message ] ]


viewTrip trip =
    case trip of
        RemoteData.Success user ->
            li []
                [ text user.email
                , ul []
                    (user.trips
                        |> List.map
                            (\item ->
                                li
                                    []
                                    [ text item.name
                                    , ul [] (List.map viewEvent item.events)
                                    ]
                            )
                    )
                ]

        RemoteData.Failure error ->
            p [] [ text ("Http error " ++ Debug.toString error) ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            li [] [ text "Loading.." ]


eventForm =
    div []
        [ input [ onInput SetEventTitle ] []
        , select [ onInput SetEventType ] (List.map (\o -> option [ value o ] [ text (String.replace "Event::" "" o) ]) event_types)
        , button [ onClick SubmitEvent ] [ text "New Event" ]
        ]


viewEvent : Event -> Html Msg
viewEvent event =
    li []
        [ case event of
            Dining uuid title ->
                li [] [ a [] [ text title ] ]

            Information uuid title ->
                li [] [ a [] [ text title ] ]

            Activity uuid title price ->
                title_with_price uuid title price

            Lodging uuid title price ->
                title_with_price uuid title price

            Flight uuid title price ->
                title_with_price uuid title price

            Transportation uuid title price ->
                title_with_price uuid title price

            Cruise uuid title price ->
                title_with_price uuid title price
        ]


title_with_price uuid title price =
    li [] [ a [ href "", onClick (ShowEvent (Uuid uuid)) ] [ text title, text <| String.fromInt <| Maybe.withDefault 0 price ] ]


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


saveEvent : EventAttributes -> Session -> Uuid -> Cmd Msg
saveEvent event session trip_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.saveEventRequest Api.endpoint event trip_id
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotEventResponse
                    )

        Guest _ ->
            Cmd.none


fetchEvent : Session -> Uuid -> Cmd Msg
fetchEvent session uuid =
    case session of
        LoggedIn _ viewer ->
            Queries.eventQuery uuid
                |> Graphql.Http.queryRequest Api.endpoint
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult >> GotFetchEventResponse)

        Guest _ ->
            Cmd.none


getUserTrip : Session -> Uuid -> Cmd Msg
getUserTrip session uuid =
    case session of
        LoggedIn _ viewer ->
            Queries.userTripQuery uuid
                |> Graphql.Http.queryRequest Api.endpoint
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult >> GotUserTripResponse)

        Guest _ ->
            Cmd.none



-- CMD }}}
-- Sub Msg {{{


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Sub Msg }}}


toSession : Model -> Session
toSession model =
    model.session
