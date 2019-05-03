module Page.Planner exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Browser
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (..)
import Html.Attributes exposing (..)
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
    | ClearProblems



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

        ClearProblems ->
            ( { model | problems = [] }, Cmd.none )



-- Update }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view { trip, session, event_title, problems, event } =
    { title = "TourFax - Tour Planner"
    , content =
        [ div [] [ Page.header "Planner" ]
        , div [ class "columns " ]
            [ div [ class "column is-10 is-offset-1 columns is-mobile" ]
                [ div [ class "column is-two-fifths" ]
                    (case session of
                        LoggedIn _ viewer ->
                            [ text (Username.toString (Viewer.username viewer))
                            , showProblem problems
                            , eventForm
                            , text ("New Event: " ++ event_title)
                            , ul [] [ viewTrip trip ]
                            ]

                        Guest _ ->
                            [ text "Unauthenticated" ]
                    )
                , div [ class "column is-three-fifths" ]
                    [ viewEvent event
                    ]
                ]
            ]
        ]
    }



-- showEvent event =
--   div [] [
--       case event of
--
--     ]


showProblem problems =
    ul [] (List.map viewProblem problems)


container content =
    div [ class "notification is-danger" ]
        [ button [ class "delete", onClick ClearProblems ] []
        , div [] content
        ]


viewProblem problem =
    case problem of
        Problem AuthenticationError message ->
            container [ a [ href "#/login" ] [ text <| message ++ ". Click here to login." ] ]

        Problem _ message ->
            container [ p [] [ text message ] ]


viewItiniraryMenu trip =
    aside [ class "menu" ]
        [ p [ class "menu-label" ]
            [ text "General  " ]
        , ul [ class "menu-list" ]
            (List.map viewEventItem trip.events)
        , p [ class "menu-label" ]
            [ text "Administration  " ]
        , ul [ class "menu-list" ]
            [ li []
                [ a []
                    [ text "Team Settings" ]
                ]
            , li []
                [ a [ class "is-active" ]
                    [ text "Manage Your Team" ]
                , ul []
                    [ li []
                        [ a []
                            [ text "Members" ]
                        ]
                    , li []
                        [ a []
                            [ text "Plugins" ]
                        ]
                    , li []
                        [ a []
                            [ text "Add a member" ]
                        ]
                    ]
                ]
            , li []
                [ a []
                    [ text "Invitations" ]
                ]
            , li []
                [ a []
                    [ text "Cloud Storage Environment Settings" ]
                ]
            , li []
                [ a []
                    [ text "Authentication" ]
                ]
            ]
        , p [ class "menu-label" ]
            [ text "Transactions  " ]
        , ul [ class "menu-list" ]
            [ li []
                [ a []
                    [ text "Payments" ]
                ]
            , li []
                [ a []
                    [ text "Transfers" ]
                ]
            , li []
                [ a []
                    [ text "Balance" ]
                ]
            ]
        ]


viewItinirary trip =
    nav [ class "panel" ]
        (List.concat
            [ [ p [ class "panel-heading" ]
                    [ text "repositories  " ]
              , div [ class "panel-block" ]
                    [ p [ class "control has-icons-left" ]
                        [ input [ class "input is-small", placeholder "search", type_ "text" ]
                            []
                        , span [ class "icon is-small is-left" ]
                            [ i [ attribute "aria-hidden" "true", class "fas fa-search" ]
                                []
                            ]
                        ]
                    ]
              , p [ class "panel-tabs" ]
                    [ a [ class "is-active" ]
                        [ text "all" ]
                    , a []
                        [ text "public" ]
                    , a []
                        [ text "private" ]
                    , a []
                        [ text "sources" ]
                    , a []
                        [ text "forks" ]
                    ]
              ]
            , List.map viewEventItem trip.events
            , [ div [ class "panel-block" ]
                    [ button [ class "button is-link is-outlined is-fullwidth" ]
                        [ text "reset all filters    " ]
                    ]
              ]
            ]
        )


viewEvent event =
    case event of
        RemoteData.Success event_ ->
            viewEventItem event_

        RemoteData.Failure error ->
            p [] [ text ("Http error " ++ Debug.toString error) ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            li [] [ text "Loading.." ]


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
                                    , viewItinirary item
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


viewEventItem : Event -> Html Msg
viewEventItem event =
    case event of
        Dining uuid title ->
            title_with_price uuid title Nothing

        Information uuid title ->
            title_with_price uuid title Nothing

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


title_with_price uuid title price =
    a
        [ class "panel-block is-active", href "", onClick (ShowEvent (Uuid uuid)) ]
        [ span [ class "panel-icon" ]
            [ i [ attribute "aria-hidden" "true", class "fas fa-book" ]
                []
            ]
        , text title
        , text <| String.fromInt <| Maybe.withDefault 0 price
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
