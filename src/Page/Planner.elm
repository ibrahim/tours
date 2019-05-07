module Page.Planner exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Browser
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Mutations
import Page exposing (header, layout)
import Problem exposing (AppError(..), Problem(..), showProblems)
import Queries
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
import Session exposing (Session(..), viewer)
import Token exposing (toString)
import Types exposing (..)
import Username exposing (toString)
import Uuid exposing (Uuid(..), toString)
import Viewer exposing (cred, tokenStr, username)



-- Msg {{{
-- | GotCreateEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))


type Msg
    = GotUserTripResponse (RemoteData (Graphql.Http.Error UserTrip) UserTrip)
    | GotCreateEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotUpdatedEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotFetchEventResponse (RemoteData (Graphql.Http.Error UserTrip) UserTrip)
    | SetEventTitle String
    | SetEventType String
    | ShowEvent Uuid
    | SubmitEvent
    | ReportProblem Problem
    | ClearProblems
    | ClearEvent
    | UpdateEventFormTitle String
    | UpdateEventFormPrice String
    | SubmittedEventForm



-- Msg }}}
-- Model {{{


type EventType
    = EventType String


type Screen
    = ListAll
    | ListDay Int
    | Search String
    | ListByType EventType
    | EventForm Uuid


type alias Model =
    { session : Session
    , trip : RemoteData (Graphql.Http.Error UserTrip) UserTrip
    , event : RemoteData (Graphql.Http.Error Event) Event
    , trip_id : Uuid
    , event_id : Maybe Uuid
    , event_title : String
    , event_type : String
    , problems : List Problem
    , event_form : Maybe EventForm
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


default_event_type =
    "Event::Activity"


empty_event =
    { uuid = Nothing, title = "", price = Nothing, event_type = default_event_type }


initial_state : Session -> Uuid -> Model
initial_state session uuid =
    { session = session
    , trip = RemoteData.Loading
    , event = RemoteData.NotAsked
    , trip_id = uuid
    , event_id = Nothing
    , event_title = ""
    , event_type = ""
    , problems = []
    , event_form = Nothing
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

        GotCreateEventResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        GotUpdatedEventResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        GotFetchEventResponse response ->
            let
                populate_event_form =
                    case response of
                        RemoteData.Success { email, event, trips } ->
                            case event of
                                Just (Dining uuid type_ title) ->
                                    { uuid = Just uuid, title = title, price = Nothing, event_type = type_ }

                                Just (Information uuid type_ title) ->
                                    { uuid = Just uuid, title = title, price = Nothing, event_type = type_ }

                                Just (Activity uuid type_ title price) ->
                                    { uuid = Just uuid, title = title, price = price, event_type = type_ }

                                Just (Lodging uuid type_ title price) ->
                                    { uuid = Just uuid, title = title, price = price, event_type = type_ }

                                Just (Flight uuid type_ title price) ->
                                    { uuid = Just uuid, title = title, price = price, event_type = type_ }

                                Just (Transportation uuid type_ title price) ->
                                    { uuid = Just uuid, title = title, price = price, event_type = type_ }

                                Just (Cruise uuid type_ title price) ->
                                    { uuid = Just uuid, title = title, price = price, event_type = type_ }

                                Nothing ->
                                    empty_event

                        _ ->
                            empty_event

                resolve =
                    \data -> { model | trip = response, event_form = Just populate_event_form }

                reject =
                    \problems ->
                        { model
                            | problems = problems
                        }
            in
            Api.processQueryResponse response model resolve reject

        SubmitEvent ->
            ( model, saveEvent (CreateEvent { uuid = Nothing, title = model.event_title, event_type = model.event_type }) model.session model.trip_id )

        SetEventTitle title ->
            ( { model | event_title = title }, Cmd.none )

        SetEventType typ_ ->
            ( { model | event_type = typ_ }, Cmd.none )

        ReportProblem problem ->
            ( { model | problems = [ problem ] }, Cmd.none )

        ShowEvent uuid ->
            ( { model | event_id = Just uuid }, fetchEvent model.session model.trip_id (Just uuid) )

        ClearProblems ->
            ( { model | problems = [] }, Cmd.none )

        ClearEvent ->
            ( { model | event_id = Nothing, event = RemoteData.NotAsked }, Cmd.none )

        SubmittedEventForm ->
            case model.event_form of
                Just event_form ->
                    ( model, updateEvent (UpdateEvent event_form) model.session model.trip_id model.event_id )

                Nothing ->
                    ( { model | problems = [ Problem InvalidData "Event form is not valid" ] }, Cmd.none )

        UpdateEventFormTitle value ->
            let
                event_form =
                    Maybe.map (\o -> { o | title = value }) model.event_form
            in
            ( { model | event_form = event_form }, Cmd.none )

        UpdateEventFormPrice value ->
            let
                event_form =
                    Maybe.map (\o -> { o | price = String.toInt value }) model.event_form
            in
            ( { model | event_form = event_form }, Cmd.none )



-- Update }}}
-- view {{{
-- {{{ view


view : Model -> { title : String, content : List (Html Msg) }
view { trip, session, event_title, problems, event_id, event_form } =
    { title = "TourFax - Tour Planner"
    , content =
        Page.layout "Planner"
            [ div [ class "container" ]
                [ div [ class "column is-three-fifths" ]
                    (case session of
                        LoggedIn _ viewer ->
                            [ showProblems ClearProblems problems
                            , div []
                                [ case event_id of
                                    Just _ ->
                                        viewEvent trip event_form

                                    Nothing ->
                                        viewTrip trip
                                ]
                            ]

                        Guest _ ->
                            [ text "Unauthenticated" ]
                    )
                ]
            ]
    }



-- }}}
-- {{{ viewEvent


viewEvent : RemoteData (Graphql.Http.Error UserTrip) UserTrip -> Maybe EventForm -> Html Msg
viewEvent event event_form =
    viewRemoteData (viewEventForm (Maybe.withDefault empty_event event_form)) event



-- }}}
-- {{{ viewEventForm


viewEventForm : EventForm -> UserTrip -> Html Msg
viewEventForm event_form { event, trips } =
    let
        trip =
            case List.head trips of
                Just trip_ ->
                    { name = trip_.name, uuid = trip_.uuid }

                Nothing ->
                    { name = "N/A", uuid = "N/A" }

        priceField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Price" ]
                , div [ class "control has-icons-left has-icons-right" ]
                    [ input
                        [ class "input"
                        , type_ "text"
                        , placeholder "Price"
                        , onInput UpdateEventFormPrice
                        , value (Maybe.withDefault "" (Maybe.map String.fromInt event_form.price))
                        ]
                        []
                    , span [ class "icon is-small is-left" ]
                        [ i [ class "fas fa-user" ] [] ]
                    , span [ class "icon is-small is-right" ]
                        [ i [ class "fas fa-check" ] [] ]
                    ]
                ]

        renderPriceField =
            case event of
                Just (Dining _ _ _) ->
                    text ""

                Just (Information _ _ _) ->
                    text ""

                _ ->
                    priceField
    in
    Html.form [ onSubmit SubmittedEventForm ]
        [ div [ class "field" ]
            [ breadcrumb
                [ ( "Home", Route.href Route.Home )
                , ( trip.name, Route.href <| Route.Planner <| Uuid trip.uuid )
                , ( event_form.title, Route.href <| Route.Planner <| Uuid trip.uuid )
                ]
            , label [ class "label" ] [ text "title" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ input
                    [ class "input"
                    , placeholder "Title"
                    , onInput UpdateEventFormTitle
                    , value event_form.title
                    ]
                    []
                ]
            ]
        , renderPriceField
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button [ class "button is-link" ] [ text "Submit" ]
                ]
            , div [ class "control" ]
                [ button [ class "button is-text" ] [ text "Cancel" ]
                ]
            ]
        ]



-- }}}
-- {{{ viewItiniraryMenu


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



--}}}
--{{{ viewItinirary


viewItinirary trip =
    div []
        [ breadcrumb
            [ ( "Home", Route.href Route.Home )
            , ( trip.name, Route.href <| Route.Planner (Uuid trip.uuid) )
            ]
        , nav [ class "panel" ]
            (List.concat
                [ [ p [ class "panel-heading" ]
                        [ text trip.name ]
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
        ]



--}}}
-- {{{ viewTrip


viewTrip remote_response =
    let
        viewfunc user =
            div []
                (user.trips
                    |> List.map
                        (\item ->
                            viewItinirary item
                        )
                )
    in
    viewRemoteData viewfunc remote_response



--}}}
-- {{{ eventForm


eventForm =
    div [ class "columns" ]
        [ div [ class "column field" ]
            [ label [ class "label" ] [ text "New Event" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ input
                    [ onInput SetEventTitle
                    , class "input"
                    ]
                    []
                ]
            ]
        , div [ class "column field" ]
            [ label [ class "label" ] [ text "New Event" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ select [ onInput SetEventType, class "select" ] (List.map (\o -> option [ value o ] [ text (String.replace "Event::" "" o) ]) event_types) ]
            ]
        , div [ class "column field" ]
            [ div [ class "control has-icons-left has-icons-right" ]
                [ button [ onClick SubmitEvent, class "button" ] [ text "New Event" ] ]
            ]
        ]



-- }}}
-- {{{ viewEventItem


viewEventItem : Event -> Html Msg
viewEventItem event =
    case event of
        Dining uuid _ title ->
            title_with_price ShowEvent uuid title Nothing

        Information uuid _ title ->
            title_with_price ShowEvent uuid title Nothing

        Activity uuid _ title price ->
            title_with_price ShowEvent uuid title price

        Lodging uuid _ title price ->
            title_with_price ShowEvent uuid title price

        Flight uuid _ title price ->
            title_with_price ShowEvent uuid title price

        Transportation uuid _ title price ->
            title_with_price ShowEvent uuid title price

        Cruise uuid _ title price ->
            title_with_price ShowEvent uuid title price



--}}}
-- {{{ title_with_price


title_with_price : (Uuid -> msg) -> String -> String -> Maybe Int -> Html msg
title_with_price toMsg uuid title price =
    a
        [ class "panel-block is-active", href "", style "justify-content" "space-between", onClick (toMsg (Uuid uuid)) ]
        [ div []
            [ span [ class "panel-icon" ]
                [ i [ attribute "aria-hidden" "true", class "fas fa-book" ]
                    []
                ]
            , span [] [ text title ]
            ]
        , div [ class "tag is-small", style "align-self" "flex-end" ] [ text <| String.fromInt <| Maybe.withDefault 0 price ]
        ]



-- }}}
-- Shared View
-- {{{
-- {{{ modal


modal : msg -> List (Html msg) -> Html msg
modal toMsg content =
    div [ class <| "modal is-active" ]
        [ div [ class "modal-background" ]
            []
        , div [ class "modal-content" ] content
        , button [ attribute "aria-label" "close", class "modal-close is-large", onClick toMsg ]
            []
        ]



-- }}}
-- {{{ breadcrumb


breadcrumb : List ( String, Attribute msg ) -> Html msg
breadcrumb pages =
    let
        is_last idx =
            idx == List.length pages - 1

        is_active idx =
            if is_last idx then
                "is-active"

            else
                ""

        is_current idx =
            if is_last idx then
                attribute "aria-current" "page"

            else
                attribute "rel" "link"

        render idx ( title, attr_msg ) =
            li [ class (is_active idx) ]
                [ a [ is_current idx, attr_msg ]
                    [ text title ]
                ]

        -- , li [ class "is-active" ]
        --     [ a [ attribute "aria-current" "page", href "#" ]
        --         [ text "Breadcrumb" ]
        --     ]
    in
    nav [ attribute "aria-label" "breadcrumbs", class "breadcrumb" ]
        [ ul []
            (List.indexedMap render pages)
        ]



-- }}}
--{{{ viewRemoteData


viewRemoteData viewfunc subject =
    case subject of
        RemoteData.Success data ->
            viewfunc data

        RemoteData.Failure error ->
            p [] [ text "Http error" ]

        RemoteData.NotAsked ->
            text ""

        RemoteData.Loading ->
            li [] [ text "Loading.." ]



-- }}}
-- {{{ errorToString


errorToString : Graphql.Http.Error parsedData -> String
errorToString errorData =
    case errorData of
        Graphql.Http.GraphqlError _ graphqlErrors ->
            graphqlErrors
                |> List.map graphqlErrorToString
                |> String.join "\n"

        Graphql.Http.HttpError httpError ->
            "Http Error"



-- }}}
-- {{{ graphqlErrorToString


graphqlErrorToString : Graphql.Http.GraphqlError.GraphqlError -> String
graphqlErrorToString error =
    error.message



--}}}
-- }}}
-- View }}}
-- Cmd Msg {{{


saveEvent : EventInputs -> Session -> Uuid -> Cmd Msg
saveEvent event session trip_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.eventRequest event trip_id Nothing
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotCreateEventResponse
                    )

        Guest _ ->
            Cmd.none


updateEvent : EventInputs -> Session -> Uuid -> Maybe Uuid -> Cmd Msg
updateEvent event session trip_id event_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.eventRequest event trip_id event_id
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotUpdatedEventResponse
                    )

        Guest _ ->
            Cmd.none


fetchEvent : Session -> Uuid -> Maybe Uuid -> Cmd Msg
fetchEvent session trip_id event_id =
    case session of
        LoggedIn _ viewer ->
            Queries.userTripQuery trip_id event_id
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
            Queries.userTripQuery uuid Nothing
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
