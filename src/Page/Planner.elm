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
    | GotFetchEventResponse (RemoteData (Graphql.Http.Error UserTrip) UserTrip)
    | GotCreateEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotCreateSectionResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotUpdateEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | ShowEvent EventId
    | Goto Screen
    | CreateNewEvent EventType SectionId
    | ReportProblem Problem
    | ClearProblems
    | UpdateEventForm EventForm
    | SubmittedEventForm EventId EventForm
    | SubmitSection String



-- Msg }}}
-- Model {{{


type EventType
    = EventType String


type Screen
    = ListAll
    | ListSections
    | ListSection SectionId
    | Search String
    | ListByType EventType
    | EditEvent EventId EventForm
    | NewSection String


type alias Model =
    { session : Session
    , trip : RemoteData (Graphql.Http.Error UserTrip) UserTrip
    , trip_id : TripId
    , problems : List Problem
    , screen : Screen
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
    , trip_id = uuid
    , problems = []
    , screen = ListSections
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

        GotCreateSectionResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip, screen = ListAll }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        GotUpdateEventResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        GotFetchEventResponse response ->
            let
                resolve data =
                    case data of
                        { email, event, trips } ->
                            case event of
                                Just (Dining uuid section_id type_ title) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = Nothing
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Information uuid section_id type_ title) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = Nothing
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Activity uuid section_id type_ title price) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = price
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Lodging uuid section_id type_ title price) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = price
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Flight uuid section_id type_ title price) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = price
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Transportation section_id uuid type_ title price) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = price
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Just (Cruise uuid section_id type_ title price) ->
                                    { model
                                        | screen =
                                            EditEvent (Uuid uuid)
                                                { uuid = uuid
                                                , section_id = section_id
                                                , title = title
                                                , price = price
                                                , event_type = type_
                                                }
                                        , trip = response
                                    }

                                Nothing ->
                                    model

                reject =
                    \problems ->
                        { model
                            | problems = problems
                        }
            in
            Api.processQueryResponse response model resolve reject

        CreateNewEvent (EventType event_type) section_id ->
            ( model, saveEvent (CreateEvent { title = "", event_type = event_type }) model.session model.trip_id section_id )

        ReportProblem problem ->
            ( { model | problems = [ problem ] }, Cmd.none )

        Goto screen ->
            ( { model | screen = screen }, Cmd.none )

        ShowEvent uuid ->
            ( model, fetchEvent model.session model.trip_id (Just uuid) )

        ClearProblems ->
            ( { model | problems = [] }, Cmd.none )

        SubmittedEventForm event_id event_form ->
            let
                section_id =
                    Uuid event_form.section_id
            in
            ( model, updateEvent (UpdateEvent event_form) model.session model.trip_id section_id (Just event_id) )

        UpdateEventForm event_form ->
            ( { model | screen = EditEvent (Uuid event_form.uuid) event_form }, Cmd.none )

        SubmitSection title ->
            let
                section_input =
                    CreateSection { title = title, trip_id = Uuid.toString model.trip_id }
            in
            ( model, saveSection section_input model.session model.trip_id )



-- Update }}}
-- view {{{
-- {{{ view


view : Model -> { title : String, content : List (Html Msg) }
view { trip, session, problems, screen } =
    { title = "TourFax - Tour Planner"
    , content =
        Page.layout "Planner"
            [ div [ class "container" ]
                [ div [ class "column is-three-fifths" ]
                    [ div []
                        [ showProblems ClearProblems problems
                        , div []
                            [ case session of
                                LoggedIn _ viewer ->
                                    case trip of
                                        RemoteData.Success user ->
                                            case List.head user.trips of
                                                Just trip_ ->
                                                    case screen of
                                                        ListAll ->
                                                            viewTrip screen trip

                                                        EditEvent event_id event_form ->
                                                            viewEvent trip event_form

                                                        ListByType event_type ->
                                                            viewTrip screen trip

                                                        ListSection section_id ->
                                                            let
                                                                section =
                                                                    getSection section_id trip_.sections

                                                                trips_events =
                                                                    List.concatMap (\o -> eventsToRecords o.events) user.trips

                                                                events =
                                                                    List.concat
                                                                        [ []
                                                                        , trips_events
                                                                        ]
                                                            in
                                                            case section of
                                                                Just section_ ->
                                                                    viewSectionEvents section_ events

                                                                Nothing ->
                                                                    text "Section Not Found"

                                                        ListSections ->
                                                            viewTrip screen trip

                                                        Search query ->
                                                            viewTrip screen trip

                                                        NewSection title ->
                                                            sectionForm title

                                                Nothing ->
                                                    text "Trip Not Found"

                                        _ ->
                                            text ""

                                Guest _ ->
                                    text "Unauthenticated"
                            ]
                        ]
                    ]
                ]
            ]
    }



-- }}}
-- {{{ viewEvent


viewEvent : RemoteData (Graphql.Http.Error UserTrip) UserTrip -> EventForm -> Html Msg
viewEvent event event_form =
    viewData (viewEventForm event_form) event



-- }}}
-- {{{ viewEventForm


viewEventForm : EventForm -> UserTrip -> Html Msg
viewEventForm event_form { event, trips } =
    let
        section_id =
            Uuid event_form.section_id

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
                        , onInput (\o -> UpdateEventForm { event_form | price = String.toInt o })
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
                Just (Dining _ _ _ _) ->
                    text ""

                Just (Information _ _ _ _) ->
                    text ""

                _ ->
                    priceField
    in
    Html.form [ onSubmit (SubmittedEventForm (Uuid event_form.uuid) event_form) ]
        [ div [ class "field" ]
            [ breadcrumb
                [ ( "Home", Route.href Route.Home )
                , ( trip.name, Route.href <| Route.Planner <| Uuid trip.uuid )
                , ( Maybe.withDefault "..." event_form.title, Route.href <| Route.Planner <| Uuid trip.uuid )
                ]
            , label [ class "label" ] [ text "title" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ input
                    [ class "input"
                    , placeholder "Title"
                    , onInput (\o -> UpdateEventForm { event_form | title = Just o })
                    , value <| Maybe.withDefault "..." event_form.title
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


viewItinirary screen trip =
    let
        events =
            eventsToRecords trip.events
    in
    div []
        [ breadcrumb
            [ ( "Home", Route.href Route.Home )
            , ( trip.name, Route.href <| Route.Planner (Uuid trip.uuid) )
            ]
        , div []
            (case screen of
                ListSections ->
                    viewSections trip.sections

                ListAll ->
                    List.map viewEventItem events

                ListSection section_id ->
                    List.map viewEventItem <|
                        List.filter (\event -> event.section_id == Uuid.toString section_id) (eventsToRecords trip.events)

                _ ->
                    [ text "Other Tabs" ]
            )
        , div [ class "panel-block" ]
            [ button [ class "button is-link is-outlined is-fullwidth", onClick (Goto (NewSection "")) ]
                [ text "Add New Section" ]
            ]
        ]



--}}}
-- {{{ viewSectionEvents


viewEventFull event =
    div [ class "event-full" ]
        [ div [ class "title" ]
            [ text <| Maybe.withDefault "..." event.title ]
        , p [ class "desc" ]
            [ text "events notes" ]
        ]


viewSectionEvents section events =
    let
        sectionHeader =
            div [ class "section-hdr level" ]
                [ div [ class "level-left" ] [ span [ class "level-item ttl" ] [ text section.title ] ]
                , div [ class "level-right" ]
                    [ p [ class "level-item" ]
                        [ createEventButton section.uuid ]
                    ]
                ]
    in
    div []
        [ sectionHeader
        , div [] (List.map viewEventFull events)
        ]



-- }}}
-- {{{ viewSections


viewSections sections =
    let
        viewSection { uuid, title } =
            div
                [ class "section-summary"
                ]
                [ div [ class "section-body" ]
                    [ div [ class "title" ]
                        [ text title ]
                    , div [ class "events-icons" ]
                        (List.repeat 7 (a [ class "event", onClick <| Goto <| ListSection <| Uuid uuid ] []))
                    ]
                , div [ class "more" ]
                    [ a
                        [ href "", onClick <| Goto <| ListSection <| Uuid uuid ]
                        [ span [ class "fas fa-chevron-right" ] [] ]
                    ]
                ]
    in
    List.map viewSection sections



--}}}
-- {{{ viewTrip


viewTrip screen remote_response =
    let
        viewfunc user =
            div []
                (user.trips
                    |> List.map
                        (\item ->
                            viewItinirary screen item
                        )
                )
    in
    viewData viewfunc remote_response



--}}}
-- {{{ eventsToRecords
-- eventsToRecords : List Event -> List EventForm


eventsToRecords events =
    let
        convert event =
            case event of
                Dining uuid section_id event_type title ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = Nothing }

                Information uuid section_id event_type title ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = Nothing }

                Activity uuid section_id event_type title price ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = price }

                Lodging uuid section_id event_type title price ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = price }

                Flight uuid section_id event_type title price ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = price }

                Transportation uuid section_id event_type title price ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = price }

                Cruise uuid section_id event_type title price ->
                    { uuid = uuid, section_id = section_id, event_type = event_type, title = title, price = price }
    in
    List.map convert events



--}}}
-- {{{ viewEventItem


viewEventItem : EventForm -> Html Msg
viewEventItem { title, uuid, section_id, price } =
    a
        [ class "panel-block is-active", href "", style "justify-content" "space-between", onClick (ShowEvent (Uuid uuid)) ]
        [ div []
            [ span [ class "panel-icon" ]
                [ i [ attribute "aria-hidden" "true", class "fas fa-book" ]
                    []
                ]
            , span [] [ text <| Maybe.withDefault "..." title ]
            ]
        , div [ class "tag is-small", style "align-self" "flex-end" ] [ text <| String.fromInt <| Maybe.withDefault 0 price ]
        ]



--}}}
-- {{{ sectionForm


sectionForm : String -> Html Msg
sectionForm title =
    Html.form [ onSubmit (SubmitSection title) ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Section" ]
            , div [ class "control has-icons-left" ]
                [ input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Section / Day name"
                    , onInput (\o -> Goto (NewSection o))
                    , value title
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-book" ] [] ]
                ]
            ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button [ class "button is-link" ] [ text "Create Section" ]
                ]
            , div [ class "control" ]
                [ button [ class "button is-text" ] [ text "Cancel" ]
                ]
            ]
        ]



-- }}}
-- {{{ Tabs


tab_title : SectionId -> TripWithEvents -> String
tab_title section_id trip =
    let
        section =
            List.head <| List.filter (\item -> item.uuid == Uuid.toString section_id) trip.sections
    in
    Maybe.withDefault "Section" <| Maybe.map (\o -> o.title) section


tabs : Screen -> TripWithEvents -> Html Msg
tabs screen trip =
    let
        section_tab =
            case screen of
                ListSection section_id ->
                    [ li [ class "is-active" ] [ a [] [ text (tab_title section_id trip) ] ] ]

                _ ->
                    []

        is_active tab =
            case screen of
                ListSection _ ->
                    if tab == "section" then
                        "is-active"

                    else
                        ""

                ListSections ->
                    if tab == "sections" then
                        "is-active"

                    else
                        ""

                ListAll ->
                    if tab == "all" then
                        "is-active"

                    else
                        ""

                Search _ ->
                    if tab == "all" then
                        "is-active"

                    else
                        ""

                _ ->
                    ""
    in
    div [ class "tabs" ]
        [ ul []
            (List.concat
                [ section_tab
                , [ li [ class (is_active "sections") ]
                        [ a
                            [ href ""
                            , onClick <| Goto ListSections
                            ]
                            [ text "Sections" ]
                        ]
                  , li [ class (is_active "all") ]
                        [ a
                            [ href ""
                            , onClick <| Goto ListAll
                            ]
                            [ text "All Events" ]
                        ]
                  , li [ class (is_active "search") ]
                        [ a [ href "" ]
                            [ text "Search" ]
                        ]
                  ]
                ]
            )
        ]



-- }}}
-- {{{ createEventButton


createEventButton section_id =
    let
        event_name o =
            String.slice 7 -1 (o ++ " ")
    in
    div
        [ class "dropdown is-hoverable create-event-menu" ]
        [ div [ class "dropdown-trigger " ]
            [ button [ attribute "aria-controls" "dropdown-menu4", attribute "aria-haspopup" "true", class "button  is-primary" ]
                [ span [ class "icon is-small" ]
                    [ i [ attribute "aria-hidden" "true", class "fas fa-plus" ]
                        []
                    ]
                , span []
                    [ text "Add Event" ]
                ]
            ]
        , div [ class "dropdown-menu", id "dropdown-menu4", attribute "role" "menu" ]
            [ div [ class "dropdown-content" ]
                (List.map
                    (\o ->
                        div [ class "dropdown-item" ]
                            [ a [ href "", onClick (CreateNewEvent (EventType o) (Uuid section_id)) ]
                                [ span [ class ("icon fa-" ++ (String.toLower <| event_name o)) ] []
                                , span [] [ text <| event_name o ]
                                ]
                            ]
                    )
                    event_types
                )
            ]
        ]



-- }}}
-- Shared View
-- {{{
-- {{{ getSection


getSection section_id sections =
    List.head <| List.filter (\item -> item.uuid == Uuid.toString section_id) sections



--}}}
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
--{{{ viewData


viewData viewfunc subject =
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


saveSection : SectionInputs -> Session -> TripId -> Cmd Msg
saveSection section session trip_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.sectionRequest section trip_id Nothing
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotCreateSectionResponse
                    )

        Guest _ ->
            Cmd.none


saveEvent : EventInputs -> Session -> TripId -> SectionId -> Cmd Msg
saveEvent event session trip_id section_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.eventRequest event trip_id section_id Nothing
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotCreateEventResponse
                    )

        Guest _ ->
            Cmd.none


updateEvent : EventInputs -> Session -> TripId -> SectionId -> Maybe Uuid -> Cmd Msg
updateEvent event session trip_id section_id event_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.eventRequest event trip_id section_id event_id
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotUpdateEventResponse
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
