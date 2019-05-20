module Page.Planner exposing (Model, Msg(..), init, subscriptions, toSession, update, view)

import Api
import Browser
import Debug exposing (log)
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onBlur, onClick, onInput, onSubmit)
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
    | GotDeleteEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotSaveSectionResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | GotUpdateEventResponse (RemoteData (Graphql.Http.Error (Maybe UserTrip)) (Maybe UserTrip))
    | ShowEvent EventId
    | DeleteEvent
    | ConfirmedDeleteEvent EventId
    | Goto Screen
    | CreateNewEvent EventType SectionId
    | ReportProblem Problem
    | ClearProblems
    | UpdateEventForm EventForm
    | SubmittedEventForm EventId EventForm
    | SubmitSection (Maybe SectionId) String
    | NoOp



-- Msg }}}
-- Model {{{


type alias Model =
    { session : Session
    , trip : RemoteData (Graphql.Http.Error UserTrip) UserTrip
    , trip_id : TripId
    , problems : List Problem
    , screen : Screen
    }


type EventType
    = EventType String


type Deletion
    = UnconfirmedDelete
    | ConfirmedDelete


type EventFormTabs
    = DetailsTab
    | PhotosTab
    | FilesTab
    | PlacesTab
    | InfoTab


type ListSectionMode
    = ListEvents
    | EditSection String


type Screen
    = ListAll
    | ListSections
    | ListSection SectionId ListSectionMode
    | Search String
    | ListByType EventType
    | EditEvent EventId EventForm Deletion EventFormTabs
    | NewSection String



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
        -- {{{ GotUserTripResponse
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

        -- }}}
        --{{{ GotCreateEventResponse
        GotCreateEventResponse response ->
            let
                resolve =
                    \data -> { model | trip = RemoteData.map (always data) model.trip }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        --}}}
        --{{{ GotDeleteEventResponse
        GotDeleteEventResponse response ->
            let
                section_id =
                    case model.screen of
                        EditEvent _ event_form _ _ ->
                            Just <| .section_id <| event_form

                        _ ->
                            Nothing

                next_screen =
                    case section_id of
                        Just uuid ->
                            ListSection (Uuid uuid) ListEvents

                        Nothing ->
                            ListSections

                resolve =
                    \data ->
                        { model
                            | trip = RemoteData.map (always data) model.trip
                            , screen = next_screen
                        }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        --}}}
        --{{{ GotSaveSectionResponse
        GotSaveSectionResponse response ->
            let
                resolve =
                    \data ->
                        { model
                            | trip = RemoteData.map (always data) model.trip
                            , screen = ListSections
                        }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        --}}}
        --{{{GotUpdateEventResponse
        GotUpdateEventResponse response ->
            let
                screen =
                    model.screen

                section_id =
                    case screen of
                        EditEvent _ event_form _ _ ->
                            Just <| .section_id <| event_form

                        _ ->
                            Nothing

                new_screen =
                    case section_id of
                        Just uuid ->
                            ListSection (Uuid uuid) ListEvents

                        Nothing ->
                            ListSections

                resolve =
                    \data ->
                        { model
                            | screen = new_screen
                            , trip = RemoteData.map (always data) model.trip
                        }

                reject =
                    \problems -> { model | problems = problems }
            in
            Api.processMutationResponse response model resolve reject

        --}}}
        --{{{ GotFetchEventResponse
        GotFetchEventResponse response ->
            let
                resolve data =
                    case data of
                        { email, event, trips } ->
                            case event of
                                Just event_form ->
                                    let
                                        event_record =
                                            event_to_record event_form

                                        event_uuid =
                                            .uuid event_record
                                    in
                                    { model
                                        | screen =
                                            EditEvent (Uuid event_uuid) event_record UnconfirmedDelete DetailsTab
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

        --}}}
        --{{{ CreateNewEvent
        CreateNewEvent (EventType event_type) section_id ->
            ( model, saveEvent (CreateEvent { event_type = event_type }) model.session model.trip_id section_id )

        --}}}
        --{{{ReportProblem
        ReportProblem problem ->
            ( { model | problems = [ problem ] }, Cmd.none )

        --}}}
        --{{{Goto
        Goto screen ->
            ( { model | screen = screen }, Cmd.none )

        --}}}
        --{{{ShowEvent
        ShowEvent uuid ->
            ( model, fetchEvent model.session model.trip_id (Just uuid) )

        --}}}
        --{{{ClearProblems
        ClearProblems ->
            ( { model | problems = [] }, Cmd.none )

        --}}}
        --{{{SubmittedEventForm
        SubmittedEventForm event_id event_form ->
            let
                section_id =
                    Uuid event_form.section_id
            in
            ( model, updateEvent (UpdateEvent event_form) model.session model.trip_id section_id (Just event_id) )

        --}}}
        --{{{DeleteEvent
        DeleteEvent ->
            let
                screen =
                    model.screen

                new_screen =
                    case screen of
                        EditEvent uuid event_form _ tab ->
                            EditEvent uuid event_form ConfirmedDelete tab

                        same ->
                            same
            in
            ( { model | screen = new_screen }, Cmd.none )

        --}}}
        --{{{ConfirmedDeleteEvent
        ConfirmedDeleteEvent event_id ->
            ( model, deleteEvent model.session model.trip_id event_id )

        --}}}
        --{{{UpdateEventForm
        UpdateEventForm event_form ->
            ( { model | screen = EditEvent (Uuid event_form.uuid) event_form UnconfirmedDelete DetailsTab }, Cmd.none )

        --}}}
        --{{{SubmitSection
        SubmitSection section_id title ->
            let
                section_input =
                    case section_id of
                        Just section_id_ ->
                            UpdateSection
                                { title = title
                                , uuid = section_id_
                                , trip_id = model.trip_id
                                , is_day = Nothing
                                , day_date = Nothing
                                , day_order = Nothing
                                }

                        Nothing ->
                            CreateSection { title = title, trip_id = model.trip_id }
            in
            ( model, saveSection section_input model.session model.trip_id )

        --}}}
        NoOp ->
            ( model, Cmd.none )



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

                                                        EditEvent event_id event_form _ _ ->
                                                            viewEventForm screen event_form trip_

                                                        ListByType event_type ->
                                                            viewTrip screen trip

                                                        ListSection section_id _ ->
                                                            let
                                                                section =
                                                                    getSection section_id trip_.sections

                                                                events =
                                                                    List.concatMap (\o -> eventsToRecords o.events) user.trips
                                                            in
                                                            case section of
                                                                Just section_ ->
                                                                    viewListSection screen section_ events trip_

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
-- {{{ viewEventForm


viewEventForm : Screen -> EventForm -> TripWithEvents -> Html Msg
viewEventForm screen form trip =
    let
        section =
            getSection (Uuid form.section_id) trip.sections

        section_title =
            case section of
                Just section_ ->
                    section_.title

                Nothing ->
                    "Section"

        --{{{ float_to_time_input
        float_to_time_input time_f =
            let
                lzero str =
                    if String.length str == 1 then
                        "0" ++ str

                    else if str == "" then
                        "00"

                    else
                        str

                rzero str =
                    if String.length str == 1 then
                        str ++ "0"

                    else if str == "" then
                        "00"

                    else
                        str

                hour_min_pair s =
                    case s of
                        [ a ] ->
                            [ lzero a, "00" ]

                        a :: b :: _ ->
                            [ lzero a, rzero b ]

                        [] ->
                            [ "00", "00" ]

                time_str =
                    case Maybe.map String.fromFloat time_f of
                        Just t ->
                            t
                                |> String.split "."
                                |> hour_min_pair
                                |> String.join ":"

                        Nothing ->
                            ""
            in
            time_str

        --}}}
        -- {{{ notesField
        notesField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Notes" ]
                , textarea
                    [ class "input textarea"
                    , rows 6
                    , onInput (\o -> UpdateEventForm { form | notes = Just o })
                    , value (Maybe.withDefault "" form.notes)
                    ]
                    []
                ]

        -- }}}
        -- {{{ priceField
        priceField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Price" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Price"
                    , onInput (\o -> UpdateEventForm { form | price = String.toInt o })
                    , value (Maybe.withDefault "" (Maybe.map String.fromInt form.price))
                    ]
                    []
                ]

        -- }}}
        -- {{{ currencyField
        currencyField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Currency" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , placeholder "Currency"
                    , onInput (\o -> UpdateEventForm { form | currency = Just o })
                    , value (Maybe.withDefault "" form.currency)
                    ]
                    []
                ]

        -- }}}
        -- {{{ booked_throughField
        booked_throughField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Booked Through" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | booked_through = Just o })
                    , value (Maybe.withDefault "" form.booked_through)
                    ]
                    []
                ]

        -- }}}
        -- {{{ confirmationField
        confirmationField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Confirmation" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | confirmation = Just o })
                    , value (Maybe.withDefault "" form.confirmation)
                    ]
                    []
                ]

        -- }}}
        -- {{{ airlineField
        airlineField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Airline" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | airline = Just o })
                    , value (Maybe.withDefault "" form.airline)
                    ]
                    []
                ]

        -- }}}
        -- {{{ flight_numberField
        flight_numberField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Flight Number" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | flight_number = Just o })
                    , value (Maybe.withDefault "" form.flight_number)
                    ]
                    []
                ]

        -- }}}
        -- {{{ terminalField
        terminalField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Terminal" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | terminal = Just o })
                    , value (Maybe.withDefault "" form.terminal)
                    ]
                    []
                ]

        -- }}}
        -- {{{ gateField
        gateField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Gate" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | gate = Just o })
                    , value (Maybe.withDefault "" form.gate)
                    ]
                    []
                ]

        -- }}}
        -- {{{ phone_numberField
        phone_numberField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Phone Number" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | phone_number = Just o })
                    , value (Maybe.withDefault "" form.phone_number)
                    ]
                    []
                ]

        -- }}}
        -- {{{ starts_atField
        starts_atField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Starts At" ]
                , input
                    [ class "input time"
                    , type_ "time"
                    , onInput
                        (\o ->
                            UpdateEventForm { form | starts_at = String.toFloat <| String.replace ":" "." o }
                        )
                    , value <| log "starts val" (float_to_time_input form.starts_at)
                    ]
                    []
                ]

        -- }}}
        -- {{{ ends_atField
        ends_atField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Ends At" ]
                , input
                    [ class "input time"
                    , type_ "time"
                    , onInput
                        (\o ->
                            UpdateEventForm { form | ends_at = String.toFloat <| String.replace ":" "." o }
                        )
                    , value <| log "end value " (float_to_time_input form.ends_at)
                    ]
                    []
                ]

        -- }}}
        -- {{{ durationField
        durationField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Duration" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | duration = String.toInt o })
                    , value (Maybe.withDefault "" (Maybe.map String.fromInt form.duration))
                    ]
                    []
                ]

        -- }}}
        -- {{{ cabin_typeField
        cabin_typeField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Cabin Type" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | cabin_type = Just o })
                    , value (Maybe.withDefault "" form.cabin_type)
                    ]
                    []
                ]

        -- }}}
        -- {{{ cabin_numberField
        cabin_numberField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Cabin Number" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | cabin_number = Just o })
                    , value (Maybe.withDefault "" form.cabin_number)
                    ]
                    []
                ]

        -- }}}
        -- {{{ providerField
        providerField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Provider" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | provider = Just o })
                    , value (Maybe.withDefault "" form.provider)
                    ]
                    []
                ]

        -- }}}
        -- {{{ info_typeField
        info_typeField =
            div [ class "field" ]
                [ label [ class "label" ] [ text "Info Type" ]
                , input
                    [ class "input"
                    , type_ "text"
                    , onInput (\o -> UpdateEventForm { form | info_type = Just o })
                    , value (Maybe.withDefault "" form.info_type)
                    ]
                    []
                ]

        -- }}}
        --{{{is_confirmed
        is_confirmed =
            case screen of
                EditEvent _ _ UnconfirmedDelete _ ->
                    False

                EditEvent _ _ ConfirmedDelete _ ->
                    True

                _ ->
                    False

        --}}}
        --{{{toggle_confirm
        toggle_confirm =
            case screen of
                EditEvent uuid form_ UnconfirmedDelete tab ->
                    EditEvent uuid form_ ConfirmedDelete tab

                EditEvent uuid form_ ConfirmedDelete tab ->
                    EditEvent uuid form_ UnconfirmedDelete tab

                same ->
                    same

        --}}}
        --{{{renderDetailsForm
        renderDetailsForm =
            case form.event_type of
                "Event::Flight" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , airlineField
                        , flight_numberField
                        , terminalField
                        , gateField
                        ]

                "Event::Lodging" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        ]

                "Event::Activity" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        ]

                "Event::Transportation" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        , phone_numberField
                        ]

                "Event::Cruise" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        , cabin_typeField
                        , cabin_numberField
                        ]

                "Event::Dining" ->
                    div []
                        [ starts_atField
                        , ends_atField
                        , durationField
                        , priceField
                        , currencyField
                        , notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        ]

                "Event::Information" ->
                    div []
                        [ notesField
                        , booked_throughField
                        , confirmationField
                        , providerField
                        ]

                _ ->
                    text ""

        --}}}
        --{{{render_forms
        renderPhotosForm =
            div [] [ text "Photos form" ]

        renderFilesForm =
            div [] [ text "Files form" ]

        renderPlacesForm =
            div [] [ text "Places form" ]

        renderInfoForm =
            div [] [ text "Info form" ]

        render_forms =
            case screen of
                EditEvent _ _ _ tab ->
                    case tab of
                        DetailsTab ->
                            renderDetailsForm

                        PhotosTab ->
                            renderPhotosForm

                        FilesTab ->
                            renderFilesForm

                        PlacesTab ->
                            renderPlacesForm

                        InfoTab ->
                            renderInfoForm

                _ ->
                    text ""

        --}}}
    in
    div [ class "edit-event" ]
        [ Html.form []
            [ div [ class "field" ]
                [ breadcrumb
                    [ ( "Home", Href (Route.href Route.Home) )
                    , ( trip.name, Href (Route.href <| Route.Planner <| Uuid trip.uuid) )
                    , ( section_title, OnClick (onClick <| Goto <| ListSection (Uuid form.section_id) ListEvents) )
                    , ( Maybe.withDefault "..." form.title, Href (Route.href <| Route.Planner <| Uuid trip.uuid) )
                    ]
                , label [ class "label" ] [ text "Title" ]
                , div [ class "control has-icons-left has-icons-right" ]
                    [ input
                        [ class "input"
                        , placeholder "Title"
                        , onInput (\o -> UpdateEventForm { form | title = Just o })
                        , value <| Maybe.withDefault "" form.title
                        ]
                        []
                    ]
                ]
            , form_tabs screen
            , render_forms
            ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button [ class "button is-link", href "", onClick (SubmittedEventForm (Uuid form.uuid) form) ] [ text "Done" ]
                ]
            , div []
                [ button
                    [ class <|
                        if is_confirmed then
                            "button is-danger flip-horizontal-top"

                        else
                            "button has-text-danger is-text flip-horizontal-bottom"
                    , href ""
                    , if is_confirmed then
                        onClick <| ConfirmedDeleteEvent <| Uuid form.uuid

                      else
                        onClick <| DeleteEvent
                    , onBlur <| Goto <| toggle_confirm
                    ]
                    [ text <|
                        if is_confirmed then
                            "Confirm Delete!"

                        else
                            "Delete Event"
                    ]
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
            [ ( "Home", Href (Route.href Route.Home) )
            , ( trip.name, Href (Route.href <| Route.Planner (Uuid trip.uuid)) )
            ]
        , div []
            (case screen of
                ListSections ->
                    viewSections trip.sections events

                ListAll ->
                    List.map viewEventItem events

                ListSection section_id _ ->
                    List.map viewEventItem <|
                        List.filter
                            (\event -> event.section_id == Uuid.toString section_id)
                            (eventsToRecords trip.events)

                _ ->
                    [ text "Other Tabs" ]
            )
        , div [ class "panel-block" ]
            [ button [ class "button is-link is-outlined is-fullwidth", onClick (Goto (NewSection "")) ]
                [ text "Add New Section" ]
            ]
        ]



--}}}
-- {{{ ListSection


viewEventFull event =
    let
        event_name_ =
            String.toLower <| event_name event.event_type
    in
    div [ class "event-full" ]
        [ div [ class "side" ]
            [ span [ class <| "sign " ++ event_name_ ] [ span [ class <| "icon " ++ " fa-" ++ event_name_ ] [] ]
            ]
        , div [ class "body" ]
            [ div [ class "event-title" ]
                [ a
                    [ href ""
                    , onClick <| ShowEvent (Uuid event.uuid)
                    ]
                    [ text <| Maybe.withDefault "..." event.title ]
                ]
            , p [ class "event-desc" ]
                [ text <| Maybe.withDefault "" event.notes ]
            ]
        ]


viewListSection screen section events trip =
    let
        sectionHeader =
            div [ class "section-hdr" ]
                [ div [ class "level" ]
                    (case screen of
                        ListSection uuid ListEvents ->
                            [ div [ class "level-right" ]
                                [ a
                                    [ href ""
                                    , onClick <| Goto <| ListSection (Uuid section.uuid) (EditSection section.title)
                                    , class "level-item title"
                                    ]
                                    [ text section.title ]
                                ]
                            ]

                        ListSection uuid (EditSection title) ->
                            [ div [ class "field  level-item has-addons" ]
                                [ div [ class "control is-expanded" ]
                                    [ input
                                        [ class "input"
                                        , type_ "text"
                                        , placeholder "Section / Day name"
                                        , onInput (\o -> Goto <| ListSection (Uuid section.uuid) (EditSection o))
                                        , value title
                                        ]
                                        []
                                    ]
                                , div [ class "control" ]
                                    [ button
                                        [ class "button is-link is-info"
                                        , href ""
                                        , onClick (SubmitSection (Just (Uuid section.uuid)) title)
                                        ]
                                        [ text "Done" ]
                                    ]
                                ]
                            ]

                        _ ->
                            [ text "" ]
                    )
                ]
    in
    div []
        [ breadcrumb
            [ ( "Home", Href (Route.href Route.Home) )
            , ( trip.name, Href (Route.href <| Route.Planner (Uuid trip.uuid)) )
            , ( section.title, OnClick (onClick <| Goto <| ListSection (Uuid section.uuid) ListEvents) )
            ]
        , sectionHeader
        , div [ class "level" ]
            [ p [ class "level-item" ]
                [ createEventButton section.uuid ]
            ]
        , div []
            (List.map viewEventFull <| List.filter (\o -> o.section_id == section.uuid) events)
        ]



-- }}}
-- {{{ viewSections


viewSections sections events =
    let
        lower_event_name o =
            String.toLower <| event_name o.event_type

        viewSection { uuid, title } =
            div
                [ class "section-summary"
                ]
                [ div [ class "section-body", onClick <| Goto <| ListSection (Uuid uuid) ListEvents ]
                    [ div [ class "title" ]
                        [ a
                            [ href ""
                            , onClick <| Goto <| ListSection (Uuid uuid) ListEvents
                            ]
                            [ text title ]
                        ]
                    , div [ class "events-icons" ]
                        (List.map
                            (\o ->
                                if o.section_id == uuid then
                                    a
                                        [ href ""
                                        , class <| "event " ++ lower_event_name o
                                        , onClick <| Goto <| ListSection (Uuid uuid) ListEvents
                                        ]
                                        [ span [ class <| "fas fa-" ++ lower_event_name o ] []
                                        ]

                                else
                                    text ""
                            )
                            events
                        )
                    ]
                , div [ class "more" ]
                    [ a
                        [ href "", onClick <| Goto <| ListSection (Uuid uuid) ListEvents ]
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


event_to_record event =
    case event of
        Dining uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation provider ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , provider = provider
            , airline = Nothing
            , terminal = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , info_type = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , carrier = Nothing
            , phone_number = Nothing
            }

        Information uuid section_id event_type title notes info_type ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , notes = notes
            , info_type = info_type
            , price = Nothing
            , currency = Nothing
            , starts_at = Nothing
            , ends_at = Nothing
            , duration = Nothing
            , booked_through = Nothing
            , confirmation = Nothing
            , provider = Nothing
            , airline = Nothing
            , terminal = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , carrier = Nothing
            , phone_number = Nothing
            }

        Activity uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation provider ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , provider = provider
            , airline = Nothing
            , terminal = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , carrier = Nothing
            , phone_number = Nothing
            , info_type = Nothing
            }

        Lodging uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation provider ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , provider = provider
            , airline = Nothing
            , terminal = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , carrier = Nothing
            , phone_number = Nothing
            , info_type = Nothing
            }

        Flight uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation airline flight_number terminal gate ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , airline = airline
            , flight_number = flight_number
            , terminal = terminal
            , gate = gate
            , carrier = Nothing
            , provider = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , phone_number = Nothing
            , info_type = Nothing
            }

        Transportation uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation carrier phone_number ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , carrier = carrier
            , phone_number = phone_number
            , provider = Nothing
            , airline = Nothing
            , terminal = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , cabin_type = Nothing
            , cabin_number = Nothing
            , info_type = Nothing
            }

        Cruise uuid section_id event_type title price currency notes starts_at ends_at duration booked_through confirmation carrier cabin_type cabin_number ->
            { uuid = uuid
            , section_id = section_id
            , event_type = event_type
            , title = title
            , price = price
            , currency = currency
            , notes = notes
            , starts_at = starts_at
            , ends_at = ends_at
            , duration = duration
            , booked_through = booked_through
            , confirmation = confirmation
            , carrier = carrier
            , cabin_type = cabin_type
            , cabin_number = cabin_number
            , airline = Nothing
            , terminal = Nothing
            , provider = Nothing
            , flight_number = Nothing
            , gate = Nothing
            , info_type = Nothing
            , phone_number = Nothing
            }


eventsToRecords events =
    List.map event_to_record events



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
    Html.form [ onSubmit (SubmitSection Nothing title) ]
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
-- {{{ form_tabs


form_tabs : Screen -> Html Msg
form_tabs screen =
    let
        goto_tab tab =
            case screen of
                EditEvent uuid form deletion _ ->
                    case tab of
                        "details" ->
                            EditEvent uuid form deletion DetailsTab

                        "photos" ->
                            EditEvent uuid form deletion PhotosTab

                        "files" ->
                            EditEvent uuid form deletion FilesTab

                        "places" ->
                            EditEvent uuid form deletion PlacesTab

                        "info" ->
                            EditEvent uuid form deletion InfoTab

                        _ ->
                            screen

                same ->
                    same

        is_active tab =
            case screen of
                EditEvent _ _ _ DetailsTab ->
                    if tab == "details" then
                        "is-active"

                    else
                        ""

                EditEvent _ _ _ PhotosTab ->
                    if tab == "photos" then
                        "is-active"

                    else
                        ""

                EditEvent _ _ _ FilesTab ->
                    if tab == "files" then
                        "is-active"

                    else
                        ""

                EditEvent _ _ _ PlacesTab ->
                    if tab == "places" then
                        "is-active"

                    else
                        ""

                EditEvent _ _ _ InfoTab ->
                    if tab == "info" then
                        "is-active"

                    else
                        ""

                _ ->
                    ""

        render_tab tab_name =
            li [ class (is_active tab_name) ]
                [ a
                    [ href ""
                    , onClick <| Goto <| goto_tab tab_name
                    ]
                    [ text tab_name ]
                ]
    in
    div [ class "tabs" ]
        [ ul []
            (List.map
                render_tab
                [ "details", "photos", "files", "places", "info" ]
            )
        ]



-- }}}
-- {{{ createEventButton


event_name o =
    String.slice 7 -1 (o ++ " ")


createEventButton section_id =
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


getSection : Uuid -> List Section -> Maybe Section
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


type LinkAttr msg
    = Href (Attribute msg)
    | OnClick (Attribute msg)


breadcrumb : List ( String, LinkAttr msg ) -> Html msg
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
                [ (case attr_msg of
                    Href attr_ ->
                        a [ is_current idx, attr_ ]

                    OnClick attr_ ->
                        a [ is_current idx, href "", attr_ ]
                  )
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
            Mutations.sectionRequest section trip_id
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotSaveSectionResponse
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


deleteEvent : Session -> TripId -> EventId -> Cmd Msg
deleteEvent session trip_id event_id =
    case session of
        LoggedIn _ viewer ->
            Mutations.deleteEventRequest trip_id event_id
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send
                    (RemoteData.fromResult
                        >> GotDeleteEventResponse
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
