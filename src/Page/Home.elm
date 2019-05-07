module Page.Home exposing (Model, Msg(..), init, initial_state, subscriptions, toSession, update, view)

import Api
import Browser
import Graphql.Http
import Graphql.Http.GraphqlError
import Html exposing (Html, a, button, div, h1, input, li, p, pre, text, ul)
import Html.Attributes exposing (href, value)
import Html.Events exposing (onClick, onInput)
import Mutations
import Page exposing (header)
import Problem exposing (Problem(..), showProblems)
import Queries
import RemoteData exposing (RemoteData)
import Session exposing (Session(..))
import Types exposing (..)
import Username exposing (toString)
import Uuid exposing (Uuid, toString)
import Viewer exposing (cred, tokenStr, username)



-- Msg {{{


type Msg
    = GotUserTripsResponse (RemoteData (Graphql.Http.Error User) User)
    | ClearProblems



-- Msg }}}
-- Model {{{


type alias Model =
    { session : Session
    , trips : RemoteData (Graphql.Http.Error User) User
    , problems : List Problem
    }



-- Model }}}
-- init {{{


initial_state : Session -> Model
initial_state session =
    { session = session
    , trips = RemoteData.Loading
    , problems = []
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( initial_state session, getUserTrips session )



-- init }}}
-- update {{{


update msg model =
    case msg of
        GotUserTripsResponse response ->
            let
                resolve =
                    \data -> { model | trips = response }

                reject =
                    \problems ->
                        { model
                            | problems = problems
                            , trips = RemoteData.NotAsked
                        }
            in
            Api.processQueryResponse response model resolve reject

        ClearProblems ->
            ( { model | problems = [] }, Cmd.none )



-- Update }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view { trips, session, problems } =
    { title = "TourFax"
    , content =
        [ div [] [ Page.header "Home" ]
        , case session of
            LoggedIn _ viewer ->
                div []
                    [ text (Username.toString (Viewer.username viewer))
                    , showProblems ClearProblems problems
                    , ul [] (viewTrips trips)
                    ]

            Guest _ ->
                text "Unauthenticated"
        ]
    }


viewTrips trips =
    case trips of
        RemoteData.Success user ->
            user.trips
                |> List.map
                    (\item ->
                        li
                            []
                            [ text item.name
                            , viewTrip item
                            ]
                    )

        RemoteData.Failure error ->
            [ text "Http error " ]

        RemoteData.NotAsked ->
            [ text "" ]

        RemoteData.Loading ->
            [ text "Loading.." ]


viewTrip : Trip -> Html msg
viewTrip { uuid, name } =
    li []
        [ a [ href ("#/planner/" ++ uuid) ] [ text name ]
        ]



-- View }}}
-- Cmd Msg {{{


getUserTrips : Session -> Cmd Msg
getUserTrips session =
    case session of
        LoggedIn _ viewer ->
            Queries.userTripsQuery
                |> Graphql.Http.queryRequest Api.endpoint
                |> Graphql.Http.withHeader "authorization" ("Bearer " ++ Viewer.tokenStr viewer)
                |> Graphql.Http.send (RemoteData.fromResult >> GotUserTripsResponse)

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
