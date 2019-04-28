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
import Mutations
import Page.Planner as Planner exposing (Model, Msg(..), init)
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Authentication(..), Event(..), Response, Trip, User)



-- Model {{{


type Model
    = Planner Planner.Model



-- Model }}}
-- Msg {{{


type Msg
    = GotPlannerMsg Planner.Msg



-- Msg }}}


type alias Flags =
    ()


init flags =
    let
        model =
            Planner.init

        init_planner =
            ( model, Planner.getUserTrips model.endpoint )
                |> updateWith Planner GotPlannerMsg (Planner model)
    in
    init_planner


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( GotPlannerMsg subMsg, Planner subModel ) ->
            Planner.update subMsg subModel
                |> updateWith Planner GotPlannerMsg model


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )


view : Model -> Browser.Document Msg
view model =
    let
        viewPage toMsg { title, content } =
            { title = title
            , body = List.map (Html.map toMsg) content
            }
    in
    case model of
        Planner planner ->
            viewPage GotPlannerMsg (Planner.view planner)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Planner planner ->
            Sub.map GotPlannerMsg (Planner.subscriptions planner)


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
