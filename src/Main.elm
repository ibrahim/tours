module Main exposing (main)

import Api
import Browser
import Browser.Navigation as Nav
import Graphql.Document as Document
import Graphql.Http
import Graphql.Http.GraphqlError
import Helpers.Main
import Html exposing (Html, button, div, h1, input, li, p, pre, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Mutations
import Page.Home as Home exposing (Model, Msg(..), init, initial_state)
import Page.Planner as Planner exposing (Model, Msg(..), init)
import Queries
import RemoteData exposing (RemoteData)
import Route exposing (Route(..))
import Session exposing (Session(..), navKey)
import Types exposing (Authentication(..), Event(..), Response, Trip, User)
import Url exposing (Url)



-- Model {{{


type Model
    = Planner Planner.Model
    | Home Home.Model
    | Redirect Session



-- Model }}}
-- Msg {{{


type Msg
    = ChangedRoute (Maybe Route)
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | GotPlannerMsg Planner.Msg
    | GotHomeMsg Home.Msg



-- Msg }}}


type alias Flags =
    ()


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    changeRouteTo (Route.fromUrl url)
        (Redirect (Guest url key))



-- changeRouteTo (Route.fromUrl url) (Home Home.initial_state)


toSession : Model -> Session
toSession model =
    case model of
        Redirect session ->
            session

        Home subModel ->
            Home.toSession subModel

        Planner subModel ->
            Planner.toSession subModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ClickedLink urlRequest, _ ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl (Session.navKey (toSession model)) (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( ChangedUrl url, _ ) ->
            changeRouteTo (Route.fromUrl url) model

        ( ChangedRoute route, _ ) ->
            changeRouteTo route model

        ( GotPlannerMsg subMsg, Planner subModel ) ->
            Planner.update subMsg subModel
                |> updateWith Planner GotPlannerMsg model

        ( GotHomeMsg subMsg, Home subModel ) ->
            Home.update subMsg subModel
                |> updateWith Home GotHomeMsg model

        ( _, _ ) ->
            -- Disregard messages that arrived for the wrong page.
            ( model, Cmd.none )


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

        Home home ->
            viewPage GotHomeMsg (Home.view home)

        _ ->
            { title = "Page Not Found"
            , body =
                [ div []
                    [ text "Page Not Found"
                    ]
                ]
            }


changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    case maybeRoute of
        Nothing ->
            Home.init (toSession model)
                |> updateWith Home GotHomeMsg model

        Just Route.Root ->
            let
                key =
                    toSession model |> Session.navKey
            in
            ( model, Route.replaceUrl key Route.Home )

        Just Route.Planner ->
            Planner.init (toSession model)
                |> updateWith Planner GotPlannerMsg model

        Just Route.Home ->
            Home.init (toSession model)
                |> updateWith Home GotHomeMsg model



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Planner planner ->
            Sub.map GotPlannerMsg (Planner.subscriptions planner)

        Home home ->
            Sub.map GotHomeMsg (Home.subscriptions home)

        _ ->
            Sub.none


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }
