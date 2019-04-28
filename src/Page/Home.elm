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
import PrintAny
import RemoteData exposing (RemoteData)
import Session exposing (Session(..))
import Types exposing (..)



-- Msg {{{


type Msg
    = HomeMsg



-- Msg }}}
-- Model {{{


type alias Model =
    { session : Session
    }



-- Model }}}
-- init {{{


initial_state : Session -> Model
initial_state session =
    { session = session
    }


init : Session -> ( Model, Cmd Msg )
init session =
    ( initial_state session, Cmd.none )



-- init }}}
-- update {{{


update msg model =
    ( model, Cmd.none )



-- Update }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "TourFax"
    , content = [ Page.header "Home" ]
    }



-- View }}}
-- Cmd Msg {{{
-- CMD }}}
-- Sub Msg {{{


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Sub Msg }}}


toSession : Model -> Session
toSession model =
    model.session
