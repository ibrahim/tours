module Main exposing (main)

import Api
import Browser
import Graphql.Document as Document
import Graphql.Http
import Graphql.Http.GraphqlError
import Helpers.Main
import Html exposing (Html, div, h1, li, p, pre, text, ul)
import Model exposing (Model)
import Msg exposing (Msg(..))
import PrintAny
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Event(..), Response, Trip, User)



-- Elm Architecture Setup


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( RemoteData.Loading, Api.getUserTrips )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotResponse response ->
            ( response, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "TourFax"
    , body =
        [ div []
            [ h1 [] [ text "Generated Query" ]
            , pre [] [ text (Document.serializeQuery Queries.userTripsQuery) ]
            ]
        , case model of
            RemoteData.Success user ->
                case user of
                    { email, trips } ->
                        div []
                            [ text email
                            , ul [] (List.map viewTrip trips)
                            ]

            RemoteData.Failure errorData ->
                errorData
                    |> errorToString
                    |> text

            _ ->
                text "Loading..."
        ]
    }


viewTrip : Trip -> Html msg
viewTrip { name, events } =
    li []
        [ text name
        , ul [] (List.map viewEvent events)
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
