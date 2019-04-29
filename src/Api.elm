port module Api exposing (ApiError(..), ApiHeaders, ApiResponse(..), Cred(..), application, decoderFromCred, endpoint, expectJson, getUserTrips, loginEndpoint, storeCredWith, username, viewerChanges)

import Avatar exposing (Avatar(..))
import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Graphql.Http
import Graphql.Http.GraphqlError
import Http exposing (Error(..), Expect, Response(..))
import Json.Decode as Decode exposing (Decoder, Value, decodeString, errorToString, field, string)
import Json.Decode.Pipeline as Pipeline exposing (optional, required)
import Json.Encode as Encode
import Queries
import RemoteData exposing (RemoteData)
import Types exposing (Endpoint, Response, Trip)
import Url exposing (Url)
import Username exposing (Username)


endpoint : String
endpoint =
    "http://localhost:5555/graphql"


loginEndpoint : String
loginEndpoint =
    "http://localhost:5555/login"


type Cred
    = Cred Username String


username : Cred -> Username
username (Cred val _) =
    val


credHeader : Cred -> Http.Header
credHeader (Cred _ str) =
    Http.header "authorization" ("Token " ++ str)


getUserTrips : Graphql.Http.Request Response
getUserTrips =
    Queries.userTripsQuery
        |> Graphql.Http.queryRequest endpoint


decoderFromCred : Decoder (Cred -> a) -> Decoder a
decoderFromCred decoder =
    Decode.map2 (\fromCred cred -> fromCred cred)
        decoder
        credDecoder


{-| It's important that this is never exposed!

We epxose `login` and `application` instead, so we can be certain that if anyone
ever has access to a `Cred` value, it came from either the login API endpoint
or was passed in via flags.

-}
credDecoder : Decoder Cred
credDecoder =
    Decode.succeed Cred
        |> required "username" Username.decoder
        |> required "token" Decode.string



-- ERRORS


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.keyValuePairs (Decode.list Decode.string)
        |> Decode.map (List.concatMap fromPair)


fromPair : ( String, List String ) -> List String
fromPair ( field, errors ) =
    List.map (\error -> field ++ " " ++ error) errors



-- LOCALSTORAGE KEYS


storeCredWith : Cred -> Avatar -> Cmd msg
storeCredWith (Cred uname token) avatar =
    let
        json =
            Encode.object
                [ ( "user"
                  , Encode.object
                        [ ( "username", Username.encode uname )
                        , ( "token", Encode.string token )
                        , ( "image", Avatar.encode avatar )
                        ]
                  )
                ]
    in
    storeCache (Just json)


port storeCache : Maybe Value -> Cmd msg


port onStoreChange : (Value -> msg) -> Sub msg


viewerChanges : (Maybe viewer -> msg) -> Decoder (Cred -> viewer) -> Sub msg
viewerChanges toMsg decoder =
    onStoreChange (\value -> toMsg (decodeFromChange decoder value))


decodeFromChange : Decoder (Cred -> viewer) -> Value -> Maybe viewer
decodeFromChange viewerDecoder val =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    Decode.decodeValue (storageDecoder viewerDecoder) val
        |> Result.toMaybe


storageDecoder : Decoder (Cred -> viewer) -> Decoder viewer
storageDecoder viewerDecoder =
    Decode.field "user" (decoderFromCred viewerDecoder)


cacheStorageKey : String
cacheStorageKey =
    "cache"


credStorageKey : String
credStorageKey =
    "cred"


application :
    Decoder (Cred -> viewer)
    ->
        { init : Maybe viewer -> Url -> Nav.Key -> ( model, Cmd msg )
        , onUrlChange : Url -> msg
        , onUrlRequest : Browser.UrlRequest -> msg
        , subscriptions : model -> Sub msg
        , update : msg -> model -> ( model, Cmd msg )
        , view : model -> Browser.Document msg
        }
    -> Program Value model msg
application viewerDecoder config =
    let
        init flags url navKey =
            let
                maybeViewer =
                    Decode.decodeValue Decode.string flags
                        |> Result.andThen (Decode.decodeString (storageDecoder viewerDecoder))
                        |> Result.toMaybe
            in
            config.init maybeViewer url navKey
    in
    Browser.application
        { init = init
        , onUrlChange = config.onUrlChange
        , onUrlRequest = config.onUrlRequest
        , subscriptions = config.subscriptions
        , update = config.update
        , view = config.view
        }


type ApiError
    = BadUrl String
    | Timeout
    | NetworkError
    | ErrorMessage Http.Metadata String
    | BadBody String


type ApiResponse a
    = ApiResponse a ApiHeaders


type alias ApiHeaders =
    Dict.Dict String String


expectJson : (Result ApiError (ApiResponse a) -> msg) -> Decode.Decoder a -> Expect msg
expectJson toMsg decoder =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (BadUrl url)

                Http.Timeout_ ->
                    Err Timeout

                Http.NetworkError_ ->
                    Err NetworkError

                Http.BadStatus_ metadata body ->
                    Err (ErrorMessage metadata body)

                Http.GoodStatus_ metadata body ->
                    case Decode.decodeString decoder body of
                        Ok value ->
                            Ok (ApiResponse value metadata.headers)

                        Err err ->
                            Err (BadBody (Decode.errorToString err))
