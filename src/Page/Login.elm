module Page.Login exposing (Model, Msg, init, subscriptions, toSession, update, view)

{-| The login page.
-}

import Api exposing (ApiError(..), ApiHeaders, ApiResponse(..), Cred, expectJson)
import Browser.Navigation as Nav
import Dict exposing (Dict(..), get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, string)
import Json.Decode.Pipeline exposing (optional)
import Json.Encode as Encode
import Page exposing (header)
import Route exposing (Route)
import Session exposing (Session)
import Viewer exposing (Viewer)



-- Model {{{


type alias Model =
    { session : Session
    , problems : List Problem
    , viewer : Maybe Viewer
    , form : Form
    }


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { email : String
    , password : String
    }



-- Model }}}
-- init {{{


init : Session -> ( Model, Cmd msg )
init session =
    ( { session = session
      , problems = []
      , viewer = Nothing
      , form =
            { email = ""
            , password = ""
            }
      }
    , Cmd.none
    )



-- init }}}
-- view {{{


view : Model -> { title : String, content : List (Html Msg) }
view model =
    { title = "Login"
    , content =
        [ Page.header "Login"
        , section [ class "cred-page section" ]
            [ div [ class "columns is-mobile is-centered" ]
                [ div [ class "column is-half-desktop is-three-quarters-mobile" ]
                    [ div [ class "" ]
                        [ h1 [ class "title is-3" ] [ text "Sign in" ]
                        , ul [ class "error-messages" ]
                            (List.map viewProblem model.problems)
                        , viewForm model.form
                        ]
                    ]
                ]
            ]
        ]
    }


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ str ->
                    str

                ServerError str ->
                    str
    in
    li [] [ text errorMessage ]


viewForm : Form -> Html Msg
viewForm form =
    Html.form [ onSubmit SubmittedForm ]
        [ div [ class "field" ]
            [ label [ class "label" ] [ text "Email" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ input
                    [ class "input"
                    , placeholder "Email"
                    , onInput EnteredEmail
                    , value form.email
                    ]
                    []
                ]
            ]
        , div [ class "field" ]
            [ label [ class "label" ] [ text "Password" ]
            , div [ class "control has-icons-left has-icons-right" ]
                [ input
                    [ class "input"
                    , type_ "password"
                    , placeholder "Password"
                    , onInput EnteredPassword
                    , value form.password
                    ]
                    []
                , span [ class "icon is-small is-left" ]
                    [ i [ class "fas fa-user" ] [] ]
                , span [ class "icon is-small is-right" ]
                    [ i [ class "fas fa-check" ] [] ]
                ]
            , p [ class "help is-success" ] [ text "This username is available" ]
            ]
        , div [ class "field is-grouped" ]
            [ div [ class "control" ]
                [ button [ class "button is-link" ] [ text "Submit" ]
                ]
            , div [ class "control" ]
                [ button [ class "button is-text" ] [ text "Cancel" ]
                ]
            ]
        ]



-- view }}}
-- Msg {{{


type Msg
    = SubmittedForm
    | EnteredEmail String
    | EnteredPassword String
    | CompletedLogin (Result ApiError (ApiResponse Viewer))
    | GotSession Session



-- Msg }}}
-- udate {{{


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SubmittedForm ->
            case validate model.form of
                Ok validForm ->
                    ( { model | problems = [] }
                    , login validForm
                    )

                Err problems ->
                    ( { model | problems = problems }
                    , Cmd.none
                    )

        EnteredEmail email ->
            updateForm (\form -> { form | email = email }) model

        EnteredPassword password ->
            updateForm (\form -> { form | password = password }) model

        CompletedLogin (Err error) ->
            let
                serverErrors =
                    case error of
                        ErrorMessage metadata body ->
                            [ ServerError body ]

                        Timeout ->
                            [ ServerError "Login Request Timeout" ]

                        NetworkError ->
                            [ ServerError "Network Error while Login" ]

                        BadUrl _ ->
                            [ ServerError "Login Request: bad url" ]

                        BadBody err ->
                            [ ServerError ("Login Request:  unable to decode login response" ++ err) ]

                -- Api.decodeErrors error
                --     |> List.map ServerError
            in
            ( { model | problems = List.append model.problems serverErrors }
            , Cmd.none
            )

        CompletedLogin (Ok (ApiResponse viewer headers)) ->
            ( { model | viewer = Just viewer }
            , Viewer.store viewer
            )

        GotSession session ->
            ( { model | session = session }
            , Route.replaceUrl (Session.navKey session) Route.Home
            )



--}}}


{-| Helper function for `update`. Updates the form and returns Cmd.none.
Useful for recording form fields!
-}
updateForm : (Form -> Form) -> Model -> ( Model, Cmd Msg )
updateForm transform model =
    ( { model | form = transform model.form }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Session.changes GotSession (Session.navKey model.session)



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Email
    | Password


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Email
    , Password
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Email ->
                if String.isEmpty form.email then
                    [ "email can't be blank." ]

                else
                    []

            Password ->
                if String.isEmpty form.password then
                    [ "password can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { email = String.trim form.email
        , password = String.trim form.password
        }



-- HTTP


login : TrimmedForm -> Cmd Msg
login (Trimmed form) =
    let
        user =
            Encode.object
                [ ( "email", Encode.string form.email )
                , ( "password", Encode.string form.password )
                ]

        loginRequest requestBody =
            Http.post
                { url = Api.loginEndpoint
                , body = requestBody
                , expect = Api.expectJson CompletedLogin loginResponseDecoder
                }

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    loginRequest body


loginResponseDecoder =
    Decode.field "user" (Api.decoderFromCred Viewer.decoder)



-- EXPORT


toSession : Model -> Session
toSession model =
    model.session
