-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.InputObject exposing (DeleteEventInput, DeleteEventInputOptionalFields, DeleteEventInputRequiredFields, SaveEventInput, SaveEventInputOptionalFields, SaveEventInputRequiredFields, SaveSectionInput, SaveSectionInputOptionalFields, SaveSectionInputRequiredFields, SaveTripInput, SaveTripInputOptionalFields, buildDeleteEventInput, buildSaveEventInput, buildSaveSectionInput, buildSaveTripInput, encodeDeleteEventInput, encodeSaveEventInput, encodeSaveSectionInput, encodeSaveTripInput)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Tour.Interface
import Tour.Object
import Tour.Scalar
import Tour.ScalarCodecs
import Tour.Union


buildDeleteEventInput : DeleteEventInputRequiredFields -> (DeleteEventInputOptionalFields -> DeleteEventInputOptionalFields) -> DeleteEventInput
buildDeleteEventInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { clientMutationId = Absent }
    in
    { clientMutationId = optionals.clientMutationId, trip_id = required.trip_id, uuid = required.uuid }


type alias DeleteEventInputRequiredFields =
    { trip_id : String
    , uuid : String
    }


type alias DeleteEventInputOptionalFields =
    { clientMutationId : OptionalArgument String }


{-| Type for the DeleteEventInput input object.
-}
type alias DeleteEventInput =
    { clientMutationId : OptionalArgument String
    , trip_id : String
    , uuid : String
    }


{-| Encode a DeleteEventInput into a value that can be used as an argument.
-}
encodeDeleteEventInput : DeleteEventInput -> Value
encodeDeleteEventInput input =
    Encode.maybeObject
        [ ( "clientMutationId", Encode.string |> Encode.optional input.clientMutationId ), ( "trip_id", Encode.string input.trip_id |> Just ), ( "uuid", Encode.string input.uuid |> Just ) ]


buildSaveEventInput : SaveEventInputRequiredFields -> (SaveEventInputOptionalFields -> SaveEventInputOptionalFields) -> SaveEventInput
buildSaveEventInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { clientMutationId = Absent, uuid = Absent, title = Absent, notes = Absent, price = Absent, currency = Absent, starts_at = Absent, ends_at = Absent, duration = Absent, day = Absent, booked_through = Absent, confirmation = Absent, provider = Absent, airline = Absent, flight_number = Absent, terminal = Absent, gate = Absent, cabin_type = Absent, cabin_number = Absent, phone_number = Absent, carrier = Absent, info_type = Absent }
    in
    { clientMutationId = optionals.clientMutationId, trip_id = required.trip_id, type_ = required.type_, section_id = required.section_id, uuid = optionals.uuid, title = optionals.title, notes = optionals.notes, price = optionals.price, currency = optionals.currency, starts_at = optionals.starts_at, ends_at = optionals.ends_at, duration = optionals.duration, day = optionals.day, booked_through = optionals.booked_through, confirmation = optionals.confirmation, provider = optionals.provider, airline = optionals.airline, flight_number = optionals.flight_number, terminal = optionals.terminal, gate = optionals.gate, cabin_type = optionals.cabin_type, cabin_number = optionals.cabin_number, phone_number = optionals.phone_number, carrier = optionals.carrier, info_type = optionals.info_type }


type alias SaveEventInputRequiredFields =
    { trip_id : String
    , type_ : String
    , section_id : String
    }


type alias SaveEventInputOptionalFields =
    { clientMutationId : OptionalArgument String
    , uuid : OptionalArgument String
    , title : OptionalArgument String
    , notes : OptionalArgument String
    , price : OptionalArgument Int
    , currency : OptionalArgument String
    , starts_at : OptionalArgument Float
    , ends_at : OptionalArgument Float
    , duration : OptionalArgument Int
    , day : OptionalArgument Int
    , booked_through : OptionalArgument String
    , confirmation : OptionalArgument String
    , provider : OptionalArgument String
    , airline : OptionalArgument String
    , flight_number : OptionalArgument String
    , terminal : OptionalArgument String
    , gate : OptionalArgument String
    , cabin_type : OptionalArgument String
    , cabin_number : OptionalArgument String
    , phone_number : OptionalArgument String
    , carrier : OptionalArgument String
    , info_type : OptionalArgument String
    }


{-| Type for the SaveEventInput input object.
-}
type alias SaveEventInput =
    { clientMutationId : OptionalArgument String
    , trip_id : String
    , type_ : String
    , section_id : String
    , uuid : OptionalArgument String
    , title : OptionalArgument String
    , notes : OptionalArgument String
    , price : OptionalArgument Int
    , currency : OptionalArgument String
    , starts_at : OptionalArgument Float
    , ends_at : OptionalArgument Float
    , duration : OptionalArgument Int
    , day : OptionalArgument Int
    , booked_through : OptionalArgument String
    , confirmation : OptionalArgument String
    , provider : OptionalArgument String
    , airline : OptionalArgument String
    , flight_number : OptionalArgument String
    , terminal : OptionalArgument String
    , gate : OptionalArgument String
    , cabin_type : OptionalArgument String
    , cabin_number : OptionalArgument String
    , phone_number : OptionalArgument String
    , carrier : OptionalArgument String
    , info_type : OptionalArgument String
    }


{-| Encode a SaveEventInput into a value that can be used as an argument.
-}
encodeSaveEventInput : SaveEventInput -> Value
encodeSaveEventInput input =
    Encode.maybeObject
        [ ( "clientMutationId", Encode.string |> Encode.optional input.clientMutationId ), ( "trip_id", Encode.string input.trip_id |> Just ), ( "_type", Encode.string input.type_ |> Just ), ( "section_id", Encode.string input.section_id |> Just ), ( "uuid", Encode.string |> Encode.optional input.uuid ), ( "title", Encode.string |> Encode.optional input.title ), ( "notes", Encode.string |> Encode.optional input.notes ), ( "price", Encode.int |> Encode.optional input.price ), ( "currency", Encode.string |> Encode.optional input.currency ), ( "starts_at", Encode.float |> Encode.optional input.starts_at ), ( "ends_at", Encode.float |> Encode.optional input.ends_at ), ( "duration", Encode.int |> Encode.optional input.duration ), ( "day", Encode.int |> Encode.optional input.day ), ( "booked_through", Encode.string |> Encode.optional input.booked_through ), ( "confirmation", Encode.string |> Encode.optional input.confirmation ), ( "provider", Encode.string |> Encode.optional input.provider ), ( "airline", Encode.string |> Encode.optional input.airline ), ( "flight_number", Encode.string |> Encode.optional input.flight_number ), ( "terminal", Encode.string |> Encode.optional input.terminal ), ( "gate", Encode.string |> Encode.optional input.gate ), ( "cabin_type", Encode.string |> Encode.optional input.cabin_type ), ( "cabin_number", Encode.string |> Encode.optional input.cabin_number ), ( "phone_number", Encode.string |> Encode.optional input.phone_number ), ( "carrier", Encode.string |> Encode.optional input.carrier ), ( "info_type", Encode.string |> Encode.optional input.info_type ) ]


buildSaveSectionInput : SaveSectionInputRequiredFields -> (SaveSectionInputOptionalFields -> SaveSectionInputOptionalFields) -> SaveSectionInput
buildSaveSectionInput required fillOptionals =
    let
        optionals =
            fillOptionals
                { clientMutationId = Absent, uuid = Absent, is_day = Absent, day_date = Absent, day_order = Absent }
    in
    { clientMutationId = optionals.clientMutationId, trip_id = required.trip_id, title = required.title, uuid = optionals.uuid, is_day = optionals.is_day, day_date = optionals.day_date, day_order = optionals.day_order }


type alias SaveSectionInputRequiredFields =
    { trip_id : String
    , title : String
    }


type alias SaveSectionInputOptionalFields =
    { clientMutationId : OptionalArgument String
    , uuid : OptionalArgument String
    , is_day : OptionalArgument Bool
    , day_date : OptionalArgument Int
    , day_order : OptionalArgument Int
    }


{-| Type for the SaveSectionInput input object.
-}
type alias SaveSectionInput =
    { clientMutationId : OptionalArgument String
    , trip_id : String
    , title : String
    , uuid : OptionalArgument String
    , is_day : OptionalArgument Bool
    , day_date : OptionalArgument Int
    , day_order : OptionalArgument Int
    }


{-| Encode a SaveSectionInput into a value that can be used as an argument.
-}
encodeSaveSectionInput : SaveSectionInput -> Value
encodeSaveSectionInput input =
    Encode.maybeObject
        [ ( "clientMutationId", Encode.string |> Encode.optional input.clientMutationId ), ( "trip_id", Encode.string input.trip_id |> Just ), ( "title", Encode.string input.title |> Just ), ( "uuid", Encode.string |> Encode.optional input.uuid ), ( "is_day", Encode.bool |> Encode.optional input.is_day ), ( "day_date", Encode.int |> Encode.optional input.day_date ), ( "day_order", Encode.int |> Encode.optional input.day_order ) ]


buildSaveTripInput : (SaveTripInputOptionalFields -> SaveTripInputOptionalFields) -> SaveTripInput
buildSaveTripInput fillOptionals =
    let
        optionals =
            fillOptionals
                { clientMutationId = Absent, uuid = Absent, status = Absent, name = Absent, price = Absent, start_at = Absent, description = Absent, download_pdf = Absent, messaging = Absent, overview_map = Absent }
    in
    { clientMutationId = optionals.clientMutationId, uuid = optionals.uuid, status = optionals.status, name = optionals.name, price = optionals.price, start_at = optionals.start_at, description = optionals.description, download_pdf = optionals.download_pdf, messaging = optionals.messaging, overview_map = optionals.overview_map }


type alias SaveTripInputOptionalFields =
    { clientMutationId : OptionalArgument String
    , uuid : OptionalArgument String
    , status : OptionalArgument Int
    , name : OptionalArgument String
    , price : OptionalArgument String
    , start_at : OptionalArgument String
    , description : OptionalArgument String
    , download_pdf : OptionalArgument Bool
    , messaging : OptionalArgument Bool
    , overview_map : OptionalArgument Bool
    }


{-| Type for the SaveTripInput input object.
-}
type alias SaveTripInput =
    { clientMutationId : OptionalArgument String
    , uuid : OptionalArgument String
    , status : OptionalArgument Int
    , name : OptionalArgument String
    , price : OptionalArgument String
    , start_at : OptionalArgument String
    , description : OptionalArgument String
    , download_pdf : OptionalArgument Bool
    , messaging : OptionalArgument Bool
    , overview_map : OptionalArgument Bool
    }


{-| Encode a SaveTripInput into a value that can be used as an argument.
-}
encodeSaveTripInput : SaveTripInput -> Value
encodeSaveTripInput input =
    Encode.maybeObject
        [ ( "clientMutationId", Encode.string |> Encode.optional input.clientMutationId ), ( "uuid", Encode.string |> Encode.optional input.uuid ), ( "status", Encode.int |> Encode.optional input.status ), ( "name", Encode.string |> Encode.optional input.name ), ( "price", Encode.string |> Encode.optional input.price ), ( "start_at", Encode.string |> Encode.optional input.start_at ), ( "description", Encode.string |> Encode.optional input.description ), ( "download_pdf", Encode.bool |> Encode.optional input.download_pdf ), ( "messaging", Encode.bool |> Encode.optional input.messaging ), ( "overview_map", Encode.bool |> Encode.optional input.overview_map ) ]
