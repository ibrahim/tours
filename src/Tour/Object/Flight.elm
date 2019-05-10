-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.Object.Flight exposing (airline, booked_through, confirmation, currency, day, duration, flight_number, gate, notes, price, section_id, snippets, starts_at, terminal, title, type_, uuid)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import Tour.InputObject
import Tour.Interface
import Tour.Object
import Tour.Scalar
import Tour.ScalarCodecs
import Tour.Union


type_ : SelectionSet String Tour.Object.Flight
type_ =
    Object.selectionForField "String" "_type" [] Decode.string


airline : SelectionSet (Maybe String) Tour.Object.Flight
airline =
    Object.selectionForField "(Maybe String)" "airline" [] (Decode.string |> Decode.nullable)


booked_through : SelectionSet (Maybe String) Tour.Object.Flight
booked_through =
    Object.selectionForField "(Maybe String)" "booked_through" [] (Decode.string |> Decode.nullable)


confirmation : SelectionSet (Maybe String) Tour.Object.Flight
confirmation =
    Object.selectionForField "(Maybe String)" "confirmation" [] (Decode.string |> Decode.nullable)


currency : SelectionSet (Maybe String) Tour.Object.Flight
currency =
    Object.selectionForField "(Maybe String)" "currency" [] (Decode.string |> Decode.nullable)


day : SelectionSet (Maybe Int) Tour.Object.Flight
day =
    Object.selectionForField "(Maybe Int)" "day" [] (Decode.int |> Decode.nullable)


duration : SelectionSet (Maybe Int) Tour.Object.Flight
duration =
    Object.selectionForField "(Maybe Int)" "duration" [] (Decode.int |> Decode.nullable)


flight_number : SelectionSet (Maybe String) Tour.Object.Flight
flight_number =
    Object.selectionForField "(Maybe String)" "flight_number" [] (Decode.string |> Decode.nullable)


gate : SelectionSet (Maybe String) Tour.Object.Flight
gate =
    Object.selectionForField "(Maybe String)" "gate" [] (Decode.string |> Decode.nullable)


notes : SelectionSet (Maybe String) Tour.Object.Flight
notes =
    Object.selectionForField "(Maybe String)" "notes" [] (Decode.string |> Decode.nullable)


price : SelectionSet (Maybe Int) Tour.Object.Flight
price =
    Object.selectionForField "(Maybe Int)" "price" [] (Decode.int |> Decode.nullable)


section_id : SelectionSet String Tour.Object.Flight
section_id =
    Object.selectionForField "String" "section_id" [] Decode.string


snippets : SelectionSet decodesTo Tour.Union.Snippet -> SelectionSet (Maybe (List (Maybe decodesTo))) Tour.Object.Flight
snippets object_ =
    Object.selectionForCompositeField "snippets" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


starts_at : SelectionSet (Maybe String) Tour.Object.Flight
starts_at =
    Object.selectionForField "(Maybe String)" "starts_at" [] (Decode.string |> Decode.nullable)


terminal : SelectionSet (Maybe String) Tour.Object.Flight
terminal =
    Object.selectionForField "(Maybe String)" "terminal" [] (Decode.string |> Decode.nullable)


title : SelectionSet (Maybe String) Tour.Object.Flight
title =
    Object.selectionForField "(Maybe String)" "title" [] (Decode.string |> Decode.nullable)


uuid : SelectionSet String Tour.Object.Flight
uuid =
    Object.selectionForField "String" "uuid" [] Decode.string
