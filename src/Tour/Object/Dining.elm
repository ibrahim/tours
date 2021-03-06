-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.Object.Dining exposing (booked_through, confirmation, currency, day, duration, ends_at, notes, price, provider, section_id, snippets, starts_at, title, type_, uuid)

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


type_ : SelectionSet String Tour.Object.Dining
type_ =
    Object.selectionForField "String" "_type" [] Decode.string


booked_through : SelectionSet (Maybe String) Tour.Object.Dining
booked_through =
    Object.selectionForField "(Maybe String)" "booked_through" [] (Decode.string |> Decode.nullable)


confirmation : SelectionSet (Maybe String) Tour.Object.Dining
confirmation =
    Object.selectionForField "(Maybe String)" "confirmation" [] (Decode.string |> Decode.nullable)


currency : SelectionSet (Maybe String) Tour.Object.Dining
currency =
    Object.selectionForField "(Maybe String)" "currency" [] (Decode.string |> Decode.nullable)


day : SelectionSet (Maybe Int) Tour.Object.Dining
day =
    Object.selectionForField "(Maybe Int)" "day" [] (Decode.int |> Decode.nullable)


duration : SelectionSet (Maybe Int) Tour.Object.Dining
duration =
    Object.selectionForField "(Maybe Int)" "duration" [] (Decode.int |> Decode.nullable)


ends_at : SelectionSet (Maybe Float) Tour.Object.Dining
ends_at =
    Object.selectionForField "(Maybe Float)" "ends_at" [] (Decode.float |> Decode.nullable)


notes : SelectionSet (Maybe String) Tour.Object.Dining
notes =
    Object.selectionForField "(Maybe String)" "notes" [] (Decode.string |> Decode.nullable)


price : SelectionSet (Maybe Int) Tour.Object.Dining
price =
    Object.selectionForField "(Maybe Int)" "price" [] (Decode.int |> Decode.nullable)


provider : SelectionSet (Maybe String) Tour.Object.Dining
provider =
    Object.selectionForField "(Maybe String)" "provider" [] (Decode.string |> Decode.nullable)


section_id : SelectionSet String Tour.Object.Dining
section_id =
    Object.selectionForField "String" "section_id" [] Decode.string


snippets : SelectionSet decodesTo Tour.Union.Snippet -> SelectionSet (Maybe (List (Maybe decodesTo))) Tour.Object.Dining
snippets object_ =
    Object.selectionForCompositeField "snippets" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


starts_at : SelectionSet (Maybe Float) Tour.Object.Dining
starts_at =
    Object.selectionForField "(Maybe Float)" "starts_at" [] (Decode.float |> Decode.nullable)


title : SelectionSet (Maybe String) Tour.Object.Dining
title =
    Object.selectionForField "(Maybe String)" "title" [] (Decode.string |> Decode.nullable)


uuid : SelectionSet String Tour.Object.Dining
uuid =
    Object.selectionForField "String" "uuid" [] Decode.string
