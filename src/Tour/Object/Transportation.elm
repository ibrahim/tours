-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.Object.Transportation exposing (currency, day, duration, notes, price, snippets, starts_at, title, type_, uuid)

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


type_ : SelectionSet (Maybe String) Tour.Object.Transportation
type_ =
    Object.selectionForField "(Maybe String)" "_type" [] (Decode.string |> Decode.nullable)


currency : SelectionSet (Maybe String) Tour.Object.Transportation
currency =
    Object.selectionForField "(Maybe String)" "currency" [] (Decode.string |> Decode.nullable)


day : SelectionSet (Maybe Int) Tour.Object.Transportation
day =
    Object.selectionForField "(Maybe Int)" "day" [] (Decode.int |> Decode.nullable)


duration : SelectionSet (Maybe Int) Tour.Object.Transportation
duration =
    Object.selectionForField "(Maybe Int)" "duration" [] (Decode.int |> Decode.nullable)


notes : SelectionSet (Maybe String) Tour.Object.Transportation
notes =
    Object.selectionForField "(Maybe String)" "notes" [] (Decode.string |> Decode.nullable)


price : SelectionSet (Maybe Int) Tour.Object.Transportation
price =
    Object.selectionForField "(Maybe Int)" "price" [] (Decode.int |> Decode.nullable)


snippets : SelectionSet decodesTo Tour.Union.Snippet -> SelectionSet (Maybe (List (Maybe decodesTo))) Tour.Object.Transportation
snippets object_ =
    Object.selectionForCompositeField "snippets" [] object_ (identity >> Decode.nullable >> Decode.list >> Decode.nullable)


starts_at : SelectionSet (Maybe String) Tour.Object.Transportation
starts_at =
    Object.selectionForField "(Maybe String)" "starts_at" [] (Decode.string |> Decode.nullable)


title : SelectionSet String Tour.Object.Transportation
title =
    Object.selectionForField "String" "title" [] Decode.string


uuid : SelectionSet String Tour.Object.Transportation
uuid =
    Object.selectionForField "String" "uuid" [] Decode.string
