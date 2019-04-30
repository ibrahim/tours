-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Tour.Object.SaveEventPayload exposing (clientMutationId, errors, user)

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


{-| A unique identifier for the client performing the mutation.
-}
clientMutationId : SelectionSet (Maybe String) Tour.Object.SaveEventPayload
clientMutationId =
    Object.selectionForField "(Maybe String)" "clientMutationId" [] (Decode.string |> Decode.nullable)


errors : SelectionSet (Maybe String) Tour.Object.SaveEventPayload
errors =
    Object.selectionForField "(Maybe String)" "errors" [] (Decode.string |> Decode.nullable)


user : SelectionSet decodesTo Tour.Object.User -> SelectionSet decodesTo Tour.Object.SaveEventPayload
user object_ =
    Object.selectionForCompositeField "user" [] object_ identity
