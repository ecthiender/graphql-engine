-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module Hasura.Enum.Todos_select_column exposing (Todos_select_column(..), decoder, fromString, list, toString)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "todos"

  - Created\_at - column name
  - Id - column name
  - Is\_completed - column name
  - Is\_public - column name
  - Title - column name
  - User\_id - column name

-}
type Todos_select_column
    = Created_at
    | Id
    | Is_completed
    | Is_public
    | Title
    | User_id


list : List Todos_select_column
list =
    [ Created_at, Id, Is_completed, Is_public, Title, User_id ]


decoder : Decoder Todos_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "created_at" ->
                        Decode.succeed Created_at

                    "id" ->
                        Decode.succeed Id

                    "is_completed" ->
                        Decode.succeed Is_completed

                    "is_public" ->
                        Decode.succeed Is_public

                    "title" ->
                        Decode.succeed Title

                    "user_id" ->
                        Decode.succeed User_id

                    _ ->
                        Decode.fail ("Invalid Todos_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representating the Enum to a string that the GraphQL server will recognize.
-}
toString : Todos_select_column -> String
toString enum =
    case enum of
        Created_at ->
            "created_at"

        Id ->
            "id"

        Is_completed ->
            "is_completed"

        Is_public ->
            "is_public"

        Title ->
            "title"

        User_id ->
            "user_id"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Todos_select_column
fromString enumString =
    case enumString of
        "created_at" ->
            Just Created_at

        "id" ->
            Just Id

        "is_completed" ->
            Just Is_completed

        "is_public" ->
            Just Is_public

        "title" ->
            Just Title

        "user_id" ->
            Just User_id

        _ ->
            Nothing
