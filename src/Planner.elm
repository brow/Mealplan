module Planner exposing (main)

import Browser
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Recipe exposing (Recipe)
import Serialize
import Set
import Task


type alias Model =
    { recipes : List Recipe
    }


init : ( Model, Cmd Msg )
init =
    ( { recipes = [] }
    , Cmd.none
    )


type Msg
    = Import
    | SelectedFiles (List File)
    | LoadedFileContent String
    | Export


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Import ->
            ( model
            , File.Select.files [] (\x xs -> SelectedFiles (x :: xs))
            )

        SelectedFiles files ->
            ( model
            , files
                |> List.map
                    (Task.perform LoadedFileContent << File.toString)
                |> Cmd.batch
            )

        LoadedFileContent content ->
            ( case Serialize.recipeFromString content of
                Ok recipe ->
                    { model | recipes = recipe :: model.recipes }

                Err error ->
                    model
            , Cmd.none
            )

        Export ->
            ( model
            , Cmd.none
            )


view : Model -> H.Html Msg
view model =
    H.div [ H.class "container" ]
        [ H.h2 [] [ H.text "Recipes" ]
        , model.recipes
            |> List.sortBy .title
            |> List.map (\recipe -> H.li [] [ H.text recipe.title ])
            |> H.ul []
        , H.button
            [ H.type_ "button"
            , H.onClick Import
            ]
            [ H.text "Import" ]
        , H.h2 [] [ H.text "Ingredients" ]
        , collectIngredients model.recipes
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map viewIngredient
            |> H.ul [ H.class "toplevel" ]
        , H.button
            [ H.type_ "button"
            , H.onClick Export
            ]
            [ H.text "Export List" ]
        ]


viewIngredient : ( String, List QuanitityForRecipe ) -> H.Html Msg
viewIngredient ( ingredient, quantities ) =
    H.li [ H.class "card" ]
        [ H.h3 [] [ H.text ingredient ]
        , quantities
            |> List.sortBy .recipeTitle
            |> List.map
                (\q ->
                    H.tr []
                        [ H.td
                            [ H.class "quantity" ]
                            [ H.text q.quantity ]
                        , H.td [] [ H.text q.recipeTitle ]
                        ]
                )
            |> H.table []
        , H.input [] []
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


type alias QuanitityForRecipe =
    { quantity : String
    , recipeTitle : String
    }


collectIngredients : List Recipe -> Dict String (List QuanitityForRecipe)
collectIngredients recipes =
    recipes
        |> List.concatMap
            (\recipe ->
                List.map
                    (\ingredient ->
                        ( ingredient.name
                        , { recipeTitle = recipe.title
                          , quantity = ingredient.quantity
                          }
                        )
                    )
                    recipe.ingredients
            )
        |> List.foldl
            (\( ingredient, quanitityForRecipe ) dict ->
                let
                    quantities =
                        Dict.get ingredient dict
                            |> Maybe.withDefault []
                in
                Dict.insert
                    ingredient
                    (quanitityForRecipe :: quantities)
                    dict
            )
            Dict.empty
