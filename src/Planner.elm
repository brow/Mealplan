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


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.h2 [] [ H.text "Recipes" ]
        , model.recipes
            |> List.sortBy (\recipe -> recipe.title)
            |> List.map (\recipe -> H.li [] [ H.text recipe.title ])
            |> H.ul []
        , H.button
            [ H.type_ "button"
            , H.onClick Import
            ]
            [ H.text "Import" ]
        , H.h2 [] [ H.text "Ingredients" ]
        , collectIngredients model
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map
                (\( ingredient, quantities ) ->
                    H.li []
                        [ H.text ingredient
                        , quantities
                            |> List.sortBy (\q -> q.recipeTitle)
                            |> List.map
                                (\q ->
                                    H.li [] [ H.text q.recipeTitle ]
                                )
                            |> H.ul []
                        ]
                )
            |> H.ul []
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


collectIngredients : Model -> Dict String (List QuanitityForRecipe)
collectIngredients model =
    model.recipes
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
