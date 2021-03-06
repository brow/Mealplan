module Page.Plan exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import File exposing (File)
import File.Download
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Port
import Recipe exposing (Recipe)
import Serialize
import Set
import Task
import View


type alias Model =
    { recipes : List Recipe
    , enteredQuantities : Dict String String
    }


init : Model
init =
    { recipes = []
    , enteredQuantities = Dict.empty
    }


type Msg
    = Import
    | SelectedFiles (List File)
    | LoadedFile String String
    | ChangeQuantity String String
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
                    (\file ->
                        Task.perform
                            (LoadedFile (File.name file))
                            (File.toString file)
                    )
                |> Cmd.batch
            )

        LoadedFile name content ->
            case Serialize.recipeFromString content of
                Ok recipe ->
                    ( { model | recipes = recipe :: model.recipes }, Cmd.none )

                Err error ->
                    ( model, Port.error (name ++ ": " ++ error) )

        ChangeQuantity ingredientName newQuantity ->
            ( { model
                | enteredQuantities =
                    Dict.insert
                        ingredientName
                        newQuantity
                        model.enteredQuantities
              }
            , Cmd.none
            )

        Export ->
            let
                text =
                    Dict.toList model.enteredQuantities
                        |> List.filter (Tuple.second >> String.isEmpty >> not)
                        |> List.sortBy Tuple.first
                        |> List.map
                            (\( ingredient, quantity ) ->
                                String.join ", " [ ingredient, quantity ]
                            )
                        |> String.join "\n"
            in
            ( model
            , File.Download.string
                "groceries.txt"
                "text/plain"
                text
            )


view : Model -> H.Html Msg
view model =
    let
        quantitiesForIngredient =
            collectIngredients model.recipes
    in
    H.div [] <|
        List.filterMap identity <|
            [ Just (viewRecipes model.recipes)
            , if Dict.isEmpty quantitiesForIngredient then
                Nothing

              else
                Just <|
                    viewIngredients
                        quantitiesForIngredient
                        model.enteredQuantities
            ]


viewRecipes : List Recipe -> H.Html Msg
viewRecipes recipes =
    let
        isEmpty =
            List.isEmpty recipes
    in
    H.section []
        [ H.h2 [] [ H.text "Recipes" ]
        , if isEmpty then
            H.div [] [ H.text "Add the recipes for this week." ]

          else
            recipes
                |> List.sortBy .title
                |> List.map (\recipe -> H.li [] [ H.text recipe.title ])
                |> H.ul [ H.class "recipes" ]
        , View.button
            "Add Recipes"
            Import
            isEmpty
        ]


viewIngredients : Dict String (List QuanitityForRecipe) -> Dict String String -> H.Html Msg
viewIngredients quantitiesForIngredient enteredQuantities =
    H.section []
        [ H.h2 [] [ H.text "Ingredients" ]
        , H.div [] [ H.text "Enter the amount we need to buy. Leave blank if none." ]
        , quantitiesForIngredient
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map
                (\( name, quantities ) ->
                    ( name
                    , quantities
                    , Dict.get name enteredQuantities |> Maybe.withDefault ""
                    )
                )
            |> List.map viewIngredient
            |> H.ul [ H.class "toplevel" ]
        , View.button
            "Export List"
            Export
            True
        ]


viewIngredient : ( String, List QuanitityForRecipe, String ) -> H.Html Msg
viewIngredient ( ingredient, quantities, enteredQuantity ) =
    H.li []
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
        , H.input
            [ H.value enteredQuantity
            , H.class "quantity"
            , H.onInput (ChangeQuantity ingredient)
            ]
            []
        ]


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
