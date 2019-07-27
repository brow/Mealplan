module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Recipe exposing (Recipe)
import Result
import Serialize
import Task



---- MODEL ----


type alias Model =
    Result String Recipe


init : ( Model, Cmd Msg )
init =
    ( Serialize.recipeFromString ""
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Input String
    | Import
    | SelectedFiles File (List File)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input string ->
            ( Serialize.recipeFromString string
            , Cmd.none
            )

        Import ->
            ( model
            , File.Select.files [] SelectedFiles
            )

        SelectedFiles first others ->
            ( model
            , Task.perform Input (File.toString first)
            )



---- VIEW ----


viewIngredient : Recipe.Ingredient -> H.Html Msg
viewIngredient ingredient =
    H.li []
        [ (ingredient.quantity ++ " " ++ ingredient.name)
            |> H.text
            |> List.singleton
            |> H.b []
        , ingredient.notes
            |> Maybe.map (\x -> ", " ++ x)
            |> Maybe.withDefault ""
            |> H.text
        ]


viewRecipe : Recipe -> H.Html Msg
viewRecipe recipe =
    H.div []
        [ H.h2 [] [ H.text "Ingredients" ]
        , H.ul [] <|
            List.map
                viewIngredient
                recipe.ingredients
        , H.h2 [] [ H.text "Instructions" ]
        , H.text recipe.instructions
        ]


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.textarea [ H.onInput Input ] []
        , H.div []
            [ H.button
                [ H.type_ "button"
                , H.onClick Import
                ]
                [ H.text "Import" ]
            ]
        , H.div []
            [ case model of
                Err err ->
                    H.text (Debug.toString err)

                Ok recipe ->
                    viewRecipe recipe
            ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
