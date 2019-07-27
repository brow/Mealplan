module Main exposing (main)

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
    { recipes : List Recipe
    , errors : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { recipes = []
      , errors = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Input String
    | Import
    | SelectedFiles (List File)
    | LoadedFileContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input string ->
            ( case Serialize.recipeFromString string of
                Ok recipe ->
                    { model | recipes = [ recipe ], errors = [] }

                Err error ->
                    { model | errors = [ error ], recipes = [] }
            , Cmd.none
            )

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
                    { model | errors = error :: model.errors }
            , Cmd.none
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
        [ H.hr [] []
        , H.h1 [] [ H.text recipe.title ]
        , H.h2 [] [ H.text "Ingredients" ]
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
        , H.div [] <|
            List.map viewRecipe model.recipes
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
