module Viewer exposing (main)

import Browser
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Recipe exposing (Recipe)
import Serialize
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


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }


view : Model -> H.Html Msg
view model =
    H.div [ H.class "container" ] <|
        if List.isEmpty model.recipes then
            [ H.button
                [ H.type_ "button"
                , H.onClick Import
                ]
                [ H.text "Import" ]
            ]

        else
            List.map viewRecipe model.recipes


viewRecipe : Recipe -> H.Html Msg
viewRecipe recipe =
    H.h2 [] [ H.text recipe.title ]
