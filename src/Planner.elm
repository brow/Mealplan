module Planner exposing (main)

import Browser
import File exposing (File)
import File.Select
import Html as H
import Recipe exposing (Recipe)
import Serialize
import Task


type alias Model =
    { recipes : List Recipe
    }


init : ( Model, Cmd Msg )
init =
    ( { recipes = [] }
    , File.Select.files [] (\x xs -> SelectedFiles (x :: xs))
    )


type Msg
    = SelectedFiles (List File)
    | LoadedFileContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    H.text "Hello, world!"


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
