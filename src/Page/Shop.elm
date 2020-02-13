module Page.Shop exposing (Model, Msg, init, update, view)

import Dict exposing (Dict)
import Extra.Maybe as Maybe
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Page
import Port
import Serialize
import ShoppingList exposing (ShoppingList)
import Task
import View


type alias Model =
    { shoppingList : ShoppingList
    , sources : Dict String String
    }


init : Model
init =
    { shoppingList = { items = [] }
    , sources = Dict.empty
    }


type InputType
    = Items
    | Sources


type Msg
    = Import InputType
    | SelectedFile InputType File
    | LoadedFileContent InputType String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Import inputType ->
            ( model
            , File.Select.file [] (SelectedFile inputType)
            )

        SelectedFile inputType file ->
            ( model
            , Task.perform
                (LoadedFileContent inputType)
                (File.toString file)
            )

        LoadedFileContent inputType content ->
            case inputType of
                Items ->
                    case Serialize.shoppingListFromString content of
                        Ok shoppingList ->
                            ( { model | shoppingList = shoppingList }
                            , Cmd.none
                            )

                        Err error ->
                            ( model, Port.error error )

                Sources ->
                    case Serialize.shoppingListSourcesFromString content of
                        Ok sources ->
                            ( { model
                                | sources =
                                    sources
                                        |> List.map (\s -> ( s.name, s.url ))
                                        |> Dict.fromList
                              }
                            , Cmd.none
                            )

                        Err error ->
                            ( model, Port.error error )


view : Model -> H.Html Msg
view model =
    let
        itemsAreEmpty =
            List.isEmpty model.shoppingList.items

        sourcesAreEmpty =
            Dict.isEmpty model.sources
    in
    H.div [] <|
        Maybe.values
            [ Just <|
                H.h2 [] [ H.text "Shopping list" ]
            , Maybe.when itemsAreEmpty <|
                H.div []
                    [ H.text "View a list you exported from "
                    , H.a
                        [ H.href (Page.path Page.Plan) ]
                        [ H.text (Page.title Page.Plan) ]
                    , H.text "."
                    ]
            , Just <|
                View.button
                    "Import List"
                    (Import Items)
                    itemsAreEmpty
            , Maybe.when (not itemsAreEmpty) <|
                View.button
                    "Import Links"
                    (Import Sources)
                    sourcesAreEmpty
            , model.shoppingList.items
                |> List.sortBy .name
                |> List.map
                    (\item ->
                        H.li
                            []
                            [ H.b []
                                [ let
                                    text =
                                        H.text item.name
                                  in
                                  case Dict.get item.name model.sources of
                                    Just url ->
                                        H.a
                                            [ H.href url, H.target "_blank" ]
                                            [ text ]

                                    Nothing ->
                                        text
                                ]
                            , H.text ", "
                            , H.text item.quantity
                            ]
                    )
                |> H.ul []
                |> Just
            ]
