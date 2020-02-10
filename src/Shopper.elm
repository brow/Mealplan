module Shopper exposing (Model, Msg, init, update, view)

import Console
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Serialize
import ShoppingList exposing (ShoppingList)
import Task


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
                            ( model, Console.error error )

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
                            ( model, Console.error error )


view : Model -> H.Html Msg
view model =
    H.div [ H.class "container" ]
        [ H.h2 [] [ H.text "Shopping list" ]
        , H.button
            [ H.type_ "button"
            , H.onClick (Import Items)
            ]
            [ H.text "Import Items" ]
        , H.text " "
        , H.button
            [ H.type_ "button"
            , H.onClick (Import Sources)
            ]
            [ H.text "Import Sources" ]
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
                                    H.a [ H.href url ] [ text ]

                                Nothing ->
                                    text
                            ]
                        , H.text ", "
                        , H.text item.quantity
                        ]
                )
            |> H.ul []
        ]
