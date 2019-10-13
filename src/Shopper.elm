module Shopper exposing (main)

import Browser
import File exposing (File)
import File.Select
import Html as H
import Html.Attributes as H
import Html.Events as H
import Serialize
import ShoppingList exposing (ShoppingList)
import Task


type alias Model =
    { shoppingList : ShoppingList }


init : ( Model, Cmd Msg )
init =
    ( { shoppingList = { items = [] } }
    , Cmd.none
    )


type Msg
    = Import
    | SelectedFile File
    | LoadedFileContent String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Import ->
            ( model
            , File.Select.file [] SelectedFile
            )

        SelectedFile file ->
            ( model
            , Task.perform LoadedFileContent (File.toString file)
            )

        LoadedFileContent content ->
            ( case Serialize.shoppingListFromString content of
                Ok shoppingList ->
                    { model | shoppingList = shoppingList }

                Err error ->
                    Debug.log error model
            , Cmd.none
            )


view : Model -> H.Html Msg
view model =
    H.div [ H.class "container" ]
        [ H.h2 [] [ H.text "Shopping list" ]
        , model.shoppingList.items
            |> List.sortBy .name
            |> List.map
                (\item ->
                    H.li
                        []
                        [ H.b [] [ H.text item.name ]
                        , H.text ", "
                        , H.text item.quantity
                        ]
                )
            |> H.ul []
        , H.button
            [ H.type_ "button"
            , H.onClick Import
            ]
            [ H.text "Import Items" ]
        , H.text " "
        , H.button
            [ H.type_ "button"
            , H.onClick Import
            ]
            [ H.text "Import Sources" ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
