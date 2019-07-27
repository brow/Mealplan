module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H
import Html.Events as H
import Recipe exposing (Recipe)
import Result
import Serialize



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Input string ->
            Serialize.recipeFromString string
    , Cmd.none
    )



---- VIEW ----


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.textarea [ H.onInput Input ] []
        , H.h2 [] [ H.text "Ingredients" ]
        , case model of
            Err err ->
                H.text (Debug.toString err)

            Ok recipe ->
                H.ul [] <|
                    List.map
                        (\ingredient ->
                            H.li []
                                ([ H.b []
                                    [ H.text ingredient.quantity
                                    , H.text " "
                                    , H.b [] [ H.text ingredient.name ]
                                    ]
                                 ]
                                    ++ (case ingredient.notes of
                                            Just notes ->
                                                [ H.text ", "
                                                , H.text notes
                                                ]

                                            Nothing ->
                                                []
                                       )
                                )
                        )
                        recipe.ingredients
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
