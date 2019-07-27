module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H
import Html.Events as H
import Parser as P exposing ((|.), (|=), Parser)
import Result



---- MODEL ----


type alias Recipe =
    { ingredients : List Ingredient }


type alias Ingredient =
    { quantity : String
    , name : String
    , notes : String
    }


type alias Model =
    Result (List P.DeadEnd) Recipe


init : ( Model, Cmd Msg )
init =
    ( P.run parser ""
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( case msg of
        Input string ->
            P.run parser string
    , Cmd.none
    )



---- VIEW ----


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div []
            [ H.textarea [ H.onInput Input ] [] ]
        , H.div []
            [ case model of
                Err err ->
                    H.text (Debug.toString err)

                Ok recipe ->
                    H.ul [] <|
                        List.map
                            (\ingredient ->
                                H.li []
                                    [ H.b []
                                        [ H.text ingredient.quantity
                                        , H.text " "
                                        , H.b [] [ H.text ingredient.name ]
                                        ]
                                    , H.text ", "
                                    , H.text ingredient.notes
                                    ]
                            )
                            recipe.ingredients
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


parser : Parser Recipe
parser =
    P.map Recipe <|
        P.loop []
            (\ingredients ->
                P.oneOf
                    [ P.end
                        |> P.map (\_ -> P.Done (List.reverse ingredients))
                    , ingredientParser
                        |> P.map (\i -> P.Loop (i :: ingredients))
                    ]
            )


ingredientParser : Parser Ingredient
ingredientParser =
    P.succeed Ingredient
        |= ingredientPartParser
        |. P.token ", "
        |= ingredientPartParser
        |. P.token ", "
        |= ingredientPartParser


ingredientPartParser : Parser String
ingredientPartParser =
    (P.chompWhile (\c -> c /= ',' && c /= '\n')
        |> P.getChompedString
    )
        |. P.chompWhile (\c -> c == '\n')
