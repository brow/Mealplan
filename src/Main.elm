module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H
import Html.Events as H
import Parser exposing (Parser)
import Result



---- MODEL ----


type alias Recipe =
    {}


type alias Model =
    Result (List Parser.DeadEnd) Recipe


init : ( Model, Cmd Msg )
init =
    ( Parser.run parser ""
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = Input String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div []
            [ H.textarea [ H.onInput Input ] [] ]
        , H.div []
            [ H.text <| Debug.toString model ]
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
    Parser.succeed {}
