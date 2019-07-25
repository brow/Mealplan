module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html as H
import Html.Events as H
import Parser exposing ((|.), (|=), Parser)
import Result



---- MODEL ----


type alias Recipe =
    { ingredients : List String }


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
    ( case msg of
        Input string ->
            Parser.run parser string
    , Cmd.none
    )



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
    Parser.map Recipe <|
        Parser.loop []
            (\ingredients ->
                Parser.oneOf
                    [ Parser.end
                        |> Parser.map (\_ -> Parser.Done ingredients)
                    , ingredientParser
                        |> Parser.map (\i -> Parser.Loop (i :: ingredients))
                    ]
            )



--         Parser.sequence
--             { start = ""
--             , separator = "\n"
--             , end = ""
--             , spaces = Parser.chompWhile (\c -> c == ' ')
--             , item = ingredientParser
--             , trailing = Parser.Optional
--             }
--


ingredientParser : Parser String
ingredientParser =
    Parser.succeed identity
        |= Parser.getChompedString (Parser.chompWhile (\c -> c /= '\n'))
        |. Parser.chompWhile (\c -> c == '\n')



-- |. Parser.chompIf (\c -> c == '\n')
-- |> Parser.andThen
--     (\s ->
--         if String.isEmpty s then
--             Parser.problem "hi"
--
--         else
--             Parser.succeed s
--     )
-- |> Parser.andThen
--     (\s ->
--         if String.isEmpty s then
--             Parser.problem "hi"
--
--         else
--             Parser.succeed s
--     )
