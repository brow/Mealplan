module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as Html
import Shopper
import Url
import Url.Parser as Parser exposing ((</>), Parser)


type Hole
    = Hole



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- MODEL


type alias Model =
    { key : Navigation.Key
    , page : Page
    }


type Page
    = Home
    | Plan
    | Shop
    | NotFound



-- VIEW


view : Model -> Browser.Document msg
view model =
    { title =
        case model.page of
            Plan ->
                "Plan"

            Shop ->
                "Shop"

            Home ->
                "Home"

            NotFound ->
                "Not Found"
    , body =
        [ Html.a [ Html.href "/" ] [ Html.text "Home" ]
        , Html.text "|"
        , Html.a [ Html.href "shop" ] [ Html.text "Shop" ]
        , Html.text "|"
        , Html.a [ Html.href "plan" ] [ Html.text "Plan" ]
        ]
    }



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ url key =
    stepUrl url { key = key, page = NotFound }



-- UPDATE


type Msg
    = Noop
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Navigation.pushUrl model.key (Url.toString url)
                    )

                Browser.External href ->
                    ( model
                    , Navigation.load href
                    )

        UrlChanged url ->
            stepUrl url model


stepUrl : Url.Url -> Model -> ( Model, Cmd msg )
stepUrl url model =
    let
        parser =
            Parser.oneOf
                [ route Parser.top
                    ( { model | page = Home }, Cmd.none )
                , route (Parser.s "plan")
                    ( { model | page = Plan }, Cmd.none )
                , route (Parser.s "shop")
                    ( { model | page = Shop }, Cmd.none )
                ]
    in
    case Parser.parse parser url of
        Just answer ->
            answer

        Nothing ->
            ( { model | page = NotFound }
            , Cmd.none
            )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
