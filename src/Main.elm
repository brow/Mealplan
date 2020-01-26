module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (Html)
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
    = Plan
    | Shop Shopper.Model
    | NotFound



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.page of
            Plan ->
                "Plan"

            Shop _ ->
                "Shop"

            NotFound ->
                "Not Found"
    , body =
        [ viewNav
        , case model.page of
            Plan ->
                Html.div [] []

            Shop shopModel ->
                Shopper.view shopModel ShopMsg

            NotFound ->
                Html.div [] []
        ]
    }


viewNav : Html msg
viewNav =
    Html.div []
        [ Html.a [ Html.href "shop" ] [ Html.text "Shop" ]
        , Html.text "|"
        , Html.a [ Html.href "plan" ] [ Html.text "Plan" ]
        ]



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ url key =
    stepUrl url { key = key, page = NotFound }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ShopMsg Shopper.Msg


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
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

        ShopMsg shopMessage ->
            (Shop (Shopper.update shopMessage)
            Debug.todo "handle ShopMsg _"


stepUrl : Url.Url -> Model -> ( Model, Cmd msg )
stepUrl url model =
    let
        parser =
            Parser.oneOf
                [ route (Parser.oneOf [ Parser.top, Parser.s "plan" ])
                    ( { model | page = Plan }, Cmd.none )
                , route (Parser.s "shop")
                    ( { model | page = Shop Shopper.init }, Cmd.none )
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
