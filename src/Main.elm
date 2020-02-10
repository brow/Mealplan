module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html as H
import Html.Attributes as A
import Planner
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
    , plan : Planner.Model
    , shop : Shopper.Model
    }


type Page
    = Plan
    | Shop
    | NotFound



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title =
        case model.page of
            Plan ->
                "Plan"

            Shop ->
                "Shop"

            NotFound ->
                "Not Found"
    , body =
        [ viewNav
        , case model.page of
            Plan ->
                Planner.view model.plan |> H.map PlanMsg

            Shop ->
                Shopper.view model.shop |> H.map ShopMsg

            NotFound ->
                H.div [] []
        ]
    }


viewNav : H.Html msg
viewNav =
    H.div []
        [ H.a
            [ A.href "plan" ]
            [ H.text "Plan" ]
        , H.text " | "
        , H.a
            [ A.href "shop" ]
            [ H.text "Shop" ]
        ]



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ url key =
    stepUrl url
        { key = key
        , page = NotFound
        , plan = Planner.init
        , shop = Shopper.init
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PlanMsg Planner.Msg
    | ShopMsg Shopper.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
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
            let
                ( shopModel, cmd ) =
                    Shopper.update shopMessage model.shop
            in
            ( { model | shop = shopModel }
            , Cmd.map ShopMsg cmd
            )

        PlanMsg planMessage ->
            let
                ( planModel, cmd ) =
                    Planner.update planMessage model.plan
            in
            ( { model | plan = planModel }
            , Cmd.map PlanMsg cmd
            )


stepUrl : Url.Url -> Model -> ( Model, Cmd msg )
stepUrl url model =
    let
        parser =
            Parser.oneOf
                [ route (Parser.oneOf [ Parser.top, Parser.s "plan" ])
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
