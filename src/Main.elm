module Main exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html as H
import Html.Attributes as A
import Page exposing (Page)
import Page.Plan
import Page.Shop
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
    , page : Maybe Page
    , plan : Page.Plan.Model
    , shop : Page.Shop.Model
    }



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        ( title, content ) =
            case model.page of
                Just Page.Plan ->
                    ( Page.title Page.Plan
                    , Page.Plan.view model.plan |> H.map PlanMsg
                    )

                Just Page.Shop ->
                    ( Page.title Page.Shop
                    , Page.Shop.view model.shop |> H.map ShopMsg
                    )

                Nothing ->
                    ( "Not Found"
                    , H.div [] []
                    )
    in
    { title = title
    , body =
        [ H.div
            [ A.class "container" ]
            [ H.nav
                [ A.class "tabs is-full" ]
                (List.map viewTab Page.allPages)
            , case model.page of
                Just Page.Plan ->
                    Page.Plan.view model.plan |> H.map PlanMsg

                Just Page.Shop ->
                    Page.Shop.view model.shop |> H.map ShopMsg

                Nothing ->
                    H.div [] []
            ]
        ]
    }


viewTab : Page -> H.Html msg
viewTab page =
    H.a
        [ A.href (Page.path page) ]
        [ H.text (Page.title page) ]



-- INIT


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd msg )
init _ url key =
    stepUrl url
        { key = key
        , page = Nothing
        , plan = Page.Plan.init
        , shop = Page.Shop.init
        }



-- UPDATE


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | PlanMsg Page.Plan.Msg
    | ShopMsg Page.Shop.Msg


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
                    Page.Shop.update shopMessage model.shop
            in
            ( { model | shop = shopModel }
            , Cmd.map ShopMsg cmd
            )

        PlanMsg planMessage ->
            let
                ( planModel, cmd ) =
                    Page.Plan.update planMessage model.plan
            in
            ( { model | plan = planModel }
            , Cmd.map PlanMsg cmd
            )


stepUrl : Url.Url -> Model -> ( Model, Cmd msg )
stepUrl url model =
    let
        routeFor page =
            route (Parser.s (Page.path page))
                ( { model | page = Just page }, Cmd.none )

        parser =
            Parser.oneOf <|
                List.map routeFor Page.allPages
                    -- The root path redirects to Plan
                    ++ [ route Parser.top
                            ( model
                            , Navigation.replaceUrl
                                model.key
                                (Page.path Page.Plan)
                            )
                       ]
    in
    Parser.parse parser url
        |> Maybe.withDefault ( { model | page = Nothing }, Cmd.none )


route : Parser a b -> a -> Parser (b -> c) c
route parser handler =
    Parser.map handler parser
