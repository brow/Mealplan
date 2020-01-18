module Main exposing (main)

import Browser
import Browser.Navigation
import Shopper
import Url


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
        , onUrlRequest = always Noop
        , onUrlChange = always Noop
        }



-- MODEL


type alias Model =
    { key : Browser.Navigation.Key
    , page : Page
    }


type Page
    = Plan
    | Shop



-- VIEW


view : Model -> Browser.Document msg
view model =
    { title =
        case model.page of
            Plan ->
                "Plan"

            Shop ->
                "Shop"
    , body = []
    }



-- INIT


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd msg )
init _ url key =
    ( { key = key, page = Plan }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd msg )
update message model =
    case message of
        Noop ->
            ( model, Cmd.none )
