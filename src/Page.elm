module Page exposing (Page(..), path)


type Page
    = Plan
    | Shop


path : Page -> String
path page =
    case page of
        Plan ->
            "plan"

        Shop ->
            "shop"
