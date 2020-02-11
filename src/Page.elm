module Page exposing (Page(..), path, title)


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


title : Page -> String
title page =
    case page of
        Plan ->
            "Plan"

        Shop ->
            "Shop"
