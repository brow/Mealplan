module View exposing (button)

import Extra.Maybe as Maybe
import Html as H
import Html.Attributes as H
import Html.Events as H


button : String -> msg -> Bool -> H.Html msg
button title onClick isPrimary =
    let
        attributes =
            Maybe.values
                [ Just <|
                    H.type_ "button"
                , Just <|
                    H.onClick onClick
                , Maybe.when isPrimary <|
                    H.class "button primary"
                ]
    in
    H.button attributes [ H.text title ]
