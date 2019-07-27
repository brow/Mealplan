module Recipe exposing (Ingredient, Recipe)


type alias Recipe =
    { title : String
    , ingredients : List Ingredient
    , instructions : String
    }


type alias Ingredient =
    { quantity : String
    , name : String
    , notes : Maybe String
    }
